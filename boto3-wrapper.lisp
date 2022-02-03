;;;; boto3-wrapper.lisp
;;;; This file contains the functions that are the public interface of the package. Some are
;;;; straight AWS calls, others consume functions from boto3-low-level.lisp

(in-package #:boto3-wrapper)

(defvar *current-profile* nil "The currently assigned profile.")
(defvar *s3-default-directory*
  (uiop:native-namestring "~/boto3-wrapper/")
  "Default directory for S3 operations")
(defvar *s3-default-bucket* nil "Bucket to use when not specified.")

(defun set-profile (profile-name &optional (region "us-east-1"))
  "Set the PROFILE-NAME for boto3. Something valid from the credentials file."
  (b3py:setup_default_session :profile_name profile-name :region_name region)
  ;; Create clients with the new profile/session
  (setf *s3-client* (b3py:client "s3"))
  (setf *ssm-client* (b3py:client "ssm"))
  (setf *lambda-client* (b3py:client "lambda"))
  (setf *cloudformation-client* (b3py:client "cloudformation"))
  (setf *secretsm-client* (b3py:client "secretsmanager"))
  (setf *current-profile* profile-name))

(defun ssm-list-parameters (path &optional (recursive t))
  "Return a list of paramaters under PATH, in the format ( key . value ). RECURSIVE is
self-explanatory."
  (loop for elem across (ssm-get-all-parameters path recursive)
        collect (cons (gethash "Name" elem) (gethash "Value" elem))))

(defun ssm-get-parameter (name)
  "Return the value under NAME, in a readable format"
  (let* ((arguments `(("Name" . ,name) ("WithDecryption" . "True")))
         (response (call-python-method *ssm-client*
                                       "get_parameter"
                                       :kwargs arguments)))
    (gethash "Value" (gethash "Parameter" response))))

(defun ssm-put-parameter (name value &optional (description "") (parameter-type "String"))
  "Upsert the parameter under NAME, with VALUE.
PARAMETER-TYPE can be String, SecureString or StringList."
  (let ((arguments `(("Name" . ,name) ("Description" . ,description)
                     ("Value" . ,value) ("Type" . ,parameter-type)
                     ("Overwrite" . "True"))))
    (call-python-method *ssm-client*
                        "put_parameter"
                        :kwargs arguments)))

(defun secretsm-list (&optional name-filter)
  "List all of secrets, if NAME-FILTER, filter by partial name."
  (let* ((all-data (secretsm-get-all))
         (names-only (loop for ht across all-data
                           collect (gethash "Name" ht))))
    (if name-filter
        (remove-if-not (lambda (name) (search name-filter name :test #'char-equal)) names-only)
        names-only)))

(defun secretsm-put-secret (name value)
  "Upsert the parameter under NAME, with VALUE.
This function does only String secrets, if I ever need binary secrets it
should be easy to add."
  (call-python-method *secretsm-client*
                      "put_secret_value"
                      :kwargs  `(("SecretId" . ,name)
                                 ("SecretString" . ,value))))

(defun secretsm-get-secret (name)
  "Return the value of the last version of secret NAME. "
  (let* ((arguments `(("SecretId" . ,name)))
         (response (call-python-method *secretsm-client*
                                       "get_secret_value"
                                       :kwargs arguments)))
    (gethash "SecretString" response)))

(defun lambda-list-functions (&optional name-filter)
  "List all lambda functions, if NAME-FILTER, filter by partial name."
  (let* ((all-data (lambda-list-all-functions))
         (names-only (loop for ht across all-data
                           collect (gethash "FunctionName" ht))))
    (if name-filter
        (remove-if-not (lambda (name) (search name-filter name :test #'char-equal)) names-only)
        names-only)))

(defun lambda-get-function-configuration (name-or-arn &optional qualifier)
  "Gets only the configuration of the lambda NAME-OR-ARN, filter by optional QUALIFIER."
  (let ((all-data (lambda-get-function-details name-or-arn qualifier)))
    (alexandria:hash-table-alist (gethash "Configuration" all-data))))

(defun lambda-get-function-tags (name-or-arn &optional qualifier)
  "Get the tags of the lambda NAME-OR-ARN, filter by optional QUALIFIER."
  (let ((all-data (lambda-get-function-details name-or-arn qualifier)))
    (alexandria:hash-table-alist (gethash "Tags" all-data))))

(defun lambda-get-function-environment (name-or-arn &optional qualifier)
  "Get the environment config (env vars & errors) of the lambda NAME-OR-ARN.
Supports filter by optional QUALIFIER."
  (let ((environment-data (gethash "Environment"
                                   (gethash "Configuration"
                                            (lambda-get-function-details name-or-arn qualifier)))))
    (loop for the-key being the hash-key in environment-data
            using (hash-value the-value)
          collect (cons the-key
                        (if (hash-table-p the-value)
                            (alexandria:hash-table-alist the-value)
                            the-value)))))

(defun lambda-invoke (name-or-arn &key (payload "") (show-log nil))
  "Invoke NAME-OR-ARN, using PAYLOAD. If SHOW-LOG, it prints the execution log."
  (let* ((arguments `(("FunctionName" . , name-or-arn)
                      ("Payload" . ,payload)
                      ("LogType" . "Tail")))
         (raw-output (call-python-method *lambda-client* "invoke"
                                         :kwargs arguments))
         (payload (json-string-to-alist
                   (py4cl::python-eval (gethash "Payload" raw-output) ".read().decode('utf-8')")))
         (body-json (alexandria:assoc-value payload "body" :test 'equal)))
    ;; the body migth not be present if there was an error executing the lambda
    (when body-json
      (setf (cdr (assoc "body" payload :test 'equal)) (json-string-to-alist body-json)))
    (when show-log
      (format t "Execution log:~%~a~%" (cl-base64:base64-string-to-string
                                        (gethash "LogResult" raw-output))))
    payload))

(defun cloudf-list-stacks (&optional name-filter (include-deleted nil))
  "List all cloudformation stacks, if NAME-FILTER, filter by partial name."
  (let* ((all-data (cloudf-list-all-stacks))
         (names-updated-status (loop for ht across all-data
                                     collect (list :name (gethash "StackName" ht)
                                                   :updated (python-datetime-string
                                                             (gethash "LastUpdatedTime" ht))
                                                   :status (gethash "StackStatus" ht)))))
    (when name-filter
      (setf names-updated-status (remove-if-not (lambda (a-plist)
                                                  (search name-filter
                                                          (getf a-plist :name)
                                                          :test #'char-equal))
                                                names-updated-status)))
    (unless include-deleted
      (setf names-updated-status (remove-if (lambda (a-plist)
                                              (string= "DELETE_COMPLETE"
                                                       (getf a-plist :status)))
                                            names-updated-status)))
    names-updated-status))

(defun cloudf-get-stack-resources (stack-name &key (type nil) (name nil))
  "List the resources of STACK-NAME. For deleted stacks, use the stack id.
Optional keywords TYPE and NAME can be used to filter the output."
  (let ((all-data (convert-aws-output (cloudf-list-stack-resources stack-name)
                                      '(:name :arn :type)
                                      '(("LogicalResourceId" . :name)
                                        ("PhysicalResourceId" . :arn)
                                        ("ResourceType" . :type)))))
    ;; apply type filter
    (when type
      (setf all-data
            (remove-if-not (lambda (item) (search type
                                                  (alexandria:assoc-value item :type)
                                                  :test #'char-equal))
                           all-data)))
    ;; apply name filter
    ;; I could do both in one pass with mapcar, I guess?
    (when name
      (setf all-data
            (remove-if-not (lambda (item) (search name
                                                  (alexandria:assoc-value item :name)
                                                  :test #'char-equal))
                           all-data)))
    ;; return the data, after optional filtering applied
    all-data))

(defun s3-list-buckets (&optional name-filter)
  "Return a list of S3 buckets that can be accessed. When NAME-FILTER is provided, filter items
that contain that string in the name."
  (let ((buckets-lst (loop for bucket across (gethash "Buckets" (call-python-method *s3-client*
                                                                                "list_buckets"))
                       collect (gethash "Name" bucket))))
    (when name-filter
      (setf buckets-lst (remove-if-not (lambda (bucket-name) (search name-filter
                                                                     bucket-name
                                                                     :test #'char-equal))
                                       buckets-lst)))
    buckets-lst))

(defun s3-list-items (&key (bucket *s3-default-bucket*) prefix name-contains)
  "Return a list of S3 items in BUCKET. Can specify a PREFIX (sub-dir). Can filter the output with
NAME-CONTAINS."
  (alexandria:when-let ((all-objects (s3-list-all-items bucket prefix)))
    (when name-contains
      (setf all-objects (remove-if-not (lambda (item) (search name-contains
                                                              (gethash "Key" item)
                                                              :test #'char-equal))
                                       all-objects)))
    ;; Other props returned: ETag StorageClass Owner
    (loop for elem across all-objects
          collect (list (cons :key (gethash "Key" elem))
                        (cons :last-modified (python-datetime-string
                                              (gethash "LastModified" elem)))
                        (cons :size (gethash "Size" elem))))))

(defun s3-list-directories (&key (bucket *s3-default-bucket*) prefix)
  "Return a list of S3 \"directories\" in BUCKET. Can specify a PREFIX (sub-dir)"
  (alexandria:when-let ((directories (s3-list-all-directories bucket prefix)))
    (loop for elem across directories
          collect (gethash "Prefix" elem))))

(defun s3-download (key &key (bucket *s3-default-bucket*) (local-directory *s3-default-directory*)
                          (preserve-path t))
  "Download a file from S3 BUCKET, under KEY, to LOCAL-DIRECTORY. If PRESERVE-PATH,
all the \"sub-directories\" in KEY are mantained, else the file is download to the root of
LOCAL-DIRECTORY."
  ;; all the dir manipulation here is non-portable, which I see as a sin but I need this and don't
  ;; want to spend the time to deal with it now...
  (let* ((local-path (concatenate 'string local-directory
                                  (if preserve-path
                                        key
                                        (car (last (uiop:split-string key :separator "/")))))))
    (when preserve-path
      (ensure-directories-exist local-path))
    (call-python-method *s3-client*
                        "download_file"
                        :kwargs `(("Bucket" . ,bucket)
                                  ("Key" . ,key)
                                  ("Filename" . ,local-path)))
    local-path))

(defun s3-upload (filename &key (bucket *s3-default-bucket*) (path-remove *s3-default-directory*))
  "Upload FILENAME to an S3 BUCKET. PATH-REMOVE is the portion of the local path that should be
removed when uploading the file. If you use *s3-default-directory*, you will never need it."
  (let* ((expanded-filename (uiop:native-namestring filename))
         (expanded-remove (uiop:native-namestring (or path-remove "")))
         (object-key (str:replace-first expanded-remove "" expanded-filename) ))
    (call-python-method *s3-client*
                        "upload_file"
                        :positional (list expanded-filename
                                          bucket
                                          object-key))
    object-key))

(defun s3-delete (key &key (bucket *s3-default-bucket*))
  "Delete KEY from an S3 BUCKET."
  (gethash "DeleteMarker"
           (call-python-method *s3-client*
                               "delete_object"
                               :kwargs `(("Bucket" . ,bucket)
                                         ("Key" . , key)))))

(defun s3-metadata (key &key (bucket *s3-default-bucket*))
  "Retrieve the metadata of KEY from an S3 BUCKET."
  (let ((keys-of-interest '("ContentLength" "ContentType" "Metadata" "VersionId"))
        (response (call-python-method *s3-client*
                                      "head_object"
                                      :kwargs `(("Bucket" . ,bucket)
                                                ("Key" . , key)))))
    (list (cons :last-modified (python-datetime-string (gethash "LastModified" response)))
          (cons :content-type (gethash "ContentType" response))
          (cons :content-length (gethash "ContentLength" response))
          (cons :metadata (gethash "Metadata" response))
          (cons :version-id (gethash "VersionId" response)))))
