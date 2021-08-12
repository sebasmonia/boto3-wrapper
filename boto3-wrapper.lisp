;;;; boto3-wrapper.lisp

(in-package #:boto3-wrapper)

;; Add more clients here. Don't forget to initialize them in `set-profile'.
(defparameter *ssm-client* nil "An SSM client with the current credentials.")
(defparameter *secretsm-client* nil "A Secrets Manager client with the current credentials.")
(defparameter *lambda-client* nil "A Lambda function client with the current credentials.")
(defparameter *cloudformation-client* nil "A Cloud Formation client with the current credentials.")
(defparameter *debug-python-calls* nil "Print python method calls details.")
(defvar *current-profile* nil "A symbol to read the currently assigned profile.")

(defun set-profile (profile-name &optional (region "us-east-1"))
  "Set the PROFILE-NAME for boto3. Something valid from the credentials file."
  (b3py:setup_default_session :profile_name profile-name :region_name region)
  ;; Create clients with the new profile/session
  (setf *ssm-client* (b3py:client "ssm"))
  (setf *lambda-client* (b3py:client "lambda"))
  (setf *cloudformation-client* (b3py:client "cloudformation"))
  (setf *secretsm-client* (b3py:client "secretsmanager"))
  (setf *current-profile* profile-name))

(defun ssm-list-parameters (path &optional (recursive t))
  "Return a list of paramaters under PATH, in the format ( key . value ). RECURSIVE is self-explanatory."
  (loop for elem across (ssm-get-parameters path recursive)
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

(defun ssm-get-parameters (path recursive &optional next-token)
  "Returns the parameters under PATH. RECURSIVE is self-explanatory.
When NEXT-TOKEN is provided, it means it is a follow up to a paged call.
This function is internal, use `ssm-list-parameters' instead."
  (let* ((arguments `(("Path" . ,path)
                      ("Recursive" . ,(if recursive "True" "False"))
                      ("NextToken" . ,next-token)))
         ;; if next-token is nil it will be removed from the kwargs
         ;; list by `format-kwargs'
         (response (call-python-method *ssm-client*
                                       "get_parameters_by_path"
                                       :kwargs arguments))
         (parameters (gethash "Parameters" response))
         (next-token (gethash "NextToken" response)))
    (if next-token
        (concatenate 'vector
                     (ssm-get-parameters path recursive next-token)
                     parameters)
        parameters)))

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

(defun secretsm-get-all (&optional next-token)
  "List all secrets. When NEXT-TOKEN is provided, it means it is a follow up to a paged call.
This function is internal, use `secretsm-list' instead."
  (let* ((arguments `(("NextToken" . ,next-token)))
         (response (call-python-method *secretsm-client*
                                       "list_secrets"
                                       :kwargs arguments))
         (secret-list (gethash "SecretList" response))
         (next-token (gethash "NextToken" response)))
    (if next-token
        (concatenate 'vector
                     (secretsm-get-all next-token)
                     secret-list)
        secret-list)))

(defun lambda-list-all-functions (&optional marker)
  "List all lambda functions published in the environment.
When MARKER is provided, it means it is a follow up to a paged call.
This function is internal, use `lambda-list-functions' instead."
  (let* ((arguments `(("Marker" . ,marker)))
         ;; if marker is nil it will be removed from the kwargs
         ;; list by `format-kwargs'
         (response (call-python-method *lambda-client*
                                       "list_functions"
                                       :kwargs arguments))
         (functions (gethash "Functions" response))
         (next-marker (gethash "NextMarker" response)))
    (if next-marker
        (concatenate 'vector
                     (lambda-list-all-functions next-marker)
                     functions)
        functions)))

(defun lambda-list-functions (&optional name-filter)
  "List all lambda functions, if NAME-FILTER, filter by partial name."
  (let* ((all-data (lambda-list-all-functions))
         (names-only (loop for ht across all-data
                           collect (gethash "FunctionName" ht))))
    (if name-filter
        (remove-if-not (lambda (name) (search name-filter name :test #'char-equal)) names-only)
        names-only)))

(defun lambda-get-function-details (name-or-arn &optional qualifier)
  "Get details of the lambda NAME-OR-ARN, filter by optional QUALIFIER.
This is called by all the lambda-get-* functions, it is inneficient but :shrug:"
  (let* ((arguments `(("FunctionName" . ,name-or-arn)
                      ("Qualifier" . ,Qualifier))))
    (call-python-method *lambda-client*
                        "get_function"
                        :kwargs arguments)))

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
      (format t "Execution log:~%~a~%" (cl-base64:base64-string-to-string (gethash "LogResult" raw-output))))
    payload))

(defun cloudf-list-all-stacks (&optional next-token)
  "List all cloudformation stacks in the environment.
When NEXT-TOKEN is provided, it means it is a follow up to a paged call.
This function is internal, use `cloudf-list-stacks' instead."
  (let* ((arguments `(("NextToken" . ,next-token)))
         (response (call-python-method *cloudformation-client*
                                       "list_stacks"
                                       :kwargs arguments))
         (stacks (gethash "StackSummaries" response))
         (next-marker (gethash "NextMarker" response)))
    (if next-marker
        (concatenate 'vector
                     (cloudf-list-all-stacks next-marker)
                     stacks)
        stacks)))

(defun cloudf-list-stacks (&optional name-filter (include-deleted nil))
  "List all cloudformation stacks, if NAME-FILTER, filter by partial name."
  (let* ((all-data (cloudf-list-all-stacks))
         (names-updated-status (loop for ht across all-data
                                     collect (list :name (gethash "StackName" ht)
                                                   :updated (python-datetime-string (gethash "LastUpdatedTime" ht))
                                                   :status (gethash "StackStatus" ht)))))
    (when name-filter
      (setf names-updated-status (remove-if-not (lambda (a-plist) (search name-filter (getf a-plist :name) :test #'char-equal)) names-updated-status)))
    (unless include-deleted
      (setf names-updated-status (remove-if (lambda (a-plist) (string= "DELETE_COMPLETE" (getf a-plist :status))) names-updated-status)))
    names-updated-status))

(defun cloudf-list-stack-resources (stack-name &optional next-token)
  "List all resources for STACK-NAME.
When NEXT-TOKEN is provided, it means it is a follow up to a paged call.
This function is internal, use `cloudf-get-stack-resources' instead."
  (let* ((arguments `(("StackName" . ,stack-name)
                      ("NextToken" . ,next-token)))
         (response (call-python-method *cloudformation-client*
                                       "list_stack_resources"
                                       :kwargs arguments))
         (resources (gethash "StackResourceSummaries" response))
         (next-marker (gethash "NextMarker" response)))
    (if next-marker
        (concatenate 'vector
                     (cloudf-list-stack-resources stack-name next-marker)
                     resources)
        resources)))

(defun cloudf-get-stack-resources (stack-name &key (type nil) (name nil))
  "List the resources of STACK-NAME. For deleted stacks, use the stack id.
Optional keywords TYPE and NAME can be used to filter the output."
  (let ((all-data (convert-aws-output (cloudf-list-stack-resources stack-name)
                                      '(:name :arn :type)
                                      '(("LogicalResourceId" . :name) ("PhysicalResourceId" . :arn) ("ResourceType" . :type)))))
    ;; apply type filter
    (when type
      (setf all-data
            (remove-if-not (lambda (item) (search type (alexandria:assoc-value item :type) :test #'char-equal)) all-data)))
    ;; apply name filter
    ;; I could do both in one pass with mapcar, I guess?
    (when name
      (setf all-data
            (remove-if-not (lambda (item) (search name (alexandria:assoc-value item :name) :test #'char-equal)) all-data)))
    ;; return the data, after optional filtering applied
    all-data))
