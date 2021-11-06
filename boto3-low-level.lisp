;;;; boto3-wrapper.lisp
;;;; This file contains lower level functions, for example ones that paginate using several calls
;;;; to AWS and then compile all the results in a single function

(in-package #:boto3-wrapper)

;; Add more clients here. Don't forget to initialize them in `set-profile'.
(defparameter *ssm-client* nil "An SSM client with the current credentials.")
(defparameter *secretsm-client* nil "A Secrets Manager client with the current credentials.")
(defparameter *lambda-client* nil "A Lambda function client with the current credentials.")
(defparameter *cloudformation-client* nil "A Cloud Formation client with the current credentials.")
(defparameter *s3-client* nil "An S3 client with the current credentials.")
(defparameter *debug-python-calls* nil "Print python method calls details.")

(defun ssm-get-all-parameters (path recursive &optional next-token)
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
                     (ssm-get-all-parameters path recursive next-token)
                     parameters)
        parameters)))

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

(defun lambda-get-function-details (name-or-arn &optional qualifier)
  "Get details of the lambda NAME-OR-ARN, filter by optional QUALIFIER.
This is called by all the lambda-get-* functions, it is inneficient but :shrug:"
  (let* ((arguments `(("FunctionName" . ,name-or-arn)
                      ("Qualifier" . ,Qualifier))))
    (call-python-method *lambda-client*
                        "get_function"
                        :kwargs arguments)))

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

(defun s3-list-all-items (bucket  &optional prefix continuation-token)
  "Returns a list of objects in BUCKET, optionally filter by PREFIX.
When CONTINUATION-TOKEN is provided, it means it is a follow up to a paged call.
This function is internal, use `s3-list-items' instead."
  (let* ((arguments `(("Bucket" . ,bucket)
                      ("Prefix" . ,prefix)
                      ("ContinuationToken" . ,continuation-token)))
         (response (call-python-method *s3-client*
                                       "list_objects_v2"
                                       :kwargs arguments))
         (contents (gethash "Contents" response))
         (next-token (gethash "NextContinuationToken" response)))
    (if next-token
        (concatenate 'vector
                     (s3-list-all-items bucket prefix next-token)
                     contents)
        contents)))

(defun s3-list-all-directories (bucket  &optional prefix)
  "Returns a list of \"directory\" objects in BUCKET, optionally filter by PREFIX.
This function is internal, use `s3-list-directories' instead."
  ;; I think this needs a continuation token too, but haven't found a sample call to confirm
  (let* ((arguments `(("Bucket" . ,bucket)
                      ("Prefix" . ,prefix)
                      ("Delimiter" . "/")))
         (response (call-python-method *s3-client*
                                       "list_objects_v2"
                                       :kwargs arguments)))
    (gethash "CommonPrefixes" response)))
