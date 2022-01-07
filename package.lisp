;;;; package.lisp

(defpackage #:boto3-wrapper
  (:nicknames "b3" :b3)
  (:use #:common-lisp #:uiop)
  (:import-from :alexandria)
  (:import-from :jonathan)
  (:import-from :cl-base64)
  (:import-from :py4cl)
  (:import-from :str)
  (:export
   #:*current-profile*
   #:*s3-default-directory*
   #:*s3-default-bucket*
   #:set-profile
   #:ssm-list-parameters
   #:ssm-get-parameter
   #:ssm-put-parameter
   #:secretsm-get-secret
   #:secretsm-put-secret
   #:secretsm-list
   #:lambda-list-functions
   #:lambda-get-function-configuration
   #:lambda-get-function-tags
   #:lambda-get-function-environment
   #:lambda-invoke
   #:cloudf-list-stacks
   #:cloudf-get-stack-resources
   #:s3-list-buckets
   #:s3-list-items
   #:s3-list-directories
   #:s3-download
   #:s3-upload
   #:s3-delete))

(in-package #:boto3-wrapper)
