;;;; package.lisp

(defpackage #:boto3-wrapper
  (:nicknames "b3" :b3)
  (:use #:common-lisp #:uiop)
  (:import-from :alexandria)
  (:import-from :jonathan)
  (:import-from :cl-base64)
  (:import-from :py4cl)
  (:export
   #:*current-profile*
   #:set-profile
   #:ssm-list-parameters
   #:ssm-get-parameter
   #:ssm-put-parameter
   #:lambda-list-functions
   #:lambda-get-function-configuration
   #:lambda-get-function-tags
   #:lambda-get-function-environment
   #:lambda-invoke))

(in-package #:boto3-wrapper)
