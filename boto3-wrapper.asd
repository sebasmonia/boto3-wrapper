;;;; boto3-wrapper.asd

(asdf:defsystem #:boto3-wrapper
  :description "Consume AWS boto3 from Common Lisp, via py4cl"
  :author "Sebastián Monía <smonia@outlook.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:uiop
               #:cl-base64
               #:jonathan
               #:py4cl)
  :components ((:file "package")
               (:file "plumbing")
               (:file "boto3-wrapper")))
