;;;; plumbing.lisp

(in-package #:boto3-wrapper)
;; Using :reload t in case it was already loaded in the REPL. I think?
;; Sometimes this fails until I import it in the REPL too, and I can't figure it out.
(py4cl:import-module "boto3" :as "b3py" :reload  t)

;; Keep all the ugliness of concatenating strings to create Python code in this function.
(defun call-python-method (py-obj method-name &key (positional nil) (kwargs nil))
  "Call METHOD-NAME on PY-OBJ, using POSITIONAL arguments and then KWARGS."
  (let ((method-call-string (format nil ".~a(~a~a)" method-name
                                    (format-positional positional)
                                    (format-kwargs kwargs))))
    (handler-case
        (progn
          (when *debug-python-calls*
            (format t "-----Python method----: ~a~%" method-call-string))
          (py4cl::python-eval py-obj method-call-string))
      (py4cl:python-error (c)
        (format t "~a~%" c)))))

(defun format-positional (arguments)
  "Prepare positional ARGUMENTS for py4cp's python-eval."
  (format nil "~{~a~^, ~}"
          (loop for arg in arguments
                collect (pythonize-value arg))))

(defun format-kwargs (arguments)
  "Prepare keyword ARGUMENTS for py4cp's python-eval.
Ignores arguments with NIL values."
  (format nil "~{~a~^, ~}"
          (loop for (key . value) in arguments
                if value
                  collect
                  (format nil "~a=~a" key (pythonize-value value)))))

(defun pythonize-value (the-value)
  "Convert THE-VALUE to a Python string for py4cl's python-eval."
  (cond
    ;; add more "literal" items to this test
    ((member the-value '("False" "True") :test #'equal) the-value)
    ((stringp the-value) (format nil "'~a'" the-value))
    (t the-value))) ;; numbers

(defun hash-table-to-alist-filter (input-ht keys-to-keep)
  "Converts INPUT-HT to an alist, then removes everything not in KEYS-TO-KEEP."
  (remove-if-not
   (lambda (pair) (member (car pair) keys-to-keep :test 'equal))
   (alexandria:hash-table-alist input-ht)))

(defun convert-aws-output (vector-of-ht keys-to-keep &optional (key-rename nil))
  "Converts each hashtable in VECTOR-OF-HT to a list of alists. Removes everything not in KEYS-TO-KEEP from each element.
If KEY-RENAME is provided, it should be an alist were the car is the current key name and
the cdr is the new key name. In that case KEYS-TO-KEEP should use the new key name."
  (when key-rename
    ;; it's easier to add a new ht key and filter later, than to setf the new key value
    ;; in the output a list.
    (loop for a-ht across vector-of-ht
          do (loop for (old . new) in key-rename
                   do (setf (gethash new a-ht) (gethash old a-ht)))))
  (loop for a-ht across vector-of-ht
        collect (hash-table-to-alist-filter a-ht keys-to-keep)))

(defun json-string-to-alist (json-string &key (null :null) (false "False") (empty-array "[]"))
  (let ((jonathan:*null-value* null)
        (jonathan:*false-value* false)
        (jonathan:*empty-array-value* empty-array ))
    (jonathan:parse json-string :as :alist)))

(defun python-datetime-string (py-object &key (date-only nil))
  (if (not py-object)
      "-"
      (let* ((date-string (py4cl::python-eval py-object ".replace(microsecond=0).isoformat(sep=' ')")))
        (subseq date-string 0 (if date-only
                                  10
                                  19)))))
