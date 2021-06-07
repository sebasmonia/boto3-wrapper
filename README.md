# boto3-wrapper

**NOT MEANT FOR PRODUCTION USE**

A bunch of functions that wrap AWS Boto3. Uses https://github.com/bendudson/py4cl/ to communicate with boto3 via a Python inferior process.

Why?

* I am moving my REPL tools from Python to Common Lisp.
* There isn't a CL SDK for AWS, and writing one is tempting but too big a project
* I could try to consume the low level APIs instead of boto3, but that adds a lot of complexity
  * Using boto3 via py4cl is much simpler, but it does pay a performance penalty
  * The perf penalty is way less than leaving Emacs and opening the AWS web console to check things :)
* Mayyyybe this code can be useful to other people working with AWS. The building blocks are in place to add more services.

## Usage

After dropping code in a place that Quicklisp can find it, for example `~/quicklisp/local-projects`:

```common-lisp
(ql:quickload :boto3-wrapper)
;; Sometimes the py4cl module import doesn't work, I couldn't figure out why yet, if that happens:
(py4cl:import-module "boto3" :as "b3py" :reload  t)
;; Setup the credentials to use from your ~/.aws/credentials file
(b3:set-profile "default")

;; List lambdas that contain "service-name"
(b3:lambda-list-functions "service-name")
;; Execute a lambda
(b3-wrapper:lambda-invoke "service-name-function-name" :show-log t)
;; Get the function's environment (env vars, etc):
(boto3-wrapper::lambda-get-function-environment "service-name-function-name")

;; List parameter store values:
(b3:ssm-list-parameters "/path/to/params")
;; update one:
(b3:ssm-put-parameter "/path/to/params/mykey" "myvalue" "This controls the nuclear reactor")
```

## How to add a new feature

Once you figure out from the boto3 docs how to do what you need, use `(call-python-method *client-for-the-service* :positional pos-args-list :kwargs kwargs-alist)`. Usually you get back a hashtable (converted from a Python dict).  
`call-python-method` will remove `nil` values from the keyword arguments, and treat "True" and "False" literally, among other conveniences.  
