(asdf:defsystem "com.evocomputing.clos-helpers"
  :description "Common Lisp utilities for working with CLOS classes."
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :license "MIT"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop
               #:parachute)
  :components ((:file "packages")
               (:file "clos-helpers"))
  :in-order-to ((asdf:test-op (asdf:test-op #:com.evocomputing.clos-helpers/tests))))

(asdf:defsystem "com.evocomputing.clos-helpers/tests"
  :description "Test system for com.evocomputing.clos-helpers"
  :author "Joel Boehland <jboehland@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:com.evocomputing.clos-helpers
               #:parachute)
  :components ((:file "packages")
               (:file "clos-helpers-tests"))
  :perform (asdf:test-op (op c) (uiop:symbol-call '#:com.evocomputing.clos-helpers-tests '#:run-tests)))
