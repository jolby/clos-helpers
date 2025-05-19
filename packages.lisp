(defpackage #:com.evocomputing.clos-helpers
  (:use #:cl)
  (:local-nicknames
   (#:alex #:alexandria))
  (:export
   #:ensure-class
   #:all-direct-slots
   #:all-slot-readers
   #:all-slot-writers
   #:slot-names
   #:direct-slot-names
   #:class-slot-exists-p
   #:map-slots
   #:to-alist
   #:to-plist
   #:slot-definition-for-name
   #:slot-initarg
   #:all-slot-initargs
   #:slot-reader
   #:slot-writer
   #:slot-reader-fn
   #:slot-writer-fn
   #:initarg-writer-pair
   #:initarg-writer-fn-pair
   #:all-initarg-writer-pairs
   #:all-initarg-writer-fn-pairs))

(defpackage #:com.evocomputing.clos-helpers-tests
  (:use #:cl #:parachute)
  (:export #:run-tests))
