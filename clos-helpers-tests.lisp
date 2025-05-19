(in-package #:com.evocomputing.clos-helpers-tests)

;; Define some test classes
(defclass base-test-class ()
  ((base-slot-1 :initarg :base-slot-1 :accessor base-slot-1)
   (base-slot-2 :initarg :base-slot-2 :reader base-slot-2)
   (base-slot-3 :initarg :base-slot-3 :writer base-slot-3)
   (base-slot-4 :initform "default" :accessor base-slot-4))
  (:documentation "A base class for testing CLOS helpers."))

(defclass subclass-test-class (base-test-class)
  ((sub-slot-1 :initarg :sub-slot-1 :accessor sub-slot-1)
   (sub-slot-2 :initform 100 :reader sub-slot-2))
  (:documentation "A subclass for testing CLOS helpers."))

(defclass simple-test-class ()
  ((slot-a :initarg :slot-a :accessor slot-a)
   (slot-b :initarg :slot-b)) ; No reader/writer
  (:documentation "A simple class for testing."))

(defclass no-slots-class ()
  ()
  (:documentation "A class with no slots."))

(defun finalize-test-classes ()
  ;; Ensure the test classes are finalized
  (com.evocomputing.clos-helpers:ensure-class-finalized 'base-test-class)
  (com.evocomputing.clos-helpers:ensure-class-finalized 'subclass-test-class)
  (com.evocomputing.clos-helpers:ensure-class-finalized 'simple-test-class)
  (com.evocomputing.clos-helpers:ensure-class-finalized 'no-slots-class))

(defun plist-to-sorted-alist (plist)
  "Helper function to convert a plist to an alist sorted by keys."
  (let ((alist nil))
    (loop for (key val) on plist by #'cddr
          do (push (cons key val) alist))
    (sort alist #'string< :key #'car)))

;; Main test entry point
(defun run-tests ()
  (parachute:test 'clos-helpers-suite))

;; Define the main test suite
(define-test clos-helpers-suite
  :parent NIL
  :description "Main test suite for com.evocomputing.clos-helpers.")

;; Test ENSURE-CLASS
(define-test ensure-class-tests
  :parent clos-helpers-suite
  (finalize-test-classes)
  (is eq (find-class 'standard-object)
        (com.evocomputing.clos-helpers:ensure-class 'standard-object)
        "ENSURE-CLASS with symbol input")
  (is eq (find-class 'standard-object)
        (com.evocomputing.clos-helpers:ensure-class "STANDARD-OBJECT")
        "ENSURE-CLASS with string input")
  (is eq (find-class 'standard-object)
        (com.evocomputing.clos-helpers:ensure-class (find-class 'standard-object))
        "ENSURE-CLASS with class object input"))

;; Test slot name and definition functions
(define-test slot-definition-tests
  :parent clos-helpers-suite
  (finalize-test-classes)
  (let ((base-class (find-class 'base-test-class))
        (sub-class (find-class 'subclass-test-class))
        (simple-class (find-class 'simple-test-class))
        (no-slots-class (find-class 'no-slots-class)))

    (is equal '(BASE-SLOT-1 BASE-SLOT-2 BASE-SLOT-3 BASE-SLOT-4)
          (sort (com.evocomputing.clos-helpers:slot-names base-class) #'string< :key #'symbol-name)
          "SLOT-NAMES on base class")
    (is equal '(BASE-SLOT-1 BASE-SLOT-2 BASE-SLOT-3 BASE-SLOT-4)
          (sort (com.evocomputing.clos-helpers:direct-slot-names base-class) #'string< :key #'symbol-name)
          "DIRECT-SLOT-NAMES on base class")

    (is equal '(BASE-SLOT-1 BASE-SLOT-2 BASE-SLOT-3 BASE-SLOT-4 SUB-SLOT-1 SUB-SLOT-2)
          (sort (com.evocomputing.clos-helpers:slot-names sub-class) #'string< :key #'symbol-name)
          "SLOT-NAMES on subclass (includes inherited)")
    (is equal '(SUB-SLOT-1 SUB-SLOT-2)
          (sort (com.evocomputing.clos-helpers:direct-slot-names sub-class) #'string< :key #'symbol-name)
          "DIRECT-SLOT-NAMES on subclass")

    (true (com.evocomputing.clos-helpers:class-slot-exists-p base-class 'base-slot-1)
             "CLASS-SLOT-EXISTS-P true case")
    (false (com.evocomputing.clos-helpers:class-slot-exists-p base-class 'non-existent-slot)
              "CLASS-SLOT-EXISTS-P false case")

    (true (typep  (com.evocomputing.clos-helpers:slot-definition-for-name base-class 'base-slot-1) 'c2mop:slot-definition)
             "SLOT-DEFINITION-FOR-NAME finds slot")
    (false (com.evocomputing.clos-helpers:slot-definition-for-name base-class 'non-existent-slot)
              "SLOT-DEFINITION-FOR-NAME returns nil for non-existent slot")
    (fail (com.evocomputing.clos-helpers:slot-definition-for-name base-class 'non-existent-slot :error-if-not-found t)
        'error
        "SLOT-DEFINITION-FOR-NAME signals error when requested")))

;; Test reader, writer, initarg functions
(define-test slot-access-tests
  :parent clos-helpers-suite
  (finalize-test-classes)
  (let ((base-class (find-class 'base-test-class))
        (simple-class (find-class 'simple-test-class)))

    ;; Readers
    (is eq 'base-slot-1 (com.evocomputing.clos-helpers:slot-reader base-class 'base-slot-1) "SLOT-READER finds accessor reader")
    (is eq 'base-slot-2 (com.evocomputing.clos-helpers:slot-reader base-class 'base-slot-2) "SLOT-READER finds reader")
    ;; (false (com.evocomputing.clos-helpers:slot-reader base-class 'base-slot-3) "SLOT-READER returns nil for writer-only")
    (true (functionp (com.evocomputing.clos-helpers:slot-writer-fn base-class 'base-slot-2)) "SLOT-WRITER-FN returns a function for reader-only slot (via slot-value)")
    (false (com.evocomputing.clos-helpers:slot-reader simple-class 'slot-b) "SLOT-READER returns nil for no reader/writer")
    (fail (com.evocomputing.clos-helpers:slot-reader base-class 'non-existent-slot :error-if-not-found t)
        'error
        "SLOT-READER signals error when requested")

    ;; Writers
    (is eq '(setf base-slot-1) (com.evocomputing.clos-helpers:slot-writer base-class 'base-slot-1) "SLOT-WRITER finds accessor writer")
    (is eq 'base-slot-3 (com.evocomputing.clos-helpers:slot-writer base-class 'base-slot-3)
        "SLOT-WRITER finds writer")
    (false (com.evocomputing.clos-helpers:slot-writer base-class 'base-slot-2) "SLOT-WRITER returns nil for reader-only")
    (false (com.evocomputing.clos-helpers:slot-writer simple-class 'slot-b) "SLOT-WRITER returns nil for no reader/writer")
    (fail (com.evocomputing.clos-helpers:slot-writer base-class 'non-existent-slot :error-if-not-found t)
        'error
        "SLOT-WRITER signals error when requested")

    ;; Initargs
    (is eq :base-slot-1 (com.evocomputing.clos-helpers:slot-initarg base-class 'base-slot-1) "SLOT-INITARG finds initarg")
    (false (com.evocomputing.clos-helpers:slot-initarg base-class 'base-slot-4) "SLOT-INITARG returns nil for no initarg")
    (fail (com.evocomputing.clos-helpers:slot-initarg base-class 'non-existent-slot :error-if-not-found t)
        'error
        "SLOT-INITARG signals error when requested")

    ;; All initargs
    (is equal '(:BASE-SLOT-1 :BASE-SLOT-2 :BASE-SLOT-3)
          (sort (com.evocomputing.clos-helpers:all-slot-initargs base-class) #'string< :key #'symbol-name)
          "ALL-SLOT-INITARGS on base class")
    (is equal '(:BASE-SLOT-1 :BASE-SLOT-2 :BASE-SLOT-3 :SUB-SLOT-1)
          (sort (com.evocomputing.clos-helpers:all-slot-initargs (find-class 'subclass-test-class)) #'string< :key #'symbol-name)
          "ALL-SLOT-INITARGS on subclass")
    (is equal '(:SLOT-A :SLOT-B)
          (com.evocomputing.clos-helpers:all-slot-initargs simple-class)
          "ALL-SLOT-INITARGS on simple class")))

;; Test map-slots, to-alist, to-plist
(define-test slot-mapping-tests
  :parent clos-helpers-suite
  (finalize-test-classes)
  (let ((instance (make-instance 'subclass-test-class
                                 :base-slot-1 "base-val-1"
                                 :base-slot-2 "base-val-2"
                                 :base-slot-3 "base-val-3"
                                 :sub-slot-1 "sub-val-1")))
    ;; map-slots
    (is equal '((BASE-SLOT-1 . "base-val-1")
                (BASE-SLOT-2 . "base-val-2")
                (BASE-SLOT-3 . "base-val-3")
                (BASE-SLOT-4 . "default") ; initform
                (SUB-SLOT-1 . "sub-val-1")
                (SUB-SLOT-2 . 100)) ; initform
          (sort (com.evocomputing.clos-helpers:map-slots #'cons instance) #'string< :key #'car)
          "MAP-SLOTS on instance")

    ;; to-alist
    (is equal '((BASE-SLOT-1 . "base-val-1")
                (BASE-SLOT-2 . "base-val-2")
                (BASE-SLOT-3 . "base-val-3")
                (BASE-SLOT-4 . "default")
                (SUB-SLOT-1 . "sub-val-1")
                (SUB-SLOT-2 . 100))
          (sort (com.evocomputing.clos-helpers:to-alist instance) #'string< :key #'car)
          "TO-ALIST on instance")

    ;; to-plist
    (is equal (plist-to-sorted-alist
               '(:BASE-SLOT-1 "base-val-1"
                :BASE-SLOT-2 "base-val-2"
                :BASE-SLOT-3 "base-val-3"
                :BASE-SLOT-4 "default"
                :SUB-SLOT-1 "sub-val-1"
                :SUB-SLOT-2 100))
          (sort (plist-to-sorted-alist (com.evocomputing.clos-helpers:to-plist instance)) #'string< :key #'car)
          "TO-PLIST on instance")))

;; Test initarg/writer pairs
(define-test initarg-writer-tests
  :parent clos-helpers-suite
  (finalize-test-classes)
  (let ((base-class (find-class 'base-test-class))
        (simple-class (find-class 'simple-test-class)))

    ;; initarg-writer-pair
    (is equal '(:BASE-SLOT-1  SETF BASE-SLOT-1)
        (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'base-slot-1)
        "INITARG-WRITER-PAIR finds accessor pair")
    (false (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'base-slot-2) "INITARG-WRITER-PAIR returns nil for reader-only with initarg")
    (is equal '(:BASE-SLOT-3 . BASE-SLOT-3)
        (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'base-slot-3)
        "INITARG-WRITER-PAIR finds writer-only slot with initarg")
    (false (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'base-slot-4)
           "INITARG-WRITER-PAIR returns nil for no initarg")
    (is equal '(:SLOT-A SETF SLOT-A)
        (com.evocomputing.clos-helpers:initarg-writer-pair simple-class 'slot-a)
        "INITARG-WRITER-PAIR on simple class")
    (false (com.evocomputing.clos-helpers:initarg-writer-pair simple-class 'slot-b)
           "INITARG-WRITER-PAIR returns nil for slot with no initarg/writer")
    (fail (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'non-existent-slot :error-if-not-found t)
        'error
        "INITARG-WRITER-PAIR signals error when requested")

    ;; all-initarg-writer-pairs
    (is equal '((:BASE-SLOT-1 SETF BASE-SLOT-1)
                (:BASE-SLOT-3 . BASE-SLOT-3))
        (sort (com.evocomputing.clos-helpers:all-initarg-writer-pairs base-class) #'string< :key #'car)
        "ALL-INITARG-WRITER-PAIRS on base class")
    (is equal '((:BASE-SLOT-1 SETF BASE-SLOT-1)
                (:BASE-SLOT-3 . BASE-SLOT-3)
                (:SUB-SLOT-1 SETF SUB-SLOT-1))
        (sort (com.evocomputing.clos-helpers:all-initarg-writer-pairs (find-class 'subclass-test-class)) #'string< :key #'car)
        "ALL-INITARG-WRITER-PAIRS on subclass")
    (is equal '((:SLOT-A SETF SLOT-A))
        (com.evocomputing.clos-helpers:all-initarg-writer-pairs simple-class)
        "ALL-INITARG-WRITER-PAIRS on simple class")))

#+(or)
(define-test initarg-writer-tests
  :parent clos-helpers-suite
  (finalize-test-classes)
  (let ((base-class (find-class 'base-test-class))
        (simple-class (find-class 'simple-test-class)))

    ;; initarg-writer-pair
    (is equal '(:BASE-SLOT-1 . BASE-SLOT-1)
          (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'base-slot-1)
          "INITARG-WRITER-PAIR finds accessor pair")
    (false (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'base-slot-2) "INITARG-WRITER-PAIR returns nil for reader-only with initarg")
    (is equal '(:BASE-SLOT-3 . (SETF BASE-SLOT-3))
          (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'base-slot-3)
          "INITARG-WRITER-PAIR finds writer-only slot with initarg")
    (false (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'base-slot-4)
              "INITARG-WRITER-PAIR returns nil for no initarg")
    (is equal '(:SLOT-A . (SETF SLOT-A))
          (com.evocomputing.clos-helpers:initarg-writer-pair simple-class 'slot-a)
          "INITARG-WRITER-PAIR on simple class")
    (false (com.evocomputing.clos-helpers:initarg-writer-pair simple-class 'slot-b)
              "INITARG-WRITER-PAIR returns nil for slot with no initarg/writer")
    (fail (com.evocomputing.clos-helpers:initarg-writer-pair base-class 'non-existent-slot :error-if-not-found t)
        'error
        "INITARG-WRITER-PAIR signals error when requested")

    ;; all-initarg-writer-pairs
    (is equal '((:BASE-SLOT-1 . BASE-SLOT-1)
                (:BASE-SLOT-3 . (SETF BASE-SLOT-3)))
          (sort (com.evocomputing.clos-helpers:all-initarg-writer-pairs base-class) #'string< :key #'car)
          "ALL-INITARG-WRITER-PAIRS on base class")
    (is equal '((:BASE-SLOT-1 . BASE-SLOT-1)
                (:BASE-SLOT-3 . (SETF BASE-SLOT-3))
                (:SUB-SLOT-1 . SUB-SLOT-1))
          (sort (com.evocomputing.clos-helpers:all-initarg-writer-pairs (find-class 'subclass-test-class)) #'string< :key #'car)
          "ALL-INITARG-WRITER-PAIRS on subclass")
    (is equal '((:SLOT-A . (SETF SLOT-A)))
          (com.evocomputing.clos-helpers:all-initarg-writer-pairs simple-class)
          "ALL-INITARG-WRITER-PAIRS on simple class")))

;; Test reader/writer function pairs
(define-test initarg-writer-fn-tests
  :parent clos-helpers-suite
  (finalize-test-classes)
  (let ((base-class (find-class 'base-test-class))
        (simple-class (find-class 'simple-test-class))
        (instance (make-instance 'base-test-class)))

    ;; slot-reader-fn
    (let ((reader-fn (com.evocomputing.clos-helpers:slot-reader-fn base-class 'base-slot-1)))
      (true (functionp reader-fn) "SLOT-READER-FN returns a function")
      (setf (base-slot-1 instance) "test-value")
      (is equal "test-value" (funcall reader-fn instance) "SLOT-READER-FN reads value"))
     (let ((reader-fn (com.evocomputing.clos-helpers:slot-reader-fn base-class 'base-slot-4))) ; Slot with no explicit reader
      (true (functionp reader-fn) "SLOT-READER-FN returns a function for slot with no explicit reader")
      (setf (base-slot-4 instance) "test-value-2")
      (is equal "test-value-2" (funcall reader-fn instance) "SLOT-READER-FN reads value for slot with no explicit reader"))
    ;; (false (com.evocomputing.clos-helpers:slot-reader-fn base-class 'base-slot-3) "SLOT-READER-FN returns nil for writer-only")
    (true (functionp (com.evocomputing.clos-helpers:slot-reader-fn base-class 'base-slot-3))
          "SLOT-READER-FN returns a function for writer-only slot (via slot-value)")
    (fail (com.evocomputing.clos-helpers:slot-reader-fn base-class 'non-existent-slot :error-if-not-found t)
        'error
        "SLOT-READER-FN signals error when requested")


    ;; slot-writer-fn
    (let ((writer-fn (com.evocomputing.clos-helpers:slot-writer-fn base-class 'base-slot-1)))
      (true (functionp writer-fn) "SLOT-WRITER-FN returns a function")
      (funcall writer-fn instance "new-value")
      (is equal "new-value" (base-slot-1 instance) "SLOT-WRITER-FN writes value"))
     (let ((writer-fn (com.evocomputing.clos-helpers:slot-writer-fn simple-class 'slot-b))) ; Slot with no explicit writer
      (true (functionp writer-fn) "SLOT-WRITER-FN returns a function for slot with no explicit writer")
      (funcall writer-fn (make-instance 'simple-test-class) "new-value-b") ; Need a new instance as slot-b has no initform
      ;; Cannot easily test the value was set without a reader, but the function should be created.
      )
    (true (functionp (com.evocomputing.clos-helpers:slot-writer-fn base-class 'base-slot-2))
          "SLOT-WRITER-FN returns returns a function for reader-only")
    (fail (com.evocomputing.clos-helpers:slot-writer-fn base-class 'non-existent-slot :error-if-not-found t)
        'error
        "SLOT-WRITER-FN signals error when requested")

    ;; initarg-writer-fn-pair
    (let ((pair (com.evocomputing.clos-helpers:initarg-writer-fn-pair base-class 'base-slot-1)))
      (true (consp pair) "INITARG-WRITER-FN-PAIR returns a cons")
      (is eq :BASE-SLOT-1 (car pair) "INITARG-WRITER-FN-PAIR car is initarg")
      (true (functionp (cdr pair)) "INITARG-WRITER-FN-PAIR cdr is a function"))
    (false (com.evocomputing.clos-helpers:initarg-writer-fn-pair base-class 'base-slot-4)
           "INITARG-WRITER-FN-PAIR returns nil for no initarg")
    (let ((pair (com.evocomputing.clos-helpers:initarg-writer-fn-pair simple-class 'slot-b)))
      (true (consp pair) "INITARG-WRITER-FN-PAIR finds pair for slot with initarg and no explicit writer")
      (is eq :SLOT-B (car pair) "INITARG-WRITER-FN-PAIR car is initarg for slot-b")
      (true (functionp (cdr pair)) "INITARG-WRITER-FN-PAIR cdr is a function for slot-b"))
    (fail (com.evocomputing.clos-helpers:initarg-writer-fn-pair base-class 'non-existent-slot :error-if-not-found t)
        'error
        "INITARG-WRITER-FN-PAIR signals error when requested")

    ;; all-initarg-writer-fn-pairs
    (let ((pairs (com.evocomputing.clos-helpers:all-initarg-writer-fn-pairs base-class)))
      (is eq 3 (length pairs) "ALL-INITARG-WRITER-FN-PAIRS returns correct number of pairs for base class")
      (true (every #'consp pairs) "ALL-INITARG-WRITER-FN-PAIRS returns list of conses")
      (true (every #'keywordp (mapcar #'car pairs)) "ALL-INITARG-WRITER-FN-PAIRS car are keywords")
      (true (every #'functionp (mapcar #'cdr pairs)) "ALL-INITARG-WRITER-FN-PAIRS cdr are functions"))
    (let ((pairs (com.evocomputing.clos-helpers:all-initarg-writer-fn-pairs (find-class 'subclass-test-class))))
       (is eq 4 (length pairs) "ALL-INITARG-WRITER-FN-PAIRS on subclass returns correct number of pairs"))
    (let ((pairs (com.evocomputing.clos-helpers:all-initarg-writer-fn-pairs simple-class)))
       (is eq 2 (length pairs) "ALL-INITARG-WRITER-FN-PAIRS on simple class returns correct number of pairs"))))
