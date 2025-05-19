(in-package :com.evocomputing.clos-helpers)

;;;; ================================================================================
;;;; CLOS utilities
;;;; ================================================================================

(defun ensure-class (class-or-name)
  "Ensure that CLASS-OR-NAME is a class, and return it."
  (etypecase class-or-name
    (class class-or-name)
    (string (find-class (intern (string-upcase class-or-name))))
    (symbol (find-class class-or-name))))
;; (ensure-class "standard-object")

(defun ensure-class-finalized (class-or-name)
  "Ensure that CLASS-OR-NAME is a class, it is finalized and return it."
  (c2mop:ensure-finalized (ensure-class class-or-name)))

;; https://stackoverflow.com/questions/38452350/is-there-a-way-to-gather-slot-definition-readers-from-all-the-inheritance-tree
(defun all-direct-slots (class)
  (setf class (ensure-class-finalized class))
  (append (c2mop:class-direct-slots class)
          (alexandria:mappend #'all-direct-slots
                  (c2mop:class-direct-superclasses class))))
;;(all-direct-slots (find-class 'subclass-accessor-test))

(defun all-slot-readers (class)
  (setf class (ensure-class class))
  (alexandria:mappend #'c2mop:slot-definition-readers
              (all-direct-slots class)))

(defun all-slot-writers (class)
  (setf class (ensure-class class))
  (alexandria:mappend #'c2mop:slot-definition-writers
              (all-direct-slots class)))

;; From cl-mop
;; https://github.com/inaimathi/cl-mop (MIT License)
;;;;;;;;;;;;;;; basic operations
(defgeneric slot-names (object)
  (:documentation "Returns a list of slot names for the given object."))

(defmethod slot-names ((object error))
  (slot-names (class-of object)))

(defmethod slot-names ((object standard-object))
  (slot-names (class-of object)))

(defmethod slot-names ((class symbol))
  (mapcar #'c2mop:slot-definition-name (c2mop:class-slots (ensure-class-finalized class))))

(defmethod slot-names ((class standard-class))
  (mapcar #'c2mop:slot-definition-name (c2mop:class-slots (ensure-class-finalized class))))

(defgeneric direct-slot-names (object)
  (:documentation "Returns a list of slot names for the given object."))

(defmethod direct-slot-names ((object error))
  (direct-slot-names (class-of object)))

(defmethod direct-slot-names ((object standard-object))
  (direct-slot-names (class-of object)))

(defmethod direct-slot-names ((class symbol))
  (mapcar #'c2mop:slot-definition-name (c2mop:class-direct-slots (ensure-class class))))

(defmethod direct-slot-names ((class standard-class))
  (mapcar #'c2mop:slot-definition-name (c2mop:class-direct-slots class)))

(defgeneric map-slots (function instance)
  (:documentation "Takes a binary function and an instance.
Returns the sequence resulting from calling the function on each bound (slot-name slot-value) of instance"))

(defmethod map-slots ((fn function) (instance condition))
  (loop for slot in (c2mop:class-slots (class-of instance))
    for slot-name = (c2mop:slot-definition-name slot)
    when (slot-boundp instance slot-name)
    collect (funcall fn slot-name (slot-value instance slot-name))))

(defmethod map-slots ((fn function) (instance standard-object))
  "The default case of map-slots specializes on STANDARD-OBJECT."
  (loop for slot in (c2mop:class-slots (class-of instance))
    for slot-name = (c2mop:slot-definition-name slot)
    when (slot-boundp instance slot-name)
    collect (funcall fn slot-name (slot-value instance slot-name))))

(defmethod to-alist ((instance error))
  (map-slots (lambda (k v) (cons k v)) instance))

(defmethod to-alist ((instance standard-object))
  "Returns an assoc list of (k . v) pairs from the given instances' slots and slot-values.
This is meant to provide an easy way of showing "
  (map-slots (lambda (k v) (cons k v)) instance))

;; Added by me
(defmethod to-plist ((instance error))
  (alexandria:flatten (map-slots (lambda (k v) (list (alexandria:make-keyword k) v)) instance)))

(defmethod to-plist ((instance standard-object))
  "Returns a plist of ((keyword k) v ...) pairs from the given instances' slots and slot-values.
This is meant to provide an easy way of showing "
  (alexandria:flatten (map-slots (lambda (k v) (list (alexandria:make-keyword k) v)) instance)))

;;;;;;;;;;;;;;; copying functions
;;;;; shallow
(defgeneric shallow-copy (object)
  (:documentation "Provides a general shallow-copy function for CLOS objects. If you've got a special case, write a new defmethod."))

(defmethod shallow-copy ((object standard-object))
  "The default shallow copy specializes on STANDARD-OBJECT. It takes an object and returns a shallow copy."
  (let ((copy (allocate-instance (class-of object))))
    (map-slots
     (lambda (k v) (setf (slot-value copy k) v))
     object)
    copy))

;;;;; deep
(defgeneric deep-copy (object)
  (:documentation "Does a general deep-copy on the given object and sub-pieces.
Returns atoms, numbers and chars.
Runs copy-tree on lists, and copy-seq on other sequences.
Runs copy-structure on pathnames, hash tables and other structure-objects"))

(defmethod deep-copy (object)
  "The default unspecialized case should only catch atoms, numbers and characters.
It merely returns its results."
  object)

(defmethod deep-copy ((object standard-object))
  "The default deep copy specializes on STANDARD-OBJECT. It takes an object and returns a deep copy."
  (let ((copy (allocate-instance (class-of object))))
    (map-slots
     (lambda (k v) (setf (slot-value copy k) (deep-copy v)))
     object)
    copy))

(defmethod deep-copy ((object sequence))
  "A deep copy of a general sequence is merely (copy-seq sequence)."
  (copy-seq object))

(defmethod deep-copy ((object list))
  "A deep copy of a list is (copy-tree list)"
  (copy-tree object))

(defmethod deep-copy ((object structure-object))
  "A deep copy of a structure-object is (copy-structure object)."
  (copy-structure object))

(defun slot-definition-for-name (class slot-name &key (error-if-not-found nil))
  "Get the slot-definition for SLOT-NAME in CLASS, and return the slot-definition"
  (let ((dslots (all-direct-slots (ensure-class class))))
    (alexandria:if-let ((slot-def (loop :for ds in dslots
                                        :when (string= slot-name (c2mop:slot-definition-name ds))
                                        :return ds)))
    slot-def
      (when error-if-not-found
        (error "No slot definition found for ~a in ~a" slot-name class)))))

(defun class-slot-exists-p (class slot-name)
  (alexandria:when-let ((slot (slot-definition-for-name (ensure-class class)
                                                        slot-name :error-if-not-found nil)))
    (not (null slot))))

(defun slot-initarg (class slot-name
                     &key
                       (error-if-not-found nil)
                       (selection-heuristic :first))
  "Retrieves the primary initarg keyword (e.g., :create-time) for a given SLOT-NAME within a CLASS.

ARGS:
  CLASS: Class object or symbol.
  SLOT-NAME: Name of the slot to check.
  ERROR-IF-NOT-FOUND: If t, raises an error if the slot is not found.
  SELECTION-HEURISTIC: How to select the slot if multiple matches are found.
                       Can be :FIRST or :EXPLICIT or :ALL."
  (let ((class (ensure-class class)))
    (alex:if-let ((slot-def (slot-definition-for-name class slot-name
                                                      :error-if-not-found error-if-not-found)))
    (alex:if-let ((initargs (c2mop:slot-definition-initargs slot-def)))
            (cond
              ((or (eql selection-heuristic :first)
                   (= 1 (length initargs)))
               (first initargs))

              ((eql selection-heuristic :all)
               initargs)

              ((and (> (length initargs) 1)
                    (eql selection-heuristic :explicit))
               ;; We're looking for the first initarg that doesn't match the slot name.
               ;; Otherwise, we just return the first initarg.
               (or (loop :for ia :in initargs
             :when (not (string= slot-name (symbol-name ia)))
             :return ia)
           (first initargs))))

          (when error-if-not-found
            (error "No initargs found for slot ~a in class ~a" slot-name class)))

      (when error-if-not-found
        (error "Slot ~a not found in class ~a" slot-name class)))))

(defun all-slot-initargs (class
                          &key
                            (error-if-not-found nil)
                            (selection-heuristic :first))
  "Get all slot initargs for a given CLASS."
  (remove-if #'null
             (loop :for slot-name :in (slot-names (ensure-class class))
                   :for slot-def = (slot-definition-for-name class slot-name
                                                             :error-if-not-found error-if-not-found)
                   :when slot-def
                   :collect (slot-initarg class slot-name
                                          :error-if-not-found error-if-not-found
                                          :selection-heuristic selection-heuristic))))

(defun slot-reader (class slot-name &key (error-if-not-found nil))
  "Get the slot-definition for SLOT-NAME in CLASS, and return the reader function symbol
for the slot if it exists."
  (let ((class (ensure-class class)))
    (alexandria:if-let ((slot (slot-definition-for-name class slot-name
                                                        :error-if-not-found error-if-not-found)))
    (alexandria:if-let ((reader (c2mop:slot-definition-readers slot)))
            (first reader)
          (when error-if-not-found
            (error "No slot reader found for ~a in ~a" slot-name class)))
      (when error-if-not-found
        (error "No slot definition found for ~a in ~a" slot-name class)))))

(defun slot-writer (class slot-name &key (error-if-not-found nil))
  "Get the slot-definition for SLOT-NAME in CLASS, and return the writer function symbol if it exists"
  (let ((class (ensure-class class)))
    (alexandria:if-let ((slot (slot-definition-for-name class slot-name
                                                        :error-if-not-found error-if-not-found)))
    (alexandria:if-let ((writer (c2mop:slot-definition-writers slot)))
            (first writer)
          (when error-if-not-found
            (error "No slot writer found for ~a in ~a" slot-name class)))
      (when error-if-not-found
        (error "No slot definition found for ~a in ~a" slot-name class)))))

(defun slot-reader-fn (class slot-name &key (error-if-not-found nil))
  "Get the slot-definition for SLOT-NAME in CLASS, and return a function that reads
the slot. First try to use the reader function, otherwise return a lambda that
uses slot-value."
  (let ((class (ensure-class class)))
    (alexandria:if-let ((reader (slot-reader class slot-name :error-if-not-found error-if-not-found)))
    (lambda (obj)
      (when (slot-boundp obj slot-name)
        (funcall (fdefinition reader) obj)))
      (alexandria:if-let ((slot (slot-definition-for-name class slot-name
                                                          :error-if-not-found error-if-not-found)))
          (lambda (obj)
            (when (slot-boundp obj slot-name)
              (slot-value obj slot-name)))
        (when error-if-not-found
          (error "No slot definition found for ~a in ~a" slot-name class))))))

(defun slot-writer-fn (class slot-name &key (error-if-not-found nil))
  "Get the slot-definition for SLOT-NAME in CLASS, and return a function that
writes the slot. First try to use the writer function, otherwise return a lambda
that uses setf on slot-value."
  (let ((class (ensure-class class)))
    (alexandria:if-let ((reader (slot-writer class slot-name
                                             :error-if-not-found error-if-not-found)))
    (lambda (obj val) (funcall (fdefinition reader) val obj))
      (alexandria:if-let ((slot (slot-definition-for-name class slot-name
                                                          :error-if-not-found error-if-not-found)))
          (lambda (obj val) (setf (slot-value obj slot-name) val))
        (when error-if-not-found
          (error "No slot definition found for ~a in ~a" slot-name class))))))

(defun initarg-writer-pair (class slot-name
                            &key
                              (error-if-not-found nil)
                              (selection-heuristic :first))
  (let* ((class (ensure-class class))
         (initarg (slot-initarg class slot-name
                                :error-if-not-found error-if-not-found
                                :selection-heuristic selection-heuristic))
         (slot-writer (slot-writer class slot-name)))
    (when (and error-if-not-found
               (some #'null (list initarg slot-writer)))
      (error (format nil
                     "Could not resolve both slot init arg and slot-writer~%~@
                      For class: ~A and slot: ~A~%"
                     class slot-name)))
    (when (and initarg slot-writer)
      (cons initarg slot-writer))))

(defun all-initarg-writer-pairs (class
                                 &key
                                   (error-if-not-found nil)
                                   (selection-heuristic :first))
  (remove-if #'null
             (mapcar
              (lambda (slot-name)
                (initarg-writer-pair class slot-name
                                     :error-if-not-found error-if-not-found
                                     :selection-heuristic selection-heuristic))
              (slot-names class))))

(defun initarg-writer-fn-pair (class slot-name
                               &key
                 (error-if-not-found nil)
                 (selection-heuristic :first))
  (let* ((class (ensure-class class))
         (initarg (slot-initarg class slot-name
                                :error-if-not-found error-if-not-found
                                :selection-heuristic selection-heuristic))
         (slot-writer-fn (slot-writer-fn class slot-name)))
    (when (and error-if-not-found
               (some #'null (list initarg slot-writer-fn)))
      (error (format nil
                     "Could not resolve both slot init arg and slot-writer-fn~%~@
                      For class: ~A and slot: ~A~%"
                     class slot-name)))
    (when (and initarg slot-writer-fn)
      (cons initarg slot-writer-fn))))

(defun all-initarg-writer-fn-pairs (class
                                    &key
                                      (error-if-not-found nil)
                                      (selection-heuristic :first))
  (remove-if #'null
             (mapcar
              (lambda (slot-name)
                (initarg-writer-fn-pair class slot-name
                    :error-if-not-found error-if-not-found
                    :selection-heuristic selection-heuristic))
              (slot-names class))))
