;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-serializer)

;;;;;;;;;;;
;;; Special

(def constant +t+                           #x00)

(def constant +nil+                         #x01)

(def constant +cons+                        #x02)

(def constant +proper-list+                 #x03)

(def constant +dotted-list+                 #x04)

;;;;;;;;;;
;;; String

(def constant +base-char+                   #x10)

(def constant +extended-char+               #x11)

(def constant +character+                   #x12)

(def constant +base-string+                 #x13)

(def constant +simple-base-string+          #x14)

(def constant +simple-string+               #x15)

(def constant +string+                      #x16)

;;;;;;;;;;
;;; Symbol

(def constant +keyword+                     #x20)

(def constant +uninterned-symbol+           #x21)

(def constant +symbol+                      #x22)

(def constant +package+                     #x23)

;;;;;;;;;;
;;; Number

(def constant +zero+                        #x30)

(def constant +integer+                     #x31)

(def constant +rational+                    #x32)

(def constant +float+                       #x33)

(def constant +short-float+                 #x34)

(def constant +single-float+                #x35)

(def constant +double-float+                #x36)

(def constant +long-float+                  #x37)

(def constant +complex+                     #x38)

;;;;;;;;;;;;
;;; Compound

(def constant +hash-table+                  #x40)

(def constant +pathname+                    #x41)

(def constant +simple-vector+               #x42)

(def constant +vector+                      #x43)

(def constant +array+                       #x44)

(def constant +simple-bit-vector+           #x45)

(def constant +bit-vector+                  #x46)

;;;;;;;;;;
;;; Object

(def constant +standard-object+             #x50)

(def constant +unbound-slot+                #x51)

(def constant +structure-object+            #x52)

;;;;;;;;;;;;;;;
;;; Circularity

(def constant +reference+                   #x7F)

(def constant +referenced-bit-marker-index+ #x07)

(def constant +code-mask+                   #x7F)

;;;;;;;;;;;;;;;;;;
;;; Code -> lambda

(def (constant :test 'equalp) +serializers+ (make-array 128))

(def (constant :test 'equalp) +deserializers+ (make-array 128))

;;;;;;;;;;;
;;; Context

(def constant +version+ 0)

(defstruct (serializer-context (:conc-name sc-))
  (buffer
   nil
   :type (simple-array (unsigned-byte 8) (*)))
  (position
   0
   :type array-index)
  (has-identity-function
   nil
   :type (or null function))
  (circularity-map
   nil
   :type (or null hash-table))
  #+nil
  (last-referenced-object-position
   nil
   :type (or null fixnum)))

;;;;;;;;;
;;; Utils

(def (function io) object-code (object)
  (cond ((eq object nil)
         +nil+)
        ((eq object t)
         +t+)
        (t
         ;; TODO: build this automatically from def serializers
         (etypecase object
           (cons +cons+)
           (symbol +symbol+)
           (integer +integer+)
           (simple-base-string +simple-base-string+)
           (string +string+)
           (package +package+)
           (standard-object +standard-object+)))))

(def (function io) circularity-map (context)
  (or (sc-circularity-map context)
      (setf (sc-circularity-map context)
            (make-hash-table :test 'eq))))

(def (function o) default-has-identity-function (object)
  (and (not (null object))
       (typep object '(or symbol package standard-object))))

(def (function io) unread-unsigned-byte-8 (context)
  (decf (sc-position context)))

(def macro ensure-simple-vector-size (vector size)
  `(let ((length (length ,vector)))
    (when (< length ,size)
      (let ((new-vector (make-array (max (1+ (* 2 length)) ,size) :element-type '(unsigned-byte 8))))
        (replace new-vector ,vector)
        (setf ,vector new-vector)))))

(def (function o) analyze-list (list)
  "Returns two values.  The first value is one of :PROPER-LIST,
:DOTTED-LIST or :CIRCULAR-LIST.  The second value is the length of
the list.  For dotted lists, the final item is included in the
length; for circular lists, the length is NIL."
  ;; This is an adapatation of the algorithm in the Hyperspec
  ;; (see LIST-LENGTH).
  (declare (type list list))
  (loop for n fixnum = 0 :then (the (integer 0 #.(- most-positive-fixnum 2)) (+ n 2))
        for fast = list :then (cddr fast)
        for slow = list :then (cdr slow)
        ;; If fast pointer hits the end, return the count.
        do (cond ((null fast)
                  (return (values :proper-list n)))
                 ((atom fast)
                  (return (values :dotted-list (the fixnum (1+ n)))))
                 ((null (cdr fast))
                  (return (values :proper-list (the fixnum (1+ n)))))
                 ((atom (cdr fast))
                  (return (values :dotted-list (the fixnum (+ n 2)))))
                 ;; If fast pointer eventually equals slow pointer,
                 ;;  then we must be stuck in a circular list.
                 ;; (A deeper property is the converse: if we are
                 ;;  stuck in a circular list, then eventually the
                 ;;  fast pointer will equal the slow pointer.
                 ;;  That fact justifies this implementation.)
                 ((and (eq fast slow) (> n 0))
                  (return (values :circular-list 0))))))

(def macro format-log (format-specifier &rest args)
  (if cl-serializer-system:*load-with-debug-p*
      `(unless
        (ignore-errors
          (format t ,format-specifier ,@args)
          t)
        (format t "~%Error during formatting ~S" ,format-specifier))
      (values)))

;;;;;;;;;;;;;
;;; Serialize

(def (function o) serialize (object &key output (has-identity-function #'default-has-identity-function))
  (let ((context (make-serializer-context :buffer (make-array 256 :element-type '(unsigned-byte 8))
                                          :has-identity-function has-identity-function)))
    (write-variable-length-positive-integer +version+ context)
    (serialize-element object context)
    (etypecase output
      (stream (write-sequence (sc-buffer context) output :end (sc-position context)))
      (null (let ((final-vector (make-array (sc-position context) :element-type '(unsigned-byte 8))))
              (replace final-vector (sc-buffer context))
              final-vector)))))

(def (function o) serialize-element (object context)
  (declare (type serializer-context context))
  (serialize-element-with-code (object-code object) object context))

(def (function o) serialize-element-with-code (code object context)
  (declare (type serializer-context context)
           (type fixnum code))
  (if (funcall (the function (sc-has-identity-function context)) object)
      (let* ((circularity-map (circularity-map context))
             (position (gethash object circularity-map)))
        (if position
            (let* ((buffer (sc-buffer context))
                   (code (aref buffer position)))
              (setf (logbitp +referenced-bit-marker-index+ code) 1)
              (setf (aref buffer position) code)
              (format-log "~%Serializing reference at ~A to object ~S seen at ~A" (sc-position context) object position)
              (write-unsigned-byte-8 +reference+ context)
              (write-variable-length-positive-integer position context))
            (progn
              (setf (gethash object circularity-map) (sc-position context))
              (write-unsigned-byte-8 code context)
              (funcall (the function (aref +serializers+ code)) object context))))
      (progn
        (write-unsigned-byte-8 code context)
        (funcall (the function (aref +serializers+ code)) object context))))

;;;;;;;;;;;;;;;
;;; Deserialize

(def (function o) deserialize (input)
  (let ((context
         (etypecase input
           (array
            (make-serializer-context :buffer (coerce input '(simple-array (unsigned-byte 8) (*)))))
           (stream
            (make-serializer-context :buffer (read-stream-into-vector input))))))
    (unless (= +version+ (read-variable-length-positive-integer context))
      (error "Serializer version mismatch"))
    (deserialize-element context)))

(def (function o) deserialize-element (context)
  (declare (type serializer-context context))
  (let* ((code-with-referenced-bit (read-unsigned-byte-8 context))
         (code (logand code-with-referenced-bit +code-mask+)))
    (if (eq code +reference+)
        (let* ((reference-position (sc-position context))
               (position (read-variable-length-positive-integer context))
               (object
                (gethash position (circularity-map context) :not-found)))
          (declare (ignorable reference-position))
          (format-log "~%Deserializing reference at ~A to object ~S at ~A" (1- reference-position) object position)
          (when (eq object :not-found)
            (error "Reference to ~A cannot be resolved, byte at that position is ~A"
                   position (aref (sc-buffer context) position)))
          object)
        (funcall (the function (aref +deserializers+ code)) context))))

(def (function io) notify-identity (object position context)
  (when (logbitp +referenced-bit-marker-index+ (aref (sc-buffer context) position))
    (format-log "~%Storing referenced object ~A seen at ~A" object position)
    (setf (gethash position (circularity-map context)) object))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serializers and deserializers

(def definer serializer-deserializer (name code type serializer-form deserializer-form)
  (let ((writer-name (concatenate-symbol *package* "write-" name))
        (reader-name (concatenate-symbol *package* "read-" name)))
    `(progn
      (def (function io) ,writer-name (object context)
        (declare (ignorable object context)
                 (type ,type object)
                 (type serializer-context context))
        ,serializer-form
        (values))
      (def (function io) ,reader-name (context)
        (declare (ignorable context)
                 (type serializer-context context))
        (the ,type
          (values ,deserializer-form)))
      ,@(when code
              `((def (function io) ,(concatenate-symbol *package* "serialize-" name) (object context)
                  (declare (type ,type object)
                           (type serializer-context context))
                  (serialize-element-with-code ,code object context)
                  (values))
                (def (function io) ,(concatenate-symbol *package* "deserialize-" name) (context)
                  (declare (type serializer-context context))
                  (the ,type
                    (values (deserialize-element context))))
                (setf (aref +serializers+ ,code) #',writer-name)
                (setf (aref +deserializers+ ,code) #',reader-name))))))

(def serializer-deserializer nil +nil+ null
  nil
  nil)

(def serializer-deserializer t +t+ t
  t
  t)

(def serializer-deserializer unsigned-byte-8 nil (unsigned-byte 8)
  (progn
    (ensure-simple-vector-size (sc-buffer context) (the fixnum (1+ (sc-position context))))
    (setf (aref (sc-buffer context) (1- (the array-index (incf (sc-position context))))) object))
  (aref (sc-buffer context) (1- (the array-index (incf (sc-position context))))))

(def serializer-deserializer unsigned-byte-32 nil (unsigned-byte 32)
  (progn
    (write-unsigned-byte-8 (ldb (byte 8 24) object) context)
    (write-unsigned-byte-8 (ldb (byte 8 16) object) context)
    (write-unsigned-byte-8 (ldb (byte 8 8) object) context)
    (write-unsigned-byte-8 (ldb (byte 8 0) object) context))
  (let ((object 0))
    (declare (type (unsigned-byte 32) object))
    (setf (ldb (byte 8 24) object) (read-unsigned-byte-8 context))
    (setf (ldb (byte 8 16) object) (read-unsigned-byte-8 context))
    (setf (ldb (byte 8 8) object) (read-unsigned-byte-8 context))
    (setf (ldb (byte 8 0) object) (read-unsigned-byte-8 context))
    object))

(def constant +integer-length-mask+ #x7F)

(def constant +integer-length-sign-bit-index+ #x07)

(def serializer-deserializer integer nil integer
  (let* ((negative (< object 0))
         (integer (if negative
                      (lognot object)
                      object))
         (length (ceiling (integer-length (abs integer)) 8))
         (length-with-sign-bit length))
    (when negative
      (setf (logbitp +integer-length-sign-bit-index+ length-with-sign-bit) 1))
    (write-unsigned-byte-8 length-with-sign-bit context)
    (loop for index :from (- (* 8 length) 8) :downto 0 :by 8
          do (write-unsigned-byte-8 (ldb (byte 8 index) integer) context)))
  (let* ((first-byte (read-unsigned-byte-8 context))
         (negative (logbitp +integer-length-sign-bit-index+ first-byte))
         (length (* 8 (logand first-byte +integer-length-mask+))))
    (let ((object 0))
      (loop for index :from (- length 8) :downto 0 :by 8
            do (setf (ldb (byte 8 index) object) (read-unsigned-byte-8 context)))
      (if negative
          (lognot object)
          object))))

(def constant +short-positive-integer+ #x7F)

(def serializer-deserializer variable-length-positive-integer nil integer
  (with-fixnum-fast-path object
    (if (<= object +short-positive-integer+)
        (write-unsigned-byte-8 object context)
        (write-integer (- object) context)))
  (let ((first-byte (read-unsigned-byte-8 context)))
    (if (logbitp +integer-length-sign-bit-index+ first-byte)
        (progn
          (unread-unsigned-byte-8 context)
          (- (the fixnum (read-integer context))))
        first-byte)))

(def serializer-deserializer octets nil (simple-array (unsigned-byte 8) (*))
  (progn
    (write-variable-length-positive-integer (length object) context)
    (loop for octet :across object
          do (write-unsigned-byte-8 octet context)))
  (let* ((length (read-variable-length-positive-integer context))
         (object (make-array length :element-type '(unsigned-byte 8))))
    (loop for index :from 0 :below length
          do (setf (aref object index) (read-unsigned-byte-8 context)))
    object))

(def constant +utf-8-mapping+ (babel::lookup-mapping babel::*string-vector-mappings* :utf-8))

(def serializer-deserializer simple-base-string +simple-base-string+ simple-base-string
  (progn
    (write-variable-length-positive-integer (length object) context)
    (loop for character :across object
          do (write-unsigned-byte-8 (char-code character) context)))
  (let* ((length (the fixnum (read-variable-length-positive-integer context)))
         (string (make-array length :element-type 'base-char)))
    (loop for index :from 0 :below length
          do (setf (aref string index) (code-char (read-unsigned-byte-8 context))))
    string))

(def serializer-deserializer string +string+ string
  (let* ((length (length object))
         (encoded-length (funcall (the function (babel::octet-counter +utf-8-mapping+)) object 0 length -1)))
    (write-variable-length-positive-integer encoded-length context)
    (let* ((position (sc-position context)))
      (ensure-simple-vector-size (sc-buffer context) (+ 1 position encoded-length))
      (funcall (the function (babel::encoder +utf-8-mapping+)) object 0 length (sc-buffer context) position)
      (incf (sc-position context) encoded-length)))
  (let* ((length (read-variable-length-positive-integer context))
         (start (sc-position context))
         (end (the fixnum (+ start length)))
         (buffer (sc-buffer context)))
    (multiple-value-bind (size new-end)
        (funcall (the function (babel::code-point-counter +utf-8-mapping+)) buffer start end -1)
      (let ((string (make-string size :element-type 'unicode-char)))
        (funcall (the function (babel::decoder +utf-8-mapping+)) buffer start new-end string 0)
        (incf (sc-position context) length)
        string))))

(def serializer-deserializer generic-string nil string
  (etypecase object
    (simple-base-string
     (write-unsigned-byte-8 +simple-base-string+ context)
     (write-simple-base-string object context))
    (string
     (write-unsigned-byte-8 +string+ context)
     (write-string object context)))
  (ecase (read-unsigned-byte-8 context)
    (#.+simple-base-string+
     (read-simple-base-string context))
    (#.+string+
     (read-string context))))

(def serializer-deserializer symbol +symbol+ symbol
  (progn
    (write-generic-string (symbol-name object) context)
    (serialize-package (symbol-package object) context))
  (let ((position (1- (sc-position context))))
    (notify-identity (intern (read-generic-string context)
                             (deserialize-package context))
                     position context)))

(def serializer-deserializer package +package+ package
  (write-generic-string (package-name object) context)
  (let ((position (1- (sc-position context))))
    (notify-identity (find-package (read-generic-string context))
                     position context)))

(def serializer-deserializer cons +cons+ cons
  ;; TODO: move to generic dispatch into serializer
  (multiple-value-bind (type length)
      (analyze-list object)
    (cond ((eq type :proper-list)
           ;; TODO: push down length
           (write-proper-list object context))
          ((eq type :dotted-list)
           ;; TODO: push down length
           (write-dotted-list object context))
          (t
           (serialize-element (car object) context)
           (serialize-element (cdr object) context))))
  (cons (deserialize-element context) (deserialize-element context))
  #+nil ;; if we ever want to keep cons identities
  (let ((cons (cons nil nil)))
    (notify-identity cons context)
    (setf (car cons) (deserialize-element context))
    (setf (cdr cons) (deserialize-element context))
    cons))

(def serializer-deserializer proper-list +proper-list+ list
  (progn
    (unread-unsigned-byte-8 context)
    (write-unsigned-byte-8 +proper-list+ context)
    ;; TODO: push down length from serialize-cons
    (write-variable-length-positive-integer (length object) context)
    (dolist (element object)
      (serialize-element element context)))
  (let ((length (read-unsigned-byte-8 context)))
    (loop for index :from 0 :below length
          collect (deserialize-element context))))

(def serializer-deserializer dotted-list +dotted-list+ list
  (progn
    (unread-unsigned-byte-8 context)
    (write-unsigned-byte-8 +dotted-list+ context)
    (write-variable-length-positive-integer (length object) context)
    (dolist (element object)
      (serialize-element element context)))
  (let ((length (read-variable-length-positive-integer context)))
    (loop for index :from 0 :below length
          collect (deserialize-element context))))

(def serializer-deserializer standard-object +standard-object+ standard-object
  (let* ((class (class-of object))
         (slots (closer-mop:class-slots class)))
    (declare (type list slots))
    (serialize-symbol (class-name class) context)
    (write-variable-length-positive-integer (length slots) context)
    (dolist (slot slots)
      (unless (eq (closer-mop:slot-definition-allocation slot) :class)
        (serialize-symbol (closer-mop:slot-definition-name slot) context)
        (if (closer-mop:slot-boundp-using-class class object slot)
            (serialize-element (closer-mop:slot-value-using-class class object slot) context)
            (write-unsigned-byte-8 +unbound-slot+ context)))))
  (let* ((position (1- (sc-position context)))
         (class-name (deserialize-symbol context))
         (class (find-class class-name))
         (object (allocate-instance class)))
    (notify-identity object position context)
    (loop repeat (the fixnum (read-variable-length-positive-integer context))
      for slot-name = (deserialize-symbol context) do
      (if (eq +unbound-slot+ (read-unsigned-byte-8 context))
          (slot-makunbound object slot-name)
          (setf (slot-value object slot-name)
                (progn
                  (unread-unsigned-byte-8 context)
                  (deserialize-element context)))))
    object))
