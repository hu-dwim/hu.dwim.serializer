;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.serializer)

;;;;;;
;;; Special

(def constant +nil-code+                         #x00)

(def constant +t-code+                           #x01)

(def constant +cons-code+                        #x02)

(def constant +proper-list-code+                 #x03)

(def constant +dotted-list-code+                 #x04)

;;;;;;
;;; String

(def constant +base-char-code+                   #x10)

(def constant +extended-char-code+               #x11)

(def constant +character-code+                   #x12)

(def constant +base-string-code+                 #x13)

(def constant +simple-base-string-code+          #x14)

(def constant +simple-string-code+               #x15)

(def constant +string-code+                      #x16)

;;;;;;
;;; Symbol

(def constant +symbol-code+                      #x20)

(def constant +keyword-code+                     #x21)

(def constant +uninterned-symbol-code+           #x22)

(def constant +package-code+                     #x23)

;;;;;;
;;; Number

(def constant +integer-code+                     #x30)

(def constant +rational-code+                    #x31)

(def constant +float-code+                       #x32)

(def constant +short-float-code+                 #x33)

(def constant +single-float-code+                #x34)

(def constant +double-float-code+                #x35)

(def constant +long-float-code+                  #x36)

(def constant +complex-code+                     #x37)

(def constant +number-code+                      #x38)

;;;;;;
;;; Compound

(def constant +simple-vector-code+               #x40)

(def constant +simple-array-code+                #x41)

(def constant +vector-code+                      #x42)

(def constant +array-code+                       #x43)

(def constant +simple-bit-vector-code+           #x44)

(def constant +bit-vector-code+                  #x45)

(def constant +hash-table-code+                  #x46)

(def constant +pathname-code+                    #x47)

(def constant +simple-unsigned-byte-8-vector-code+ #x48)

;;;;;;
;;; Object

(def constant +structure-object-code+            #x50)

(def constant +standard-object-code+             #x51)

(def constant +unbound-slot-code+                #x52)

(def constant +standard-class-code+              #x53)

(def constant +standard-direct-slot-definition-code+ #x54)

(def constant +standard-effective-slot-definition-code+ #x55)

;;;;;;
;;; Reserved

(def constant +first-reserved-code+              #x60)

(def constant +last-reserved-code+               #x6F)

;;;;;;
;;; Reference

(def constant +reference-code+                   #x7F)

(def constant +referenced-bit-marker-index+      #x07)

(def constant +code-mask+                        #x7F)

;;;;;;
;;; Code -> lambda

(declaim (type (simple-vector 128) +writers+ +readers+))

(def load-time-constant +writers+ (make-array 128))

(def load-time-constant +readers+ (make-array 128))

;;;;;;
;;; Context

(def constant +version+ 0)

(defstruct (serializer-context (:conc-name sc-))
  (buffer
   nil
   :type (simple-array (unsigned-byte 8) (*)))
  (position
   0
   :type array-index)
  (mapper
   nil
   :type function)
  (identity-map
   nil
   :type (or null hash-table))
  (list-length
   nil
   :type (or null fixnum)))

(def print-object serializer-context ()
  (princ (sc-position -self-)))

(def special-variable *deserialize-element-position*)

;;;;;;
;;; Util

(def (function io) identity-map (context)
  (or (sc-identity-map context)
      (setf (sc-identity-map context)
            (make-hash-table :test 'eq))))

(def (function io) position-to-identity-map (context)
  (identity-map context))

(def (function io) identity-to-position-map (context)
  (identity-map context))

(def (function o) default-serializer-mapper (object context)
  "Returns (values TYPE-CODE HAS-IDENTITY WRITER-FUNCTION), where TYPE-CODE is the (unsigned-byte 8) code that identifies the object's type in the serialized output; HAS-IDENTITY is a boolean telling the engine whether to keep the object's identity through a serialize-deserialize (which is a performance overhead); and WRITER-FUNCTION is called to do the serialization after the type code has been written."
  (declare (ignore context))
  (the (values fixnum boolean function)
    (flet ((local-return (code identity)
             (values code identity (the function (aref +writers+ code)))))
      (cond ((eq object nil)
             (local-return +nil-code+ #f))
            ((eq object t)
             (local-return +t-code+ #f))
            (t
             #. ;; we need to do this at read-time, because etypecase doesn't tolerate NIL entries
             `(etypecase object
                (cons (local-return +cons-code+ #t))
                (keyword (local-return +keyword-code+ #t))
                (symbol (if (symbol-package object)
                            (local-return +symbol-code+ #t)
                            (local-return +uninterned-symbol-code+ #t)))
                (integer (local-return +integer-code+ #f))
                (rational (local-return +rational-code+ #f))
                (float (local-return +float-code+ #f))
                (complex (local-return +complex-code+ #f))
                ,@(unless (subtypep 'character 'base-char)
                    `((base-char (local-return +base-char-code+ #f))))
                (character (local-return +character-code+ #f))
                ,@(unless (subtypep 'string 'base-string)
                    `((simple-base-string (local-return +simple-base-string-code+ #t))))
                (simple-string (local-return +simple-string-code+ #t))
                (string (local-return +string-code+ #t))
                (pathname (local-return +pathname-code+ #t))
                (package (local-return +package-code+ #t))
                (simple-unsigned-byte-8-vector (local-return +simple-unsigned-byte-8-vector-code+ #t))
                (simple-vector (local-return +simple-vector-code+ #t))
                (simple-array (local-return +simple-array-code+ #t))
                (vector (local-return +vector-code+ #t))
                (array (local-return +array-code+ #t))
                (hash-table (local-return +hash-table-code+ #t))
                (standard-class (local-return +standard-class-code+ #t))
                (closer-mop:standard-direct-slot-definition (local-return +standard-direct-slot-definition-code+ #t))
                (closer-mop:standard-effective-slot-definition (local-return +standard-effective-slot-definition-code+ #t))
                (structure-object (local-return +structure-object-code+ #t))
                (standard-object (values +standard-object-code+ #t #'write-object)
                                 ;; inserting the following here would disable the READ/WRITE-OBJECT-SLOTS generics: (local-return +standard-object-code+ #t)
                                 )))))))

(def (function o) default-deserializer-mapper (code context)
  (declare (ignore context))
  (the function
    (if (eql code +standard-object-code+) ; dropping this special case would disable the READ/WRITE-OBJECT-SLOTS generics
        #'read-object
        (aref +readers+ code))))

(def (function io) unread-unsigned-byte-8 (context)
  (decf (sc-position context)))

;;;;;;
;;; Serialize

(def (function oe) serialize (object &key (output nil) (buffer-size 1024) (serializer-mapper #'default-serializer-mapper))
  (log.debug "Serialization of ~A starts" object)
  (bind ((context (make-serializer-context :buffer (make-array buffer-size :element-type '(unsigned-byte 8))
                                           :mapper serializer-mapper)))
    (write-variable-length-positive-integer +version+ context)
    (serialize-element object context)
    (log.debug "Serialization of ~A finished at ~A" object (sc-position context))
    (etypecase output
      (stream (write-sequence (sc-buffer context) output :end (sc-position context)))
      (null (aprog1
                (make-array (sc-position context) :element-type '(unsigned-byte 8))
              (replace it (sc-buffer context)))))))

(def (function o) serialize-element (object context)
  (declare (type serializer-context context))
  (bind (((:values type-code has-identity writer-function) (funcall (sc-mapper context) object context)))
    (check-type type-code (unsigned-byte 8))
    (check-type has-identity boolean)
    (check-type writer-function function)
    (when has-identity
      (bind ((identity-to-position-map (identity-to-position-map context))
             (position (gethash object identity-to-position-map)))
        (if position
            (bind ((buffer (sc-buffer context))
                   (code (aref buffer position)))
              (setf (bit-value +referenced-bit-marker-index+ code) #t)
              (setf (aref buffer position) code)
              (log.debug "Serializing reference at ~A to object ~S first seen at ~A" (sc-position context) object position)
              (write-unsigned-byte-8 +reference-code+ context)
              (write-variable-length-positive-integer position context)
              (return-from serialize-element (values)))
            (progn
              (log.debug "Registering identity of ~A (of type ~S) first seen at ~A" object (type-of object) (sc-position context))
              (setf (gethash object identity-to-position-map) (sc-position context))))))
    (write-unsigned-byte-8 type-code context)
    (log.debug "SERIALIZE-ELEMENT calling the writer function ~A" writer-function)
    (funcall writer-function object context)
    (values)))

;;;;;;
;;; Deserialize

(def (function oe) deserialize (input &key (deserializer-mapper #'default-deserializer-mapper))
  (log.debug "Deserialization starts")
  (bind ((context (make-serializer-context
                   :mapper deserializer-mapper
                   :buffer (etypecase input
                             (array
                              (coerce input '(simple-array (unsigned-byte 8) (*))))
                             (stream
                              (read-stream-into-vector input))))))
    (unless (= +version+ (read-variable-length-positive-integer context))
      (error "Serializer version mismatch"))
    (prog1
        (deserialize-element context)
      (log.debug "Deserialization finished"))))

(def (function o) deserialize-element (context)
  (check-type context serializer-context)
  (bind ((*deserialize-element-position* (sc-position context))
         (code-with-referenced-bit (read-unsigned-byte-8 context))
         (referenced? (bit-value +referenced-bit-marker-index+ code-with-referenced-bit))
         (code (logand code-with-referenced-bit +code-mask+)))
    (if (eq code +reference-code+)
        (bind ((position (read-variable-length-positive-integer context))
               (result (gethash position (position-to-identity-map context) :not-found)))
          (log.debug "Deserializing reference at ~A to object ~S first seen at ~A" *deserialize-element-position* result position)
          (when (eq result :not-found)
            (error "Reference to ~A cannot be resolved, byte at that position is ~A"
                   position (aref (sc-buffer context) position)))
          result)
        (bind ((reader-function (funcall (sc-mapper context) code context)))
          (log.debug "DESERIALIZE-ELEMENT calling the reader function ~A" reader-function)
          (funcall reader-function context referenced?)))))

(def (function io) announce-identity (object context)
  (log.debug "Registering identity of ~A (of type ~S) seen at ~A" object (type-of object) *deserialize-element-position*)
  (setf (gethash *deserialize-element-position* (position-to-identity-map context)) object)
  object)

;;;;;;
;;; Serializers and deserializers

(def definer serializer-deserializer (name code type serializer-form deserializer-form)
  (bind ((writer-name (symbolicate '#:write- name))
         (reader-name (symbolicate '#:read- name)))
    `(progn
      (def (function io) ,writer-name (-object- -context-)
        (declare (ignorable -object- -context-))
        (check-type -object- ,type)
        (check-type -context- serializer-context)
        ,serializer-form
        (values))
      (def (function io) ,reader-name (-context- &optional -referenced-)
        (declare (ignorable -context- -referenced-))
        (check-type -context- serializer-context)
        (the ,type
          (values ,deserializer-form)))
      ,@(when code
              `((def (function io) ,(symbolicate '#:serialize- name) (object context)
                  (check-type context serializer-context)
                  (check-type object ,type)
                  (serialize-element object context)
                  (values))
                (def (function io) ,(symbolicate '#:deserialize- name) (context)
                  (check-type context serializer-context)
                  (the ,type
                    (values (deserialize-element context))))
                (setf (aref +writers+ ,code) #',writer-name)
                (setf (aref +readers+ ,code) #',reader-name))))))

(def serializer-deserializer nil +nil-code+ null
  nil
  nil)

(def serializer-deserializer t +t-code+ t
  t
  t)

(def serializer-deserializer unsigned-byte-8 nil (unsigned-byte 8)
  (progn
    (ensure-simple-vector-size (sc-buffer -context-) (the fixnum (1+ (sc-position -context-))))
    (setf (aref (sc-buffer -context-) (1- (the array-index (incf (sc-position -context-))))) -object-))
  (aref (sc-buffer -context-) (1- (the array-index (incf (sc-position -context-))))))

(def serializer-deserializer unsigned-byte-32 nil (unsigned-byte 32)
  (progn
    (write-unsigned-byte-8 (ldb (byte 8 24) -object-) -context-)
    (write-unsigned-byte-8 (ldb (byte 8 16) -object-) -context-)
    (write-unsigned-byte-8 (ldb (byte 8 8) -object-) -context-)
    (write-unsigned-byte-8 (ldb (byte 8 0) -object-) -context-))
  (bind ((-object- 0))
    (declare (type (unsigned-byte 32) -object-))
    (setf (ldb (byte 8 24) -object-) (read-unsigned-byte-8 -context-))
    (setf (ldb (byte 8 16) -object-) (read-unsigned-byte-8 -context-))
    (setf (ldb (byte 8 8) -object-) (read-unsigned-byte-8 -context-))
    (setf (ldb (byte 8 0) -object-) (read-unsigned-byte-8 -context-))
    -object-))

(def constant +integer-length-mask+ #x7F)

(def constant +integer-length-sign-bit-index+ #x07)

;; TODO: specialize on fixnums
(def serializer-deserializer integer +integer-code+ integer
  (bind ((negative (< -object- 0))
         (integer (if negative
                      (lognot -object-)
                      -object-))
         (length (ceiling (integer-length (abs integer)) 8))
         (length-with-sign-bit length))
    (when negative
      (setf (bit-value +integer-length-sign-bit-index+ length-with-sign-bit) #t))
    (write-unsigned-byte-8 length-with-sign-bit -context-)
    (loop
      :for index :from (- (* 8 length) 8) :downto 0 :by 8
      :do (write-unsigned-byte-8 (ldb (byte 8 index) integer) -context-)))
  (bind ((first-byte (read-unsigned-byte-8 -context-))
         (negative? (bit-value +integer-length-sign-bit-index+ first-byte))
         (length (* 8 (logand first-byte +integer-length-mask+))))
    (bind ((result 0))
      (loop
        :for index :from (- length 8) :downto 0 :by 8
        :do (setf (ldb (byte 8 index) result) (read-unsigned-byte-8 -context-)))
      (if negative?
          (lognot result)
          result))))

(def constant +short-positive-integer+ #x7F)

;; TODO: specialize on fixnums
(def serializer-deserializer variable-length-positive-integer nil integer
  (if (<= -object- +short-positive-integer+)
      (write-unsigned-byte-8 -object- -context-)
      (write-integer (- -object-) -context-))
  (bind ((first-byte (read-unsigned-byte-8 -context-)))
    (if (bit-value +integer-length-sign-bit-index+ first-byte)
        (progn
          (unread-unsigned-byte-8 -context-)
          (- (the fixnum (read-integer -context-))))
        first-byte)))

(def serializer-deserializer rational +rational-code+ rational
  (progn
    (write-integer (numerator -object-) -context-)
    (write-integer (denominator -object-) -context-))
  (/ (read-integer -context-) (read-integer -context-)))

(def serializer-deserializer float +float-code+ float
  (bind (((:values significand exponent sign)
          (integer-decode-float -object-)))
    (write-integer significand -context-)
    (write-integer exponent -context-)
    (write-unsigned-byte-8 (1+ sign) -context-))
  (bind ((significand (read-integer -context-))
         (exponent (read-integer -context-))
         (sign (1- (read-unsigned-byte-8 -context-))))
    (* sign (scale-float (float significand 1.0L0) exponent))))

(def serializer-deserializer complex +complex-code+ complex
  (progn
    (write-float (realpart -object-) -context-)
    (write-float (imagpart -object-) -context-))
  (complex (read-float -context-) (read-float -context-)))

(def serializer-deserializer simple-unsigned-byte-8-vector +simple-unsigned-byte-8-vector-code+ simple-unsigned-byte-8-vector
  (progn
    (write-variable-length-positive-integer (length -object-) -context-)
    (loop
      :for octet :across -object-
      :do (write-unsigned-byte-8 octet -context-)))
  (bind ((length (read-variable-length-positive-integer -context-))
         (result (make-array length :element-type '(unsigned-byte 8))))
    (loop
      :for index :from 0 :below length
      :do (setf (aref result index) (read-unsigned-byte-8 -context-)))
    result))

(def serializer-deserializer character +character-code+ character
  (write-integer (char-code -object-) -context-)
  (code-char (read-integer -context-)))

(def serializer-deserializer extended-char +extended-char-code+ character
  (write-integer (char-code -object-) -context-)
  (code-char (read-integer -context-)))

(def serializer-deserializer base-char +base-char-code+ character
  (write-unsigned-byte-8 (char-code -object-) -context-)
  (code-char (read-unsigned-byte-8 -context-)))

(define-symbol-macro +utf-8-mapping+ (load-time-value (babel::lookup-mapping babel::*string-vector-mappings* :utf-8)))

(def serializer-deserializer simple-base-string +simple-base-string-code+ simple-base-string
  (progn
    (write-variable-length-positive-integer (length -object-) -context-)
    (loop
      :for character :across -object-
      :do (write-unsigned-byte-8 (char-code character) -context-)))
  (bind ((length (the fixnum (read-variable-length-positive-integer -context-)))
         (string (make-array length :element-type 'base-char)))
    (loop
      :for index :from 0 :below length
      :do (setf (aref string index) (code-char (read-unsigned-byte-8 -context-))))
    (announce-identity string -context-)))

(def serializer-deserializer simple-string +simple-string-code+ string
  (bind ((length (length -object-))
         (encoded-length (funcall (the function (babel::octet-counter +utf-8-mapping+)) -object- 0 length -1)))
    (declare (type fixnum encoded-length))
    (write-variable-length-positive-integer encoded-length -context-)
    (bind ((position (sc-position -context-)))
      (ensure-simple-vector-size (sc-buffer -context-) (+ 1 position encoded-length))
      (funcall (the function (babel::encoder +utf-8-mapping+)) -object- 0 length (sc-buffer -context-) position)
      (incf (sc-position -context-) encoded-length)))
  (bind ((length (read-variable-length-positive-integer -context-))
         (start (sc-position -context-))
         (end (the fixnum (+ start length)))
         (buffer (sc-buffer -context-))
         ((:values size new-end) (funcall (the function (babel::code-point-counter +utf-8-mapping+)) buffer start end -1))
         (string (make-string size :element-type 'unicode-char)))
    (declare (type fixnum length start end))
    (funcall (the function (babel::decoder +utf-8-mapping+)) buffer start new-end string 0)
    (incf (sc-position -context-) length)
    (announce-identity string -context-)))

(def serializer-deserializer string +string-code+ string
  ;; TODO: this is generating garbage on the heap
  (write-simple-string (coerce -object- 'simple-string) -context-)
  (coerce (read-simple-string -context-) 'string))

(def serializer-deserializer generic-string nil string
  (etypecase -object-
    (simple-base-string
     (write-unsigned-byte-8 +simple-base-string-code+ -context-)
     (write-simple-base-string -object- -context-))
    (simple-string
     (write-unsigned-byte-8 +simple-string-code+ -context-)
     (write-simple-string -object- -context-))
    (string
     (write-unsigned-byte-8 +string-code+ -context-)
     (write-string -object- -context-)))
  (ecase (read-unsigned-byte-8 -context-)
    (#.+simple-base-string-code+
     (read-simple-base-string -context-))
    (#.+simple-string-code+
     (read-simple-string -context-))
    (#.+string-code+
     (read-string -context-))))

(def serializer-deserializer pathname +pathname-code+ pathname
  (write-simple-string (namestring -object-) -context-)
  (pathname (read-simple-string -context-)))

(def serializer-deserializer keyword +keyword-code+ keyword
  (write-generic-string (symbol-name -object-) -context-)
  (announce-identity (intern (read-generic-string -context-) :keyword) -context-))

(def serializer-deserializer symbol +symbol-code+ symbol
  (progn
    (write-generic-string (symbol-name -object-) -context-)
    (serialize-package (symbol-package -object-) -context-))
  (announce-identity (intern (read-generic-string -context-)
                             (deserialize-package -context-)) -context-))

(def serializer-deserializer uninterned-symbol +uninterned-symbol-code+ symbol
  (write-generic-string (symbol-name -object-) -context-)
  (announce-identity (make-symbol (read-generic-string -context-)) -context-))

(def serializer-deserializer package +package-code+ package
  (write-generic-string (package-name -object-) -context-)
  (bind ((package-name (read-generic-string -context-))
         (package (find-package package-name)))
    (unless package
      (restart-case
          (error "Cannot find package ~S while deserializing" package-name)
        (use-current-package ()
          :report (lambda (stream)
                    (format stream "Return *package* (~A) instead of ~S (not advised, use only when debugging)" *package* package-name))
          (setf package *package*))))
    (announce-identity package -context-)))

(def serializer-deserializer cons +cons-code+ cons
  (progn
    (serialize-element (car -object-) -context-)
    (serialize-element (cdr -object-) -context-))
  (bind ((result (cons nil nil)))
    (announce-identity result -context-)
    (setf (car result) (deserialize-element -context-))
    (setf (cdr result) (deserialize-element -context-))
    result))

(def serializer-deserializer proper-list +proper-list-code+ list
  (progn
    (unread-unsigned-byte-8 -context-)
    (write-unsigned-byte-8 +proper-list-code+ -context-)
    (write-variable-length-positive-integer (sc-list-length -context-) -context-)
    (dolist (element -object-)
      (serialize-element element -context-)))
  (bind ((length (read-variable-length-positive-integer -context-)))
    (loop
      :for index :from 0 :below length
      :collect (deserialize-element -context-))))

(def serializer-deserializer dotted-list +dotted-list-code+ list
  (progn
    (unread-unsigned-byte-8 -context-)
    (write-unsigned-byte-8 +dotted-list-code+ -context-)
    (write-variable-length-positive-integer (sc-list-length -context-) -context-)
    (loop
      :for cons :on -object-
      :do (serialize-element (car cons) -context-)
      :finally (serialize-element cons -context-)))
  (bind ((length (read-variable-length-positive-integer -context-))
         (result (loop
                   :repeat (1- length)
                   :collect (deserialize-element -context-))))
    (setf (cdr (last result)) (deserialize-element -context-))
    result))

(def (function io) %read-array (dimensions adjustable context)
  (bind ((element-type (deserialize-element context))
         (result (make-array dimensions :element-type element-type :adjustable adjustable)))
    (announce-identity result context)
    (loop
      :for index :from 0 :below (array-total-size result)
      :do (setf (row-major-aref result index) (deserialize-element context)))
    result))

(def (function io) %write-array (object context)
  (serialize-element (array-element-type object) context)
  (loop
    :for index :from 0 :below (array-total-size object)
    :do (serialize-element (row-major-aref object index) context)))

(def serializer-deserializer simple-vector +simple-vector-code+ simple-vector
  (progn
    (write-variable-length-positive-integer (length -object-) -context-)
    (%write-array -object- -context-))
  (%read-array (read-variable-length-positive-integer -context-) #f -context-))

(def serializer-deserializer simple-array +simple-array-code+ simple-array
  (progn
    (serialize-element (array-dimensions -object-) -context-)
    (%write-array -object- -context-))
  (%read-array (deserialize-element -context-) #f -context-))
  
(def serializer-deserializer vector +vector-code+ vector
  (progn
    (write-variable-length-positive-integer (length -object-) -context-)
    (%write-array -object- -context-))
  (%read-array (read-variable-length-positive-integer -context-) #t -context-))

(def serializer-deserializer array +array-code+ array
  (progn
    (serialize-element (array-dimensions -object-) -context-)
    (%write-array -object- -context-))
  (%read-array (deserialize-element -context-) #t -context-))

(def serializer-deserializer hash-table +hash-table-code+ hash-table
  (progn
    (write-symbol (hash-table-test -object-) -context-)
    (write-variable-length-positive-integer (the fixnum (hash-table-count -object-)) -context-)
    (maphash (lambda (key value)
               (serialize-element key -context-)
               (serialize-element value -context-))
             -object-))
  (bind ((result (make-hash-table :test (read-symbol -context-))))
    (loop
      :repeat (the fixnum (read-variable-length-positive-integer -context-))
      :do (setf (gethash (deserialize-element -context-) result) (deserialize-element -context-)))
    result))

;;;;;;
;;; CLOS objects
;;;
;;; .../primitive versions of the functions are not customizable through a generic method

(def function write-object (object context)
   (log.debug "WRITE-OBJECT of ~A at ~A" object (sc-position context))
   (bind ((class (class-of object)))
     (serialize-symbol (class-name class) context)
     (write-object-slots class object context)))

(def (generic e) write-object-slots (class object context)
  (:documentation "When using the DEFAULT-SERIALIZER-MAPPER, the writing of the slots of STANDARD-OBJECT's go through this generic, so that users can customize it. If you override WRITE-OBJECT-SLOTS, then make sure you also override READ-OBJECT-SLOTS!")
  (:method (class object context)
    (debug-only (assert (eq class (class-of object))))
    (log.debug "WRITE-OBJECT of ~A at ~A" object (sc-position context))
    (write-object-slots/primitive object context)))

(def function read-object (context &optional referenced?)
  (bind ((class-name (deserialize-symbol context))
         (class (find-class class-name))
         (prototype (closer-mop:class-prototype class)))
    (log.debug "READ-OBJECT on an instance of ~S seen at ~A" class-name *deserialize-element-position*)
    (aprog1
        (read-object-slots class prototype context :is-referenced referenced?)
      (log.debug "READ-OBJECT returnes ~A, now at ~A" it (sc-position context)))))

(def (generic e) read-object-slots (class prototype context &key is-referenced)
  (:documentation "When using the DEFAULT-DESERIALIZER-MAPPER, the reading of STANDARD-OBJECT's go through this generic, so that users can customize it. The PROTOTYPE argument may only be used for dispatching (it's the CLASS-PROTOTYPE of the class)! See also WRITE-OBJECT-SLOTS.")
  (:method (class prototype context &key &allow-other-keys)
    (debug-only (assert (eq class (class-of prototype))))
    (bind ((object (allocate-instance class)))
      (log.debug "READ-OBJECT of an instance of class ~S at ~S, slots are at ~A" (class-name class) *deserialize-element-position* (sc-position context))
      (announce-identity object context)
      (read-object-slots/primitive object context)
      object)))

(def serializer-deserializer standard-class +standard-class-code+ standard-class
  (write-symbol (class-name -object-) -context-)
  (announce-identity (find-class (read-symbol -context-)) -context-))

(def (function io) write-object-slots/primitive (object context &optional (slots (closer-mop:class-slots (class-of object))))
  (check-type slots list)
  (log.debug "WRITE-OBJECT-SLOTS/PRIMITIVE of ~A, slots: ~A, at ~A" object slots (sc-position context))
  (bind ((class (class-of object)))
    (write-variable-length-positive-integer (length slots) context)
    (dolist (slot slots)
      (unless (eq (ignore-errors (closer-mop:slot-definition-allocation slot)) :class) ;; TODO THL rewrite nicely (not defined/portable for structures)
        (serialize-symbol (closer-mop:slot-definition-name slot) context)
        (if (closer-mop:slot-boundp-using-class class object slot)
            (serialize-element (closer-mop:slot-value-using-class class object slot) context)
            (write-unsigned-byte-8 +unbound-slot-code+ context))))))

(def (function io) read-object-slots/primitive (object context)
  (log.debug "READ-OBJECT-SLOTS/PRIMITIVE of ~A at ~A" object (sc-position context))
  (loop
    :repeat (the fixnum (read-variable-length-positive-integer context))
    :for slot-name = (deserialize-symbol context)
    :do (if (eq +unbound-slot-code+ (read-unsigned-byte-8 context))
            (slot-makunbound object slot-name)
            (setf (slot-value object slot-name)
                  (progn
                    (unread-unsigned-byte-8 context)
                    (deserialize-element context)))))
  (values))

(def (function o) write-object/primitive (object context)
  (log.debug "WRITE-OBJECT/PRIMITIVE of ~A at ~A" object (sc-position context))
  (serialize-symbol (class-name (class-of object)) context)
  (write-object-slots/primitive object context))

(def (function o) read-object/primitive (context &key (allocator #'allocate-instance))
  (bind ((class-name (deserialize-symbol context))
         (class (find-class class-name)))
    (log.debug "READ-OBJECT/PRIMITIVE of an instance of class ~S at ~A" class-name *deserialize-element-position*)
    (bind ((object (funcall allocator class)))
      (announce-identity object context)
      (read-object-slots/primitive object context)
      object)))

(def serializer-deserializer structure-object/primitive +structure-object-code+ structure-object
  (write-object/primitive -object- -context-)
  (read-object/primitive -context-))

(def serializer-deserializer standard-object/primitive +standard-object-code+ standard-object
  (write-object/primitive -object- -context-)
  (read-object/primitive -context-))

(def serializer-deserializer closer-mop:standard-direct-slot-definition +standard-direct-slot-definition-code+ closer-mop:standard-direct-slot-definition
  (progn
    #*((:sbcl (write-symbol (class-name (slot-value -object- 'sb-pcl::%class)) -context-))
       (t (not-yet-implemented)))
    (write-symbol (closer-mop:slot-definition-name -object-) -context-))
  (announce-identity (find-direct-slot (read-symbol -context-) (read-symbol -context-)) -context-))

(def serializer-deserializer closer-mop:standard-effective-slot-definition +standard-effective-slot-definition-code+ closer-mop:standard-effective-slot-definition
  (progn
    (log.debug "WRITE-STANDARD-EFFECTIVE-SLOT-DEFINITION of ~A" -object-)
    #*((:sbcl (write-symbol (class-name (slot-value -object- 'sb-pcl::%class)) -context-))
       (t (not-yet-implemented)))
    (write-symbol (closer-mop:slot-definition-name -object-) -context-))
  (bind ((class-name (read-symbol -context-))
         (slot-name (read-symbol -context-)))
    (log.debug "READ-STANDARD-EFFECTIVE-SLOT-DEFINITION of a slot called ~S of class ~S" slot-name class-name)
    (announce-identity (find-slot class-name slot-name ) -context-)))
