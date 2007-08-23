;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-serializer)

;; TODO: use 1 bit from code to test whether circularity check is needed during runtime

;;;;;;;;;;;
;;; Special

(def constant +t+                     #x00)

(def constant +nil+                   #x01)

(def constant +cons+                  #x02)

;;;;;;;;;;
;;; String

(def constant +base-char+             #x10)

(def constant +extended-char+         #x11)

(def constant +character+             #x12)

(def constant +base-string+           #x13)

(def constant +simple-base-string+    #x14)

(def constant +simple-string+         #x15)

(def constant +string+                #x16)

;;;;;;;;;;
;;; Symbol

(def constant +keyword+               #x20)

(def constant +uninterned-symbol+     #x21)

(def constant +symbol+                #x22)

(def constant +package+               #x23)

;;;;;;;;;;
;;; Number

(def constant +zero+                  #x30)

(def constant +integer+               #x31)

(def constant +rational+              #x32)

(def constant +float+                 #x33)

(def constant +short-float+           #x34)

(def constant +single-float+          #x35)

(def constant +double-float+          #x36)

(def constant +long-float+            #x37)

(def constant +complex+               #x38)

;;;;;;;;;;;;
;;; Compound

(def constant +hash-table+            #x40)

(def constant +pathname+              #x41)

(def constant +simple-vector+         #x42)

(def constant +vector+                #x43)

(def constant +array+                 #x44)

(def constant +simple-bit-vector+     #x45)

(def constant +bit-vector+            #x46)

;;;;;;;;;;
;;; Object

(def constant +standard-object+       #x50)

(def constant +unbound-slot+          #x51)

(def constant +structure-object+      #x52)

;;;;;;;;;;;;;;;
;;; Circularity

(def constant +reference+             #x7F)

(def constant +referenced-bit-mask+   #x80)

(def constant +code-mask+             #x7F)

;;;;;;;;;;;;;;;;;;
;;; Code -> lambda

(def (constant :test 'equalp) +serializers+ (make-array 128))

(def (constant :test 'equalp) +deserializers+ (make-array 128))

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
  (last-referenced-object-position
   nil
   :type (or null fixnum)))

(def (function io) circularity-map (context)
  (or (sc-circularity-map context)
      (setf (sc-circularity-map context)
            (make-hash-table :test 'eq))))

;;;;;;;;;;;;;;;;;
;;; Reader writer

(def definer writer-reader (name type writer-form reader-form)
  `(progn
    (def (function io) ,(concatenate-symbol *package* "write-" name) (object context)
      (declare (type serializer-context context)
               (type ,type object))
      ,writer-form
      (values))
    (def (function io) ,(concatenate-symbol *package* "read-" name) (context)
      (declare (type serializer-context context))
      (the ,type
        (values ,reader-form)))))

(def macro ensure-simple-vector-size (vector size)
  `(let ((length (length ,vector)))
    (when (< length ,size)
      (let ((new-vector (make-array (max (1+ (* 2 length)) ,size) :element-type '(unsigned-byte 8))))
        (replace new-vector ,vector)
        (setf ,vector new-vector)))))

(def writer-reader unsigned-byte-8 (unsigned-byte 8)
  (progn
    (ensure-simple-vector-size (sc-buffer context) (the fixnum (1+ (sc-position context))))
    (setf (aref (sc-buffer context) (1- (the array-index (incf (sc-position context))))) object))
  (aref (sc-buffer context) (1- (the array-index (incf (sc-position context))))))

(def writer-reader unsigned-byte-32 (unsigned-byte 32)
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

(def writer-reader octets (simple-array (unsigned-byte 8) (*))
  (progn
    (write-unsigned-byte-32 (length object) context)
    (loop for octet :across object
          do (write-unsigned-byte-8 octet context)))
  (let* ((length (read-unsigned-byte-32 context))
         (object (make-array length :element-type '(unsigned-byte 8))))
    (loop for index :from 0 :below length
          do (setf (aref object index) (read-unsigned-byte-8 context)))
    object))

(def constant +utf-8-mapping+ (babel::lookup-mapping babel::*string-vector-mappings* :utf-8))

(def writer-reader string string
  (let* ((length (length object))
         (encoded-length (funcall (the function (babel::octet-counter +utf-8-mapping+)) object 0 length -1)))
    (write-unsigned-byte-32 encoded-length context)
    (let* ((position (sc-position context)))
      (ensure-simple-vector-size (sc-buffer context) (+ 1 position encoded-length))
      (funcall (the function (babel::encoder +utf-8-mapping+)) object 0 length (sc-buffer context) position)
      (incf (sc-position context) encoded-length)))
  (let* ((length (read-unsigned-byte-32 context))
         (start (sc-position context))
         (end (the fixnum (+ start length)))
         (buffer (sc-buffer context)))
    (multiple-value-bind (size new-end)
        (funcall (the function (babel::code-point-counter +utf-8-mapping+)) buffer start end -1)
      (let ((string (make-string size :element-type 'unicode-char)))
        (funcall (the function (babel::decoder +utf-8-mapping+)) buffer start new-end string 0)
        (incf (sc-position context) length)
        string))))

(def writer-reader simple-base-string simple-base-string
  (progn
    (write-unsigned-byte-32 (length object) context)
    (loop for character :across object
          do (write-unsigned-byte-8 (char-code character) context)))
  (let* ((length (the fixnum (read-unsigned-byte-32 context)))
         (string (make-array length :element-type 'base-char)))
    (loop for index :from 0 :below length
          do (setf (aref string index) (code-char (read-unsigned-byte-8 context))))
    string))

;; TODO: cleanup
(def writer-reader generic-string string
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

(def writer-reader symbol symbol
  (progn
    (write-generic-string (symbol-name object) context)
    (write-generic-string (package-name (symbol-package object)) context))
  (intern (read-generic-string context) (read-generic-string context)))

;;;;;;;;;;;;;
;;; Serialize

(def (function o) default-has-identity-function (object)
  (and (not (null object))
       (typep object '(or symbol standard-object))))

(def (function o) serialize (object &key output (has-identity-function #'default-has-identity-function))
  (let ((context (make-serializer-context :buffer (make-array 256 :element-type '(unsigned-byte 8))
                                          :has-identity-function has-identity-function)))
    (write-unsigned-byte-8 +version+ context)
    (serialize-element object context)
    (etypecase output
      (stream (write-sequence (sc-buffer context) output :end (sc-position context)))
      (null (let ((final-vector (make-array (sc-position context) :element-type '(unsigned-byte 8))))
              (replace final-vector (sc-buffer context))
              final-vector)))))

(def (function io) serialize-element (object context)
  (declare (type serializer-context context))
  (let ((code
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
                  (standard-object +standard-object+))))))
    (if (funcall (the function (sc-has-identity-function context)) object)
        (let* ((circularity-map (circularity-map context))
               (position (gethash object circularity-map)))
          (if position
              (let ((buffer (sc-buffer context)))
                (setf (aref buffer position)
                      (logior +referenced-bit-mask+ (aref buffer position)))
                ;;(format t "~%Serializing reference at ~A to object ~A stored at ~A" (sc-position context) object position)
                (write-unsigned-byte-8 +reference+ context)
                (write-unsigned-byte-32 position context))
              (progn
                (setf (gethash object circularity-map) (sc-position context))
                (write-unsigned-byte-8 code context)
                (funcall (the function (aref +serializers+ code)) object context))))
        (progn
          (write-unsigned-byte-8 code context)
          (funcall (the function (aref +serializers+ code)) object context)))))

;;;;;;;;;;;;;;;
;;; Deserialize

(def (function o) deserialize (input)
  (let ((context
         (etypecase input
           (array
            (make-serializer-context :buffer (coerce input '(simple-array (unsigned-byte 8) (*)))))
           (stream
            (make-serializer-context :buffer (read-stream-into-vector input))))))
    (unless (= +version+ (read-unsigned-byte-8 context))
      (error "Serializer version mismatch"))
    (deserialize-element context)))

(def (function io) deserialize-element (context)
  (declare (type serializer-context context))
  (when (sc-last-referenced-object-position context)
    (error "Deserializer did not notify about identity"))
  (let* ((code-with-referenced-bit (read-unsigned-byte-8 context))
         (code (logand code-with-referenced-bit +code-mask+)))
    (unless (zerop (logand code-with-referenced-bit +referenced-bit-mask+))
      (setf (sc-last-referenced-object-position context) (1- (sc-position context))))
    (if (eq code +reference+)
        (let* ((position (read-unsigned-byte-32 context))
               (object
                (gethash position (circularity-map context) :not-found)))
          ;;(format t "~%Deserializing reference at ~A to object ~A at ~A" (- (sc-position context) 5) object position)
          (when (eq object :not-found)
            (error "Reference ~A cannot be resolved, byte at that position is: ~A" position (aref (sc-buffer context) position)))
          object)
        (funcall (the function (aref +deserializers+ code)) context))))

(def (function io) notify-identity (object context)
  (let ((last-referenced-object-position (sc-last-referenced-object-position context)))
    (when last-referenced-object-position
      ;;(format t "~%Storing referenced object ~A at: ~A" object last-referenced-object-position)
      (setf (gethash last-referenced-object-position (circularity-map context)) object)
      (setf (sc-last-referenced-object-position context) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serializer deserializer

(def definer serializer-deserializer (code type serializer-form deserializer-form)
  `(progn
    (setf (aref +serializers+ ,code)
     (lambda (object context)
       (declare (ignorable object context)
                (type ,type object)
                (type serializer-context context))
       ,serializer-form
       (values)))
    (setf (aref +deserializers+ ,code)
     (lambda (context)
       (declare (ignorable context)
                (type serializer-context context))
       (the ,type
         (values
          (progn
            ,deserializer-form)))))))

(def serializer-deserializer +nil+ null
  nil
  nil)

(def serializer-deserializer +t+ t
  t
  t)

(def serializer-deserializer +cons+ cons
  (progn
    (serialize-element (car object) context)
    (serialize-element (cdr object) context))
  ;; TODO: when circularity not needed: (cons (deserialize-element context) (deserialize-element context))
  (cons (deserialize-element context) (deserialize-element context))
  #+nil
  (let ((cons (cons nil nil)))
    (notify-identity cons context)
    (setf (car cons) (deserialize-element context))
    (setf (cdr cons) (deserialize-element context))
    cons))

;; TODO: 
(def serializer-deserializer +integer+ integer
  (write-unsigned-byte-32 object context)
  (read-unsigned-byte-32 context))

(def serializer-deserializer +string+ string
  (write-string object context)
  (read-string context))

(def serializer-deserializer +symbol+ symbol
  (write-symbol object context)
  #+nil ;; not circular stuff
  (read-symbol context)
  (let ((symbol (read-symbol context)))
    (notify-identity symbol context)
    symbol))

(def serializer-deserializer +standard-object+ standard-object
  (let* ((class (class-of object))
         (slots (closer-mop:class-slots class)))
    (declare (type list slots))
    (write-symbol (class-name class) context)
    (write-unsigned-byte-8 (length slots) context)
    (dolist (slot slots)
      (write-symbol (closer-mop:slot-definition-name slot) context)
      (if (closer-mop:slot-boundp-using-class class object slot)
          (serialize-element (closer-mop:slot-value-using-class class object slot) context)
          (write-unsigned-byte-8 +unbound-slot+ context))))
  (let* ((class-name (read-symbol context))
         (class (find-class class-name))
         (object (make-instance class)))
    (notify-identity object context)
    (loop repeat (the fixnum (read-unsigned-byte-8 context))
      for slot-name = (read-symbol context) do
      (if (eq +unbound-slot+ (read-unsigned-byte-8 context))
          (slot-makunbound object slot-name)
          (setf (slot-value object slot-name)
                (progn
                  (decf (sc-position context))
                  (deserialize-element context)))))
    object))
