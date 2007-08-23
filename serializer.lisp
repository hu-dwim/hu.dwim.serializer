;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-serializer)

;; TODO: add circularity support
;; TODO: use 1 bit from code to test whether circularity check is needed during runtime

;;;;;;;;
;;; Atom

(def constant +t+                 #x00)

(def constant +nil+               #x01)

(def constant +cons+              #x02)

;;;;;;;;;;
;;; String

(def constant +character+         #x10)

(def constant +string+            #x11)

;;;;;;;;;;
;;; Symbol)

(def constant +keyword+           #x20)

(def constant +uninterned-symbol+ #x21)

(def constant +symbol+            #x22)

(def constant +package+           #x23)

;;;;;;;;;;
;;; Number

(def constant +zero+              #x30)

(def constant +integer+           #x31)

(def constant +rational+          #x32)

(def constant +float+             #x33)

(def constant +short-float+       #x34)

(def constant +single-float+      #x35)

(def constant +double-float+      #x36)

(def constant +long-float+        #x37)

(def constant +complex+           #x38)

;;;;;;;;;;;;;;;;;
;;; Compound type

(def constant +hash-table+        #x40)

(def constant +pathname+          #x41)

(def constant +array+             #x42)

;;;;;;;;;;
;;; Object

(def constant +standard-object+   #x50)

(def constant +unbound-slot+      #x51)

(def constant +structure-object+  #x52)

;;;;;;;;;;;;;;;
;;; Circularity

(def constant +reference+         #x7F)

;;;;;;;;;;;;;;;;;;
;;; Code -> lambda

(def (constant :test 'equalp) +serializers+ (make-array 128))

(def (constant :test 'equalp) +deserializers+ (make-array 128))

(def constant +version+ 0)

(defstruct (serializer-context (:conc-name sc-))
  (buffer
   #()
   :type (simple-array (unsigned-byte 8) (*)))
  (position
   0
   :type array-index))

;;;;;;;;;;;;;;;;;
;;; Reader writer

(def definer writer-reader (name type writer-form reader-form)
  `(progn
    (def (function io) ,(concatenate-symbol *package* "write-" name) (object context)
      (declare (optimize (speed 3) (debug 0) (safety 0))
               (type serializer-context context)
               (type ,type object))
      ,writer-form
      (values))
    (def (function io) ,(concatenate-symbol *package* "read-" name) (context)
      (declare (optimize (speed 3) (debug 0) (safety 0))
               (type serializer-context context))
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
    (setf (ldb (byte 8 0) object) (read-unsigned-byte-8 context))))

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

(def writer-reader symbol symbol
  (progn
    (write-string (symbol-name object) context)
    (write-string (package-name (symbol-package object)) context))
  (intern (read-string context) (read-string context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serializer deserializer

(def function read-stream-into-vector (stream)
  ;; TODO:
  (declare (ignore stream))
  )

(def definer serializer-deserializer (code type serializer-form deserializer-form)
  `(progn
    (setf (aref +serializers+ ,code)
     (lambda (object context)
       (declare (ignorable object context)
                (optimize (speed 3) (debug 0) (safety 0))
                (type ,type object)
                (type serializer-context context))
       ,serializer-form
       (values)))
    (setf (aref +deserializers+ ,code)
     (lambda (context)
       (declare (ignorable context)
                (optimize (speed 3) (debug 0) (safety 0))
                (type serializer-context context))
       (the ,type
         (values
          (progn
            ,deserializer-form)))))))

(def (function o) serialize (object &optional output)
  (let ((context (make-serializer-context :buffer (make-array 256 :element-type '(unsigned-byte 8)))))
    (write-unsigned-byte-8 +version+ context)
    (%serialize object context)
    (etypecase output
      (stream (write-sequence (sc-buffer context) output :end (sc-position context)))
      (null (let ((final-vector (make-array (sc-position context) :element-type '(unsigned-byte 8))))
              (replace final-vector (sc-buffer context))
              final-vector)))))

(def (function io) %serialize (object context)
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
                  (string +string+)
                  (standard-object +standard-object+))))))
    (write-unsigned-byte-8 code context)
    (funcall (the function (aref +serializers+ code)) object context)))

(def (function o) deserialize (input)
  (let ((context
         (etypecase input
           (array
            (make-serializer-context :buffer (coerce input '(simple-array (unsigned-byte 8) (*)))))
           (stream
            (make-serializer-context :buffer (read-stream-into-vector input))))))
    (unless (= +version+ (read-unsigned-byte-8 context))
      (error "Serializer version mismatch"))
    (%deserialize context)))

(def (function io) %deserialize (context)
  (declare (type serializer-context context))
  (funcall (the function (aref +deserializers+ (read-unsigned-byte-8 context))) context))

(def serializer-deserializer +nil+ null
  nil
  nil)

(def serializer-deserializer +t+ t
  t
  t)

(def serializer-deserializer +cons+ cons
  (progn
    (%serialize (car object) context)
    (%serialize (cdr object) context))
  (cons (%deserialize context) (%deserialize context)))

;; TODO: 
(def serializer-deserializer +integer+ integer
  (write-unsigned-byte-32 object context)
  (read-unsigned-byte-32 context))

(def serializer-deserializer +string+ string
  (write-string object context)
  (read-string context))

(def serializer-deserializer +symbol+ symbol
  (write-symbol object context)
  (read-symbol context))

(def serializer-deserializer +standard-object+ standard-object
  (let* ((class (class-of object))
         (slots (closer-mop:class-slots class)))
    (declare (type list slots))
    (write-symbol (class-name class) context)
    (write-unsigned-byte-8 (length slots) context)
    (dolist (slot slots)
      (write-symbol (closer-mop:slot-definition-name slot) context)
      (%serialize (closer-mop:slot-value-using-class class object slot) context)))
  (let* ((class-name (read-symbol context))
         (class (find-class class-name))
         (object (make-instance class)))
    (loop repeat (read-unsigned-byte-8 context) do
          (setf (slot-value object (read-symbol context))
                (%deserialize context)))
    object))
