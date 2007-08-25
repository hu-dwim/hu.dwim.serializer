;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defpackage :cl-serializer-test
  (:nicknames :serializer-test)

  (:use :common-lisp :cl-def :stefil :cl-serializer))

(in-package :cl-serializer-test)

;;;;;;;;
;;; Test

(in-root-suite)

(defsuite* test)

(defclass serialize-test ()
  ((slot :initarg :slot :accessor slot-of)))

(defsuite* (test/serialize-deserialize :in test))

(def definer serialize-deserialize-test (name value)
  `(def test ,(serializer::concatenate-symbol *package* "test/serialize-deserialize/" name) ()
    (is (equal ,value (deserialize (serialize ,value))))))

(def serialize-deserialize-test nil nil)

(def serialize-deserialize-test t t)

(def serialize-deserialize-test integer/1 -1)
(def serialize-deserialize-test integer/2 0)
(def serialize-deserialize-test integer/3 1)
(def serialize-deserialize-test integer/4 255)
(def serialize-deserialize-test integer/5 256)

(def serialize-deserialize-test string/1 "")
(def serialize-deserialize-test string/2 "test")
(def serialize-deserialize-test string/3 "áéíóúöőüűÁÉÍÓÚÖŐÜŰ")

(def serialize-deserialize-test symbol/1 'test)

(def serialize-deserialize-test cons/1 (let ((cons (cons nil nil)))
                                         (setf (car cons) cons)
                                         (setf (cdr cons) cons)
                                         cons))

(def serialize-deserialize-test proper-list/1 (list nil t))

(def serialize-deserialize-test dotted-list/1 (list nil t))

(def serialize-deserialize-test circularity/1 (let ((instance (make-instance 'serialize-test)))
                                                (setf (slot-of instance) instance)
                                                instance))

#|

(def function cl-store-serialize (object)
  (flexi-streams:with-output-to-sequence (stream)
    (cl-store:store object stream)))

;; TODO:

(defvar k (with-call/cc
            (print "Hello")
            (let/cc k k)
            (print "World")))

(length
 (flexi-streams:with-output-to-sequence (stream)
   (cl-store:store k stream)))
800
(cl:time
 (iter (repeat 1000)
       (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                         (cl-store:store k stream)))
         (cl-store:restore stream))))
Evaluation took:
  2.329 seconds of real time
  2.288143 seconds of user run time
  0.0 seconds of system run time
  [Run times include 0.044 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  35,586,656 bytes consed.
NIL



(length (serialize k))
652
(cl:time
 (iter (repeat 1000)
       (deserialize (serialize k))))
Evaluation took:
  0.232 seconds of real time
  0.228014 seconds of user run time
  0.004001 seconds of system run time
  [Run times include 0.012 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  11,797,056 bytes consed.
NIL










(defvar ii '(a (b c) a b (d e (f g (h i)))))


(length
 (flexi-streams:with-output-to-sequence (stream)
   (cl-store:store ii stream)))
158
(cl:time
 (iter (repeat 10000)
       (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                         (cl-store:store ii stream)))
         (cl-store:restore stream))))
Evaluation took:
  4.189 seconds of real time
  4.120258 seconds of user run time
  0.028002 seconds of system run time
  [Run times include 0.112 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  95,359,584 bytes consed.
NIL


(length (serialize ii))
88
(cl:time
 (iter (repeat 10000)
       (deserialize (serialize ii))))
Evaluation took:
  0.394 seconds of real time
  0.392025 seconds of user run time
  0.0 seconds of system run time
  [Run times include 0.028 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  31,204,976 bytes consed.
NIL
|#
