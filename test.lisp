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
  ((slot :initarg :slot :accessor :slot-of)))

(def test test/serialize-deserialize ()
  (dolist (object `(nil t 0 -1 1 a "b" (c d) ,(make-instance 'serialize-test :slot 1)))
    (stefil:is (equal object (deserialize (serialize object))))))

#|
;; TODO:
(def function test/performance ()
  (let ((k (with-call/cc
             (print "Hello")
             (let/cc k k)
             (print "World"))))
    (kall (deserialize (serialize k)))))

(defvar ii '(a (b c) a b (d e (f g (h i)))))

(cl:time
 (iter (repeat 10000)
       (deserialize (serialize ii))))

(cl:time
 (iter (repeat 10000)
       (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                         (cl-store:store ii stream)))
         (cl-store:restore stream))))
|#
