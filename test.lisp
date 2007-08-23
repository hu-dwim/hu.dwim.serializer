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
