;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.serializer.test)

(def function setup-readtable ()
  (enable-sharp-boolean-syntax))

(def suite* (test :in root-suite))
