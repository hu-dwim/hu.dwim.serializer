;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(hu.dwim.def:def package :hu.dwim.serializer.test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.serializer
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar)
  (:readtable-setup/same-as :hu.dwim.serializer))
