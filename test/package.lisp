;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def package :hu.dwim.serializer.test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.serializer
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.serializer)))
