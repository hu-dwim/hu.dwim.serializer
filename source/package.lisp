;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(hu.dwim.def:def package :hu.dwim.serializer
  (:use :babel
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.util)
  (:shadow #:read-string
           #:write-string)
  (:readtable-setup
   (hu.dwim.syntax-sugar:enable-sharp-boolean-syntax)
   (hu.dwim.syntax-sugar:enable-feature-cond-syntax)))
