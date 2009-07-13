;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.serializer
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Generic serializer and deserializer"
  :depends-on (:babel
               :hu.dwim.common-lisp
               :hu.dwim.def
               :hu.dwim.syntax-sugar
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "configuration" :depends-on ("duplicates"))
                             (:file "serializer" :depends-on ("configuration"))))))