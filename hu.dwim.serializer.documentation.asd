;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.serializer.documentation
  :class hu.dwim.documentation-system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Documentation for hu.dwim.serializer"
  :depends-on (:hu.dwim.serializer.test
               :hu.dwim.wui)
  :components ((:module "documentation"
                :components ((:file "package")
                             (:file "serializer" :depends-on ("package"))))))
