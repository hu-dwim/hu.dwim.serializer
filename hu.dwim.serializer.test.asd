;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.serializer.test
  :class hu.dwim.test-system
  :description "Test suite for hu.dwim.serializer"
  :depends-on (:hu.dwim.def+hu.dwim.stefil
               :hu.dwim.serializer)
  :default-component-class local-cl-source-file
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "test" :depends-on ("suite"))))))
