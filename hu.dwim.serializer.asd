;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.serializer
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :description "Generic serializer and deserializer."
  :depends-on (:babel
               :hu.dwim.common
               :hu.dwim.def
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :hu.dwim.util.mop)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "util" :depends-on ("duplicates"))
                             (:file "serializer" :depends-on ("duplicates" "util"))))))
