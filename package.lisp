;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-serializer-system)

(defpackage :cl-serializer
  (:nicknames :serializer)

  (:use :common-lisp :alexandria :cl-def :babel)
  
  (:shadow #:read-string
           #:write-string)

  (:export #:serialize
           #:deserialize))

(in-package :cl-serializer)

(defun transform-function-definer-options (options)
  (if cl-serializer-system:*load-with-debug-p*
      (remove-from-plist options :inline :optimize)
      options))
