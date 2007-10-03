;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-serializer)

(def macro prog1-bind (var ret &body forms)
  `(bind ((,var ,ret))
    ,@forms
    ,var))

(def function concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with an exception: when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (cl:write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (cl:write-string (symbol-name arg) str))
                             (integer (cl:write-string (princ-to-string arg) str))
                             (character (cl:write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

(def (function o) read-stream-into-vector (stream)
  (prog1-bind buffer
      (make-array (file-length stream) :element-type '(unsigned-byte 8))
    (read-sequence buffer stream)))
