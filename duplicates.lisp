;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-serializer)

(defun concatenate-symbol (&rest args)
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

(defun remove-keywords (plist &rest keywords)
  "Creates a copy of PLIST without the listed KEYWORDS."
  (declare (optimize (speed 3)))
  (loop for cell = plist :then (cddr cell)
        for el = (car cell)
        while cell
        unless (member el keywords :test #'eq)
        collect el
        and collect (cadr cell)
        and do (assert (cdr cell) () "Not a proper plist")))
