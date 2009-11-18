;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.serializer)

(def type simple-unsigned-byte-8-vector (&optional (size '*))
  `(simple-array (unsigned-byte 8) (,size)))

(def (function o) analyze-list (list)
  "Returns two values.  The first value is of type (member :PROPER-LIST :DOTTED-LIST :CIRCULAR-LIST). The second value is the length of the list. For dotted lists the final item is included in the length. For circular lists the length is NIL."
  ;; This is an adapatation of the algorithm in the Hyperspec (see LIST-LENGTH).
  (declare (type list list))
  (loop
    :for n fixnum = 0 :then (the (integer 0 #.(- most-positive-fixnum 2)) (+ n 2))
    :for fast = list :then (cddr fast)
    :for slow = list :then (cdr slow)
    ;; If fast pointer hits the end, return the count.
    :do (cond
          ((null fast)
           (return (values :proper-list n)))
          ((atom fast)
           (return (values :dotted-list (the fixnum (1+ n)))))
          ((null (cdr fast))
           (return (values :proper-list (the fixnum (1+ n)))))
          ((atom (cdr fast))
           (return (values :dotted-list (the fixnum (+ n 2)))))
          ;; If fast pointer eventually equals slow pointer,
          ;;  then we must be stuck in a circular list.
          ;; (A deeper property is the converse: if we are
          ;;  stuck in a circular list, then eventually the
          ;;  fast pointer will equal the slow pointer.
          ;;  That fact justifies this implementation.)
          ((and (eq fast slow) (> n 0))
           (return (values :circular-list 0))))))

(def macro format-log (format-specifier &rest args)
  (declare (ignorable format-specifier args))
  (if *load-as-production?*
      (values)
      `(unless (ignore-errors
                 (format t ,format-specifier ,@args)
                 t)
         (format t "~%Error during formatting ~S" ,format-specifier))))
