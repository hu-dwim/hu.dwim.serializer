;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.serializer)

(def macro ensure-simple-vector-size (vector size)
  `(let ((length (length ,vector)))
     (when (< length ,size)
       (let ((new-vector (make-array (max (1+ (* 2 length)) ,size) :element-type '(unsigned-byte 8))))
         (replace new-vector ,vector)
         (setf ,vector new-vector)))))

(def (function o) read-stream-into-vector (stream)
  (bind ((buffer-size 1024)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
         (buffer-pointer 0)
         (read-buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop for bytes-read = (read-sequence read-buffer stream)
       do (progn
            (ensure-simple-vector-size buffer (+ buffer-pointer bytes-read))
            (replace buffer read-buffer :start1 buffer-pointer :end2 bytes-read)
            (incf buffer-pointer bytes-read))
       while (= bytes-read buffer-size)
       finally (return
                 (prog1-bind result-buffer (make-array buffer-pointer :element-type '(unsigned-byte 8))
                   (replace result-buffer buffer :end2 buffer-pointer))))))
