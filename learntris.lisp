#!/usr/bin/sbcl --script

(defvar *matrix* nil)

(defun empty-board ()
  (loop for i from 1 to 22
     collect (loop for j from 1 to 10 collect '\.)))

(defun init ()
  (setf *matrix* (empty-board)))

(defun game ()
  (loop for input = (read)
     until (equal input 'q)
     do (input-handler input)))

(defun input-handler (input)
  (case input
    (p (print-board))))

(defun print-board ()
  (format t "~{~{~a~^ ~}~%~}" *matrix*))

(init)
(game)

