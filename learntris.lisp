#!/usr/bin/sbcl --script

(defvar *matrix* nil)
(defvar *score* 0)
(defvar *lines-cleared* 0)
(defvar empty-row nil)
(defvar empty-board nil)
(defparameter *valid-input* '(#\. #\b #\c #\g #\m #\o #\r #\y))

(defun global-init ()
  (setf empty-row (loop for i from 1 to 10 collect #\.))
  (setf empty-board (loop for i from 1 to 22 collect empty-row))
  (init))

(defun unbroken-row-p (row)
  (loop for i in row never (char= i #\.)))

(defun init ()
  (setf *lines-cleared* 0)
  (setf *score* 0)
  (setf *matrix* empty-board))

(defun game ()
  (loop for input = (read)
     until (equal input 'q)
     do (input-handler input)))

(defun input-handler (input)
  (case input
    (s (next-step))
    (p (print-board))
    (g (setf *matrix* (read-board)))
    (c (init))
    (?s (display *score*))
    (?n (display *lines-cleared*))))

(defun next-step ()
  (setf *matrix*
	(loop for row in *matrix*
	   collect (if (unbroken-row-p row)
		       (progn (incf *lines-cleared*)
			      (incf *score* 100)
			      empty-row)
		       row))))

(defun print-board ()
  (format t "~{~{~a~^ ~}~%~}" *matrix*))

(defun read-board ()
  (loop for i from 1 to 22
       collect (loop for j from 1 to 10 collect (reader))))

(defun reader ()
  (let ((input (read-char)))
    (if (member input *valid-input*)
	input
	(reader))))
;;yeah yeah it's fucking stupid but what can i do
;;they're partial towards languages that don't have
;;special meanings for the period :(

(defun display (val)
  (format t "~a~%" val))

(global-init)
(game)











