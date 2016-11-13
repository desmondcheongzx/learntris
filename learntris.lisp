#!/usr/bin/sbcl --script

(defvar *board* nil)
(defvar *score* 0)
(defvar *lines-cleared* 0)
(defvar empty-row nil)
(defvar empty-board nil)
(defparameter *valid-input* '(#\. #\b #\c #\g #\m #\o #\r #\y))
(defvar *active-tetramino* nil)
(defparameter i-tetramino '((#\. #\. #\. #\.)
			    (#\C #\C #\C #\C)
			    (#\. #\. #\. #\.)
			    (#\. #\. #\. #\.)))
(defparameter o-tetramino '((#\Y #\Y)
			    (#\Y #\Y)))
(defparameter z-tetramino '((#\R #\R #\.)
			    (#\. #\R #\R)
			    (#\. #\. #\.)))
(defparameter s-tetramino '((#\. #\G #\G)
			    (#\G #\G #\.)
			    (#\. #\. #\.)))
(defparameter j-tetramino '((#\B #\. #\.)
			    (#\B #\B #\B)
			    (#\. #\. #\.)))
(defparameter l-tetramino '((#\. #\. #\O)
			    (#\O #\O #\O)
			    (#\. #\. #\.)))
(defparameter t-tetramino '((#\. #\M #\.)
			    (#\M #\M #\M)
			    (#\. #\. #\.)))

(defun global-init ()
  (setf empty-row (loop for i from 1 to 10 collect #\.))
  (setf empty-board (loop for i from 1 to 22 collect empty-row))
  (init))

(defun unbroken-row-p (row)
  (loop for i in row never (char= i #\.)))

(defun init ()
  (setf *lines-cleared* 0)
  (setf *score* 0)
  (setf *board* empty-board))

(defun game ()
  (global-init)
  (block quit
    (loop for input = (read-line)
       do (input-converter input #'(lambda () (return-from quit))))))

(defun input-handler (input)
  (case input
    (w (next-step)) ;changed to w because we substitute w for small s
    (p (print-matrix *board*))
    ((t) (print-matrix *active-tetramino*))
    (g (setf *board* (read-board)))
    (c (init))
    (?w (display *score*)) ;changed to w because we substitute w for s
    (?n (display *lines-cleared*))
    ((I O Z S J L V) (set-tetramino input))
    (f (rotate-clockwise))
    (d (rotate-anti-clockwise))
    (k (format t "~%"))))

(defun next-step ()
  (setf *board*
	(loop for row in *board*
	   collect (if (unbroken-row-p row)
		       (progn (incf *lines-cleared*)
			      (incf *score* 100)
			      empty-row)
		       row))))

(defun rotate-clockwise ()
  (setf *active-tetramino* (mapcar #'nreverse *active-tetramino*))
  (rotate-anti-clockwise)
  (setf *active-tetramino* (mapcar #'nreverse *active-tetramino*)))

(defun rotate-anti-clockwise ()
  (setf *active-tetramino*
	(nreverse
	  (labels ((pop-and-go (l)
		     (if (null (car l)) nil
			 (progn
			   (cons (pop (car l)) (pop-and-go (cdr l)))))))
	    (loop until (null (car *active-tetramino*))
	       collect (pop-and-go *active-tetramino*))))))

(defun set-tetramino (input)
  (case input
    (I (setf *active-tetramino* i-tetramino))
    (O (setf *active-tetramino* o-tetramino))
    (Z (setf *active-tetramino* z-tetramino))
    (S (setf *active-tetramino* s-tetramino))
    (J (setf *active-tetramino* j-tetramino))
    (L (setf *active-tetramino* l-tetramino))
    (V (setf *active-tetramino* t-tetramino))))

(defun print-matrix (matrix)
  (format t "~{~{~a~^ ~}~%~}" matrix))

(defun read-board ()
  (loop for i from 1 to 22
       collect (loop for j from 1 to 10 collect (reader))))

(defun reader ()
  (let ((input (read-char)))
    (if (member input *valid-input*) input
	(reader))))
;;yeah yeah it's fucking stupid but what can i do
;;they're partial towards languages that don't have
;;special meanings for the period :(

(defun display (val)
  (format t "~a~%" val))

(defparameter substitutions '((#\w #\s)
			      (#\V #\T)
			      (#\k #\;)
			      (#\f #\)) ;f for clockwise, duh
			      (#\d #\())) ;d for anti-clockwise, duh duh
(defun input-converter (input quit)
  (labels ((substituter (subs cur)
	     (if (null (car subs)) cur
		 (substitute (first (car subs))
			     (second (car subs))
			     (substituter (cdr subs) cur)))))
    (mapcar #'(lambda (x)
		(if (equal x 'q)
		    (funcall quit)
		    (input-handler x)))
	    (read-from-string
	     (concatenate 'string "("
			  (substituter substitutions input)
			  ")")))))

(game)
