#!/usr/bin/sbcl --script

(defvar *board* nil)
(defvar *score* 0)
(defvar *lines-cleared* 0)
(defvar empty-row nil)
(defvar empty-board nil)			      
(defvar *active-tetramino* nil)
(defparameter i-tetramino '((#\. #\. #\. #\.)
			    (#\c #\c #\c #\c)
			    (#\. #\. #\. #\.)
			    (#\. #\. #\. #\.)))
(defparameter o-tetramino '((#\y #\y)
			    (#\y #\y)))
(defparameter z-tetramino '((#\r #\r #\.)
			    (#\. #\r #\r)
			    (#\. #\. #\.)))
(defparameter s-tetramino '((#\. #\g #\g)
			    (#\g #\g #\.)
			    (#\. #\. #\.)))
(defparameter j-tetramino '((#\b #\. #\.)
			    (#\b #\b #\b)
			    (#\. #\. #\.)))
(defparameter l-tetramino '((#\. #\. #\o)
			    (#\o #\o #\o)
			    (#\. #\. #\.)))
(defparameter t-tetramino '((#\. #\m #\.)
			    (#\m #\m #\m)
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

(defvar end-game nil)
(defun game ()
  (global-init)
  (block quit
    (setf end-game #'(lambda () (return-from quit)))
    (loop for input = (read-line)
       do (input-converter input))))

(defun next-step ()
  (setf *board*
	(loop for row in *board*
	   collect (if (unbroken-row-p row)
		       (progn (incf *lines-cleared*)
			      (incf *score* 100)
			      empty-row)
		       row))))

(defun insert-tetramino ()
  (setf *board*
	(append
	 (loop
	    initially (loop for row in *active-tetramino* do (pop *board*))
	    for row in *active-tetramino*
	    collect (generate-row row))
	 *board*))
  (print-matrix *board*))

(defun generate-row (row)
  (setf row (mapcar #'char-upcase row))
  (let ((right t))
    (loop until (= (length row) 10)
       do (if right (progn (setf row (append row '(#\.))) (setf right nil))
	      (progn (push #\. row) (setf right t)))))
  row)

(defun rotate-clockwise ()
  (setf *active-tetramino* (mapcar #'reverse *active-tetramino*))
  (rotate-anti-clockwise)
  (setf *active-tetramino* (mapcar #'reverse *active-tetramino*)))

(defun rotate-anti-clockwise ()
  (setf *active-tetramino*
	(reverse
	  (labels ((pop-and-go (l)
		     (if (null (car l)) nil
			 (progn
			   (cons (pop (car l)) (pop-and-go (cdr l)))))))
	    (loop until (null (car *active-tetramino*))
	       collect (pop-and-go *active-tetramino*))))))

(defun set-tetramino (input)
  (case input
    (#\I (setf *active-tetramino* i-tetramino))
    (#\O (setf *active-tetramino* o-tetramino))
    (#\Z (setf *active-tetramino* z-tetramino))
    (#\S (setf *active-tetramino* s-tetramino))
    (#\J (setf *active-tetramino* j-tetramino))
    (#\L (setf *active-tetramino* l-tetramino))
    (#\T (setf *active-tetramino* t-tetramino))))

(defun print-matrix (matrix)
  (format t "~{~{~a~^ ~}~%~}" matrix))

(defun input-board ()
  (setf *board* (read-board)))

(defun read-board ()
  (loop for i from 1 to 22
     collect
       (loop for c across (read-line)
	  unless (eql c #\space)
	    collect c)))

(defun display (val)
  (format t "~a~%" val))

(defun input-handler (input)
  (case input
    (#\p (print-matrix *board*))
    (#\q (funcall end-game))
    (#\g (input-board))
    (#\c (init))
    (?s (display *score*))
    (?n (display *lines-cleared*))
    (#\s (next-step))
    (#\t (print-matrix *active-tetramino*))
    ((#\I #\O #\Z #\S #\J #\L #\T) (set-tetramino input))
    (#\) (rotate-clockwise))
    (#\( (rotate-anti-clockwise))
    (#\; (format t "~%"))
    (#\P (insert-tetramino))))

(let ((special nil)
      (specials '((#\s ?s)
		  (#\n ?n))))
  (labels ((specify (c)
	     (second (assoc c specials))))
    (defun input-converter (input)
      (setf special nil)
      (loop for c across input
	 when (eql c #\?)
	 do (setf special t)
	 else unless (eql c #\space)
	 do (progn (input-handler (if special (specify c) c))
		   (setf special nil))))))

(game)
