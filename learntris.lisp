#!/usr/bin/sbcl --script

(defvar *board* nil)
(defvar *tetramino-board* nil)
(defvar *score* 0)
(defvar *lines-cleared* 0)
(defvar empty-row nil)
(defvar empty-board nil)			      
(defvar *active-tetramino* nil)
(defconstant i-tetramino '((#\. #\. #\. #\.)
			   (#\c #\c #\c #\c)
			   (#\. #\. #\. #\.)
			   (#\. #\. #\. #\.)))
(defconstant o-tetramino '((#\y #\y)
			   (#\y #\y)))
(defconstant z-tetramino '((#\r #\r #\.)
			   (#\. #\r #\r)
			   (#\. #\. #\.)))
(defconstant s-tetramino '((#\. #\g #\g)
			   (#\g #\g #\.)
			   (#\. #\. #\.)))
(defconstant j-tetramino '((#\b #\. #\.)
			   (#\b #\b #\b)
			   (#\. #\. #\.)))
(defconstant l-tetramino '((#\. #\. #\o)
			   (#\o #\o #\o)
			   (#\. #\. #\.)))
(defconstant t-tetramino '((#\. #\m #\.)
			   (#\m #\m #\m)
			   (#\. #\. #\.)))

(defun global-init ()
  (setf empty-row (loop for i from 1 to 10 collect #\.))
  (setf empty-board (loop for i from 1 to 22 collect empty-row))
  (init))

(defun init ()
  (setf *lines-cleared* 0)
  (setf *score* 0)
  (setf *board* empty-board)
  (setf *tetramino-board* empty-board))

(defvar end-game nil)
(defun game ()
  (global-init)
  (block quit
    (setf end-game #'(lambda () (return-from quit)))
    (loop for input = (read-line)
       do (input-converter input))))

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

(defun input-handler (input)
  (case input
    (#\p (print-board))
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
    (#\P (print-matrix *tetramino-board*))
    ((#\< #\> #\v) (nudge input))
    (#\V (plunge))))

(defun next-step ()
  (setf *board*
	(loop for row in *board*
	   collect (if (unbroken-row-p row)
		       (progn (incf *lines-cleared*)
			      (incf *score* 100)
			      empty-row)
		       row))))

(defun unbroken-row-p (row)
  (loop for i in row never (char= i #\.)))

(defun insert-tetramino ()
  (loop initially (setf *tetramino-board* (copy-list empty-board))
     for x = 0 then (1+ x)
     for row in *active-tetramino*
     do (setf (elt *tetramino-board* x) (generate-row row))))

(defun generate-row (row)
  (setf row (mapcar #'char-upcase row))
  (let ((right t))
    (loop until (= (length row) 10)
       do (if right (progn (setf row (append row '(#\.))) (setf right nil))
	      (progn (push #\. row) (setf right t)))))
  row)

(defun collision-p (direction)
  (case direction
    (#\< (notevery #'(lambda (row) (eql #\. (car row)))
		*tetramino-board*))
    (#\> (notevery #'(lambda (row) (eql #\. (car (last row))))
		 *tetramino-board*))
    (#\v (notevery #'(lambda (char) (eql #\. char))
		   (car (last *tetramino-board*))))))

(defun nudge (direction)
  (unless (collision-p direction)
    (setf *tetramino-board*
	  (case direction
	    (#\< (loop for row in *tetramino-board*
		    collect (append (cdr row) '(#\.))))
	    (#\> (loop for row in *tetramino-board*
		    collect (cons #\. (butlast row))))
	    (#\v (cons empty-row (butlast *tetramino-board*)))))))

(defun plunge ()
  (loop until (null (nudge #\v))))

(defun rotate-clockwise ()
  (setf *active-tetramino*
	 (loop until (null (car *active-tetramino*))
	    collect (reverse
		     (loop repeat (length *active-tetramino*)
			for x = 0 then (1+ x)
			collect (pop (elt *active-tetramino* x))))))
  (insert-tetramino))

(defun rotate-anti-clockwise ()
  (setf *active-tetramino*
	(reverse
	 (labels ((pop-and-go (l)
		     (if (null (car l)) nil
			 (progn
			   (cons (pop (car l)) (pop-and-go (cdr l)))))))
	    (loop until (null (car *active-tetramino*))
	       collect (pop-and-go *active-tetramino*)))))
  (insert-tetramino))
  

(defun set-tetramino (input)
  (case input
    (#\I (setf *active-tetramino* i-tetramino))
    (#\O (setf *active-tetramino* o-tetramino))
    (#\Z (setf *active-tetramino* z-tetramino))
    (#\S (setf *active-tetramino* s-tetramino))
    (#\J (setf *active-tetramino* j-tetramino))
    (#\L (setf *active-tetramino* l-tetramino))
    (#\T (setf *active-tetramino* t-tetramino)))
  (insert-tetramino))

(defun print-board ()
  (print-matrix 
   (loop for ya in *board*
      for yb in *tetramino-board*
      collect (loop for xa in ya
		 for xb in yb
		 collect (if (eql xa #\.) (char-downcase xb) xa)))))

(defun display (val)
  (format t "~a~%" val))

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

(game)



