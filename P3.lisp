(defconstant empty 0 "An empty square")
(defconstant red 1 "A red piece")
(defconstant yellow 2 "A yellow piece")
(defconstant outer 3 "Marks squares outside the 7x6 board")

(defconstant winningvalue most-positive-fixnum)
(defconstant losingvalue  most-negative-fixnum)

(defvar anzahlbew 0)

(defun nameof (piece)
  (char " RY" piece))

(defun createboard ()
  (let ((board (make-array (list 9 8) :initial-element empty)))
  	(dotimes (col (array-dimension board 0))
    	(dotimes (row (array-dimension board 1))
      		(if (or (equal col 0) (equal row 0) (equal col 8) (equal row 7))
          		(setf (aref board col row) outer))))
  board))

(defun opponent (piece) 
  (if (equal piece red) yellow red))

(defun legalmoves (board)
  (let ((legalcols '()))
	(dotimes (col (array-dimension board 0))
    	(if (equal (aref board col 1) 0)
        	(setf legalcols (append legalcols (cons col '())))))
 	(if (equal legalcols '())
  		nil
    	legalcols)))

(defun makemove (board piece col)
  (if (member col (legalmoves board))
  	(dotimes (row (- (array-dimension board 1) 1))
   		(if (not (equal (aref board col (+ row 1)) empty))
        	(progn 
           		(setf (aref board col row) piece)
             	(return board))))
   (format t "~%Illegal move")))
  
(defun checkdraw (board)
  (if (equal (legalmoves board) nil)
    t
  	nil))

(defun calculatevalue (board piece)
  (setq anzahlbew (+ anzahlbew 1))
  (let ((value 0))
  "check horizontal"
  (dotimes (row (array-dimension board 1))
    (dotimes (col (- (array-dimension board 0) 3))
      (if (= (aref board col row) piece)
        (progn (setf value (+ value 1)) (if (= (aref board (+ col 1) row) piece)
                           		(progn (setf value (+ value 10)) (if (= (aref board (+ col 2) row) piece)
                                                     (progn (setf value (+ value 100)) (if (= (aref board (+ col 3) row) piece)
                                                                              (return-from calculatevalue winningvalue))))))))))
  "check vertical"
  (dotimes (row (- (array-dimension board 1) 3))
    (dotimes (col (array-dimension board 0))
      (if (= (aref board col row) piece)
          (progn (setf value (+ value 1)) (if (= (aref board col (+ row 1)) piece)
                                 (progn (setf value (+ value 10)) (if (= (aref board col (+ row 2)) piece)
                                                        (progn (setf value (+ value 100)) (if (= (aref board col (+ row 3)) piece)
                                                                                (return-from calculatevalue winningvalue))))))))))
  "check diagonal from bottom left to top right"
  (dotimes (row (- (array-dimension board 1) 3))
    (dotimes (col (array-dimension board 0))
      (if (= (aref board col (+ row 3)) piece)
          (progn (setf value (+ value 1)) (if (= (aref board (+ col 1) (+ row 2)) piece)
                                 (progn (setf value (+ value 10)) (if (= (aref board (+ col 2) (+ row 1)) piece)
                                                          (progn (setf value (+ value 100)) (if (= (aref board (+ col 3) row) piece)
                                                                                   (return-from calculatevalue winningvalue))))))))))
  "check diagonal from top left to bottom right"
  (dotimes (row (- (array-dimension board 1) 3))
    (dotimes (col (- (array-dimension board 0) 3))
      (if (= (aref board col row) piece)
          (progn (setf value (+ value 1)) (if (= (aref board (+ col 1) (+ row 1)) piece)
                                 (progn (setf value (+ value 100)) (if (= (aref board (+ col 2) (+ row 2)) piece)
                                                          (progn (setf value (+ value 100)) (if (= (aref board (+ col 3) (+ row 3)) piece)
                                                                                   (return-from calculatevalue winningvalue))))))))))
  value))

(defun value (board piece)
  (- (calculatevalue board piece) (calculatevalue board (opponent piece))))

(defun checkwin (board piece)
  "check horizontal"
  (dotimes (row (array-dimension board 1))
    (dotimes (col (- (array-dimension board 0) 3))
      (if (and (= (aref board col row) piece)
               (= (aref board (+ col 1) row) piece)
               (= (aref board (+ col 2) row) piece)
               (= (aref board (+ col 3) row) piece))
          (return-from checkwin t))))
  "check vertical"
  (dotimes (row (- (array-dimension board 1) 3))
    (dotimes (col (array-dimension board 0))
      (if (and (= (aref board col row) piece)
               (= (aref board col (+ row 1)) piece)
               (= (aref board col (+ row 2)) piece)
               (= (aref board col (+ row 3)) piece))
          (return-from checkwin t))))
  "check diagonal from bottom left to top right"
  (dotimes (row (- (array-dimension board 1) 3))
    (dotimes (col (array-dimension board 0))
      (if (and (= (aref board col (+ row 3)) piece)
               (= (aref board (+ col 1) (+ row 2)) piece)
               (= (aref board (+ col 2) (+ row 1)) piece)
               (= (aref board (+ col 3) row) piece))
          (return-from checkwin t))))
  "check diagonal from top left to bottom right"
  (dotimes (row (- (array-dimension board 1) 3))
    (dotimes (col (- (array-dimension board 0) 3))
      (if (and (= (aref board col row) piece) 
               (= (aref board (+ col 1) (+ row 1)) piece)
               (= (aref board (+ col 2) (+ row 2)) piece)
               (= (aref board (+ col 3) (+ row 3)) piece))
          (return-from checkwin t))))
  )

(defun drawboard (board)
  (format t "~%Current Board:~%")
  (dotimes (row (array-dimension board 1))
    (dotimes (col (array-dimension board 0))
      (if (not (equal (aref board col row) outer))
          (format t "| ~c " (nameof (aref board col row)))))
    (if (not (or (equal row 0) (equal row 7)))
    	(format t "|~%")))
  (format t "--1---2---3---4---5---6---7--~%"))

(setf *random-state* (make-random-state t))
(defun randomstrategy (board piece)
  (format t "~%Player ~c turn (random).~%" (nameof piece))
  (let ((lst (legalmoves board)))
  (nth (random (length lst)) lst)))

(defun humanstrategy (board piece)
  (format t "~%Player ~c turn, choose column ~a to make a move: ~%" (nameof piece) (legalmoves board))
 	(read))

(defun copyboard (board)
  (let ((board2 (createboard)))
      (dotimes (col (array-dimension board 0))
        (dotimes (row (array-dimension board 1))
          (setf (aref board2 col row) (aref board col row))))
      board2))

(defun minimax (piece board ply evalfn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values."
  (if (= ply 0)
      (funcall evalfn board piece)
      (let ((moves (legalmoves board)))
        (if (null moves)
            (if (legalmoves board)
                (- (minimax (opponent piece) board
                            (- ply 1) evalfn))
                (return-from minimax 0))
            (let ((bestmove nil)
                  (bestval nil))
              (dolist (move moves)
                (let* ((board2 (makemove (copyboard board) piece move))
                       (val (- (minimax
                                 (opponent piece) board2
                                 (- ply 1) evalfn))))
                  (when (or (null bestval)
                            (> val bestval))
                    (setf bestval val)
                    (setf bestmove move))))
              (values bestval bestmove))))))

(defun minimaxsearcher (ply evalfn)
  "A strategy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (board piece)
      (multiple-value-bind (value move)
          (minimax piece board ply evalfn) 
        (declare (ignore value))
        move)))

(defun alphabeta (piece board achievable cutoff ply evalfn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values,
  using cutoffs whenever possible."
  (if (= ply 0)
      (funcall evalfn board piece)
      (let ((moves (legalmoves board)))
        (if (null moves)
            (if (legalmoves board)
                (- (alphabeta (opponent piece) board
                               (- cutoff) (- achievable)
                               (- ply 1) evalfn))
                (return-from alphabeta 0))
            (let ((bestmove (first moves)))
              (loop for move in moves do
                (let* ((board2 (makemove (copyboard board) piece move))
                       (val (- (alphabeta
                                 (opponent piece) board2
                                 (- cutoff) (- achievable)
                                 (- ply 1) evalfn))))
                  (when (> val achievable)
                    (setf achievable val)
                    (setf bestmove move)))
                until (>= achievable cutoff))
              (values achievable bestmove))))))

(defun alphabetasearcher (depth evalfn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  #'(lambda (board piece)
      (multiple-value-bind (value move)
          (alphabeta piece board losingvalue winningvalue depth evalfn) 
        (declare (ignore value))
        move)))

(defun viergewinnt (redstrategy yellowstrategy)
  	(let ((board (createboard)))
    	(loop while (not (checkdraw board)) do
           	(makemove board red (funcall redstrategy board red))
           	(drawboard board)
            (format t "~%~d~%" anzahlbew)
            (setq anzahlbew 0)
           	(if (checkwin board red)
               (progn (format t "~%Red won") (return-from viergewinnt 'end)))
            (makemove board yellow (funcall yellowstrategy board yellow))
           	(drawboard board)
            (format t "~%~d~%" anzahlbew)
            (setq anzahlbew 0)
			(if (checkwin board yellow)
               (progn (format t "~%Yellow won") (return-from viergewinnt 'end))))
   (format t "draw")))


(viergewinnt (minimaxsearcher 6 #'value) (alphabetasearcher 6 #'value))