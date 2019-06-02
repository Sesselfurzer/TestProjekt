(defvar goal (make-array '(3 3) :initial-contents
                         '((0 1 2)
  			  			           (3 4 5) 
      		  			         (6 7 8))))

(defun creategoal (n)
  (let ((puzzle (make-array (list n n) :initial-element 0)))
    (dotimes (col n)
      (dotimes (row n)
        (setf (aref puzzle col row) (+ (* col n) row))))
    (setq goal puzzle)))

(defun goalp (puzzle)
  (dotimes (col (array-dimension puzzle 1))
    (dotimes (row (array-dimension puzzle 0))
      (if (not (equal (aref puzzle col row) (aref goal col row)))
          (return-from goalp nil))))
  t)

(defun checktop (puzzle col row)
  (if (equal row 0)
      nil
      (if (equal (aref puzzle col (- row 1)) 0)
          t
          nil)))

(defun checkbot (puzzle col row)
  (if (equal row (- (array-dimension puzzle 0) 1))
      nil
      (if (equal (aref puzzle col (+ row 1)) 0)
          t
          nil)))

(defun checkleft (puzzle col row)
  (if (equal col 0)
      nil
      (if (equal (aref puzzle (- col 1) row) 0)
          t
          nil)))

(defun checkright (puzzle col row)
  (if (equal col (- (array-dimension puzzle 1) 1))
      nil
      (if (equal (aref puzzle (+ col 1) row) 0)
          t
          nil)))

(defun checklegalmove (puzzle col row)
  (if (or 
        (checktop puzzle col row)
        (checkbot puzzle col row)
        (checkleft puzzle col row)
        (checkright puzzle col row))
      t
      nil))

(defun drawpuzzle (puzzle)
  (format t "~%Current Puzzle:~%")
  (dotimes (col (array-dimension puzzle 1))
    (dotimes (row (array-dimension puzzle 0))
      (if (equal (aref puzzle col row) 0)
          (format t "   ")
          (format t "~2d " (aref puzzle col row))))
    (format t "~%")))

(defun numberposition (puzzle number)
  (dotimes (col (array-dimension puzzle 1))
    (dotimes (row (array-dimension puzzle 0))
      (if(equal (aref puzzle col row) number)
          (return-from numberposition (cons col row))))))

(defun legalmoves (puzzle)
  (let ((legalmoves '()))
    (dotimes (col (array-dimension puzzle 1))
      (dotimes (row (array-dimension puzzle 0))
        (if (checklegalmove puzzle col row)
            (setf legalmoves (cons (aref puzzle col row) legalmoves)))))
    (if (equal legalmoves '())
        nil
        legalmoves)))

(defun makemove (puzzle number)
  (if (member number (legalmoves puzzle))
    (progn 
      (let ((col (car (numberposition puzzle number)))
            (row (cdr (numberposition puzzle number))))
  	  (if (checktop puzzle col row)
       	(rotatef (aref puzzle col (- row 1)) (aref puzzle col row))
       	(if (checkbot puzzle col row)
       	  (rotatef (aref puzzle col (+ row 1)) (aref puzzle col row))
          (if (checkleft puzzle col row)
       		(rotatef (aref puzzle (- col 1) row) (aref puzzle col row))
         	(if (checkright puzzle col row)
       	  		(rotatef (aref puzzle (+ col 1) row) (aref puzzle col row))
            	(format t "~%Illegal Move~%")))))))
 	(format t "~%Illegal Move~%")))

(setf *random-state* (make-random-state t))
(defun randommoves (puzzle n)
  (dotimes (m n)
    (let ((lst (legalmoves puzzle)))
    (makemove puzzle (nth (random (length lst)) lst))))
  puzzle)

(defun createpuzzle (n moves)
  (creategoal n)
  (let ((puzzle (make-array (list n n) :initial-element 0)))
    (dotimes (col n)
      (dotimes (row n)
        (setf (aref puzzle col row) (+ (* col n) row))
        ))
    (randommoves puzzle moves)
    puzzle))

(defun copypuzzle (puzzle)
  (let ((puzzle2 (createpuzzle (array-dimension puzzle 0) 0)))
      (dotimes (col (array-dimension puzzle 0))
        (dotimes (row (array-dimension puzzle 1))
          (setf (aref puzzle2 col row) (aref puzzle col row))))
      puzzle2))

(defun legalpuzzles (puzzle)
  (let ((copy (copypuzzle puzzle))
        (legalpuzzles '()))
    (loop for move in (legalmoves puzzle) do
          (makemove copy move)
          (setf legalpuzzles (cons copy legalpuzzles))
          (setf copy (copypuzzle puzzle)))
    legalpuzzles))

(defun distance (puzzle number)
  (let ((actualPosition (numberposition puzzle number))
        (goalPosition (numberposition goal number)))
    (+ (abs (- (car actualPosition) (car goalPosition))) (abs (- (cdr actualPosition) (cdr goalPosition))))))

(defun manhattendistance (puzzle)
  (let ((result 0))
    (dotimes (row (array-dimension puzzle 0))
      (dotimes (col (array-dimension puzzle 1))
            (setf result (+ (distance puzzle (aref puzzle row col)) result))))
    result))

(defun checkwrongspots (puzzle)
  (let ((result 0))
    (dotimes (col (array-dimension puzzle 1))
      (dotimes (row (array-dimension puzzle 0))
        (if (not (equal (aref puzzle col row) (aref goal col row)))
            (setf result (+ result 1)))))
    result))

(defstruct (path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
          (path-state path) (path-total-cost path)))

(defun findpath (state paths state=)
  "Find the path with this state among a list of paths."
  (find state paths :key #'path-state :test state=))

(defun betterpath (path1 path2)
  "Is path1 cheaper than path2?"
  (< (path-total-cost path1) (path-total-cost path2)))

(defun insertpath (path paths)
  "Put path into the right position, sorted by total cost."
  ;; MERGE is a built-in function
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun a*-search (paths goal-p successors cost-fn cost-left-fn
                  &optional (state= #'eql) old-paths)
  "Find a path whose state satisfies goal-p.  Start with paths,
  and expand successors, exploring least cost first.
  When there are duplicate states, keep the one with the
  lower cost and discard the other."
  (cond
    ((null paths) nil)
    ((funcall goal-p (path-state (first paths)))
     (drawpuzzle (path-state (first paths)))
     (values (first paths) paths))
    (t (let* ((path (pop paths))
              (state (path-state path)))
         ;; Update PATHS and OLD-PATHS to reflect
         ;; the new successors of STATE:
         (setf old-paths (insertpath path old-paths))
         (dolist (state2 (funcall successors state))
           (let* ((cost (+ (path-cost-so-far path)
                           (funcall cost-fn)))
                  (cost2 (funcall cost-left-fn state2))
                  (path2 (make-path
                           :state state2 :previous path
                           :cost-so-far cost
                           :total-cost (+ cost cost2)))
                  (old nil))
             ;; Place the new path, path2, in the right list:
             (cond
               ((setf old (findpath state2 paths state=))
                (when (betterpath path2 old)
                  (setf paths (insertpath
                                path2 (delete old paths)))))
               ((setf old (findpath state2 old-paths state=))
                (when (betterpath path2 old)
                  (setf paths (insertpath path2 paths))
                  (setf old-paths (delete old old-paths))))
               (t (setf paths (insertpath path2 paths))))))
         ;; Finally, call A* again with the updated path lists:
         (a*-search paths goal-p successors cost-fn cost-left-fn
                    state= old-paths)))))

(defun costfn()
  1)

(defun main(puzzle)
  (let ((paths (make-path
                 :state puzzle :previous nil
                 :cost-so-far 0
                 :total-cost 0)))
    (drawpuzzle puzzle)
    (write (a*-search (list paths) #'goalp #'legalpuzzles #'costfn #'manhattendistance nil))))


(main (createpuzzle 3 20))