(include "consts.lisp")
(include "functions.lisp")

(const WALL_PENALTY -1000000)

(defun main (world undefined)
  (cons 0 step))

(defun for-each (f list)
  (if (atom list)
      (pass)
    (do (f (car list)) (for-each f (cdr list)))))

(defun max (a b) (if (>= a b) a b))

(defun step (state world)
  (let ((player (car (cdr world)))
        (world-map (car world))
        (ghost-info (tuple-nth world 4 2)))
  (let ((player-pos (car (cdr player))) (player-dir (tuple-nth player 5 2)))
  (let ((player-x (car player-pos))
        (player-y (cdr player-pos)))
  (let ((dir-with-xy (lambda (dir) (cons dir (inc player-x player-y dir)))))
    (let ((up 0)
          (down 0)
          (left 0)
          (right 0))
    (let
          ((places
           (cons (cons (dir-with-xy UP) (lambda (d) (set up (+ up d))))
           (cons (cons (dir-with-xy DOWN) (lambda (d) (set down (+ down d))))
           (cons (cons (dir-with-xy LEFT) (lambda (d) (set left (+ left d))))
           (cons (cons (dir-with-xy RIGHT) (lambda (d) (set right (+ right d))))
                 0)))))
          ; these do computations on each of the (dir, function) pairs
          ; above, calling the function with an adjustment to make for
          ; the score of that direction
          (rating-functions
           (cons (lambda (s)
            (dbug s)
            (let ((xy (cdr (car s))))
              (if (not-a-wall (2d-nth world-map (car xy) (cdr xy)))
                (pass) ((cdr s) WALL_PENALTY)))) ; wall test
           ;(cons (lambda (s) ...)
           ;(cons (lambda (s) ...)
           ; ...
             0))
          )
      ; run each of those on each dimension
      ;(dbug places)
      (for-each
       (lambda (score)
         (for-each
          (lambda (rate) (rate score))
          rating-functions))
       places)
      ; find the best one
      (let ((m (max up (max down (max left right)))))
        (cons state (if (= m up) UP
          (if (= m down) DOWN
            (if (= m left) LEFT
              RIGHT))))
      ))))))))
