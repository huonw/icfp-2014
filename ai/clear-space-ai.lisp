; an "AI" that cyclically looks for an empty space around the player,
; gets stuck easily.

(include "consts.lisp")
(include "functions.lisp")

(defun main ()
  (cons 0 step))

(defun step (state world)
  (let ((player (car (cdr world)))
        (map (car world)))
    (let ((player-pos (car (cdr player))))
      (let ((player-x (car player-pos))
            (player-y (cdr player-pos)))
        (let ((player-row (list-nth map player-y)))
          (cons
           0
           (if
               (>= (list-nth player-row (+ player-x 1)) EMPTY)
               RIGHT
             (if
                 (>= (list-nth (list-nth map (+ player-y 1)) player-x) EMPTY)
                 DOWN
               (if (>= (list-nth player-row (- player-x 1)) EMPTY)
                   LEFT
                 UP)))))))))
