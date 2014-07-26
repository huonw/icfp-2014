; follows lines of pills, otherwise just searching cyclically.

(include "consts.lisp")
(include "functions.lisp")

(defun main (world undefined)
  (cons 0 step))

(defun step (state world)
  (let ((player (car (cdr world)))
        (map (car world)))
    (let ((player-pos (car (cdr player))))
      (let ((player-x (car player-pos))
            (player-y (cdr player-pos)))
        (let ((player-row (list-nth map player-y)))
          (let ((maybe-pill (check-surrounds map player-row player-x player-y pill-or-better)))
            (cons
             0
             (if (eq maybe-pill -1)
                 (check-surrounds map player-row player-x player-y not-a-wall)
               maybe-pill))))))))

(defun not-a-wall (value)
  (ge value EMPTY))

; don't count the starting places
(defun pill-or-better (value)
  (and (ge value PILL) (ge FRUIT value)))

; call `f' on the cell values in each of the directions, returning the
; first returns true (or -1 if they're all false)
(defun check-surrounds (map player-row player-x player-y f)
  (if (f (list-nth player-row (add player-x 1)))
      RIGHT
    (if (f (list-nth (list-nth map (add player-y 1)) player-x))
        DOWN
      (if (f (list-nth player-row (sub player-x 1)))
          LEFT
        (if (f (list-nth (list-nth map (sub player-y 1)) player-x))
            UP
          -1)))))
