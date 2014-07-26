; follows lines of pills, otherwise just searching cyclically.

(include "consts.lisp")
(include "functions.lisp")

(defun main (world undefined)
  (cons
   (cons 0 (make-list 200 (cons 0 0)))
   step))

(defun step (state world)
  (let ((tick (car state))
        (last-visited (cdr state))
        (player (car (cdr world)))
        (map (car world)))
    (let ((player-pos (car (cdr player))))
      (let ((player-x (car player-pos))
            (player-y (cdr player-pos)))
        (let ((player-row (list-nth map player-y)))
          (let
              ((move
                (let
                    ((maybe-pill
                      (check-surrounds map player-row player-x player-y pill-or-better)))
                  (if (atom maybe-pill)
                      (let
                          ((maybe-new
                            (check-surrounds map player-row
                                             player-x player-y
                                             (lambda (value x y)
                                               (not-a-wall-or-old value x y last-visited)))))
                        (if (atom maybe-new)
                            (let ((point
                                   (car (cdr (find
                                              (lambda (xy)
                                                (let ((x (car xy)) (y (cdr xy)))
                                        ; check if this point is near us now
                                                  (or (and (= x player-x)
                                                        (= 1 (abs (- y player-y))))
                                                     (and (= 1 (abs (- x player-x)))
                                                        (= y player-y)))))
                                              last-visited)))))
                              (cons
                               (let ((point-x (car point)) (point-y (cdr point)))
                                 (if (> point-x player-x) RIGHT
                                   (if (> player-x point-x) LEFT
                                     (if (> point-y player-y) DOWN
                                       UP))))
                               point))
                          maybe-new))
                    maybe-pill))))
            (cons
             (cons (+ tick 1) (push-back (cdr last-visited) (cdr move)))
             (car move))))))))

(defun not-a-wall (value)
  (>= value EMPTY))

(defun not-a-wall-or-old (value x y old-points)
  (if (not-a-wall value)
      (point-not-in-list (cons x y) old-points)
    0))

; don't count the starting places
(defun pill-or-better (value x y)
  (and (>= value PILL) (>= POWER_PILL value)))

; call `f' on the cell values in each of the directions, returning the
; first returns true (or -1 if they're all false)
(defun check-surrounds (map player-row player-x player-y f)
  (let ((x (+ player-x 1)))
    (if (f (list-nth player-row x) x player-y)
      (cons RIGHT (cons x player-y))

      (let ((y (+ player-y 1)))
        (if (f (list-nth (list-nth map y) player-x) player-x y)
        (cons DOWN (cons player-x y))

        (let ((x (- player-x 1)))
          (if (f (list-nth player-row x) x player-y)
          (cons LEFT (cons x player-y))

          (let ((y (- player-y 1)))
            (if (f (list-nth (list-nth map y) player-x) player-x y)
                (cons UP (cons player-x y))
              0)))))))))
