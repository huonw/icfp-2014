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
        (world-map (car world))
        (ghost-info (tuple-nth world 4 2)))

    (let ((player-pos (car (cdr player)))
          (ghosts (map (lambda (x) (tuple-nth x 3 1)) ghost-info)))

      (let ((player-x (car player-pos))
            (player-y (cdr player-pos)))
        (let ((player-row (list-nth world-map player-y)))

          (let
              ((move
                (let
                    ; check for nearby pills (hopefully avoiding ghosts)
                    ((maybe-pill
                      (check-surrounds world-map player-row player-x player-y
                                       (lambda (value x y)
                                         (check-and-not-old-or-ghost pill-or-better
                                                                     value x y
                                                                     last-visited
                                                                     ghosts)))))
                  (if (atom maybe-pill)
                      ; no pill nearby
                      (let
                          ; check for open squares we haven't visited
                          ; recently (hopefully avoiding ghosts)
                          ((maybe-new
                            (check-surrounds world-map player-row
                                             player-x player-y
                                             (lambda (value x y)
                                               (check-and-not-old-or-ghost not-a-wall
                                                                       value x y
                                                                       last-visited
                                                                       ghosts)))))
                        (if (atom maybe-new)
                            ; none of the them, so find the point
                            ; we've visited the least recently
                            (let ((point
                                   (car
                                    (cdr
                                     ; the points are ordered from
                                     ; least to most recent, so find
                                     ; shortcircuiting on the first
                                     ; one is perfect
                                     (find
                                      (lambda (xy)
                                        (let ((x (car xy)) (y (cdr xy)))
                                        ; check if this point is next to us.
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

; don't count the starting places
(defun pill-or-better (value)
  (and (>= value PILL) (>= POWER_PILL value)))

(defun check-and-not-old-or-ghost (f value x y old-points ghosts)
  (let ((point (cons x y)))
    (if (f value)
        (if (point-not-in-list point ghosts)
            (point-not-in-list (cons x y) old-points)
          0)
      0)))

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
