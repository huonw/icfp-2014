; follows lines of pills, otherwise just searching cyclically.

(include "consts.lisp")
(include "functions.lisp")
(include "map-functions.lisp")

(defun main (world undefined)
  (let ((pills (pills world)))
    (cons
      (cons 0 (cons pills (make-list 200 (cons 0 0))))
      step)))

(defun step (state world)
  (let ((tick (car state))
        (pills (car (cdr state))
        (last-visited (cdr (cdr state)))
        (player (car (cdr world)))
        (world-map (car world))
        (ghost-info (tuple-nth world 4 2)))

    (let ((player-pos (car (cdr player)))
          (ghosts (generate-ghost-pos ghost-info)))

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
                          ; no unvisited cell not occupied by ghosts
                          (let
                              ; check for open squares (hopefully avoiding ghosts)
                              ((maybe-no-ghost
                                (check-surrounds world-map player-row
                                                 player-x player-y
                                                 (lambda (value x y)
                                                   (check-and-not-old-or-ghost not-a-wall
                                                                           value x y
                                                                           0
                                                                           ghosts)))))
                           (if (atom maybe-no-ghost)
                              ; none of the the ghostless open squares, find distant pill to bear towards
                              (let ((result (choose-direction-and-update-pills pills player-pos world-map)))
                                (set pills (cdr result))
                                (let ((maybe-distant-pill (car result)))
                                  (if (eq maybe-distant-pill -1)
                                    ; no pills left seemingly? worrying... anyway find point
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
                                      (dbug 101910912)
                                      (dbug player-pos)
                                      (cons ; TODO: default to choose-direction's choice (and update `pills` in line with what choose-direction says)
                                       (let ((point-x (car point)) (point-y (cdr point)))
                                         (if (> point-x player-x) RIGHT
                                           (if (> player-x point-x) LEFT
                                             (if (> point-y player-y) DOWN
                                               UP))))
                                       point))
                                    maybe-distant-pill)))
                             maybe-no-ghost))
                          maybe-new))
                    maybe-pill))))
            (cons
             (cons (+ tick 1) (push-back (cdr last-visited) (cdr move)))
             (car move)))))))))

(defun choose-direction-and-update-pills (pills player-pos map)
  (cons ; big indented expr block returns a direction (-1 for failure)
    (if (atom pills) -1 ; fail: just pick a direction
      (let (pill (car pills))
        (set pills (cdr pills)) ; this one is now shotgunned (regardless of whether it remains a pill or not)
        (let ((pill-x (car pill)) (pill-y (cdr pill)))
          (if (pill-or-better (2d-nth map pill-x pill-y)) ; pill still there
            (let ((player-x (car player-pos)) (player-y (cdr player-pos)))
              ; choose direction taking us towards a pill
              (let ((abs-x (abs (- player-x pill-x))) (abs-y (abs (- player-y pill-y))))
                (if (>= abs-x abs-y)
                  (if (>= pill-x player-x) RIGHT LEFT)
                  (if (>= pill-y player-y) DOWN UP)
                )
              )
            )
            ; else move onto next pill
            (let ((result (choose-direction-and-update-pills (cdr pills) player-pos map)))
              (set pills (cdr result))
              (car result) ; return the direction chosen by recursive call
            )
          )
        )
      )
    )
  pills)) ; return (cons direction new-pill-list) pair


(defun generate-ghost-pos (ghost-info)
  (if (atom ghost-info) 0
    (let ((ghost (car ghost-info)))
      (let ((ghost-pos (car (cdr ghost))))
        (let ((x (car ghost-pos)) (y (cdr ghost-pos)))
          (cons ghost-pos
            (cons (cons (+ x 1) y)
            (cons (cons (- x 1) y)
            (cons (cons x (+ y 1))
            (cons (cons x (- y 1))
            (cons (cons (+ x 2) y)
            (cons (cons (- x 2) y)
            (cons (cons x (+ y 2))
            (cons (cons x (- y 2))
            (cons (cons (+ x 1) (+ y 1))
            (cons (cons (+ x 1) (- y 1))
            (cons (cons (- x 1) (+ y 1))
            (cons (cons (- x 1) (- y 1))
                (generate-ghost-pos (cdr ghost-info))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

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
