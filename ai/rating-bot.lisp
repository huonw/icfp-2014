(include "consts.lisp")
(include "functions.lisp")
(include "ghost.lisp")

(const WALL_PENALTY -1500000)
(const GHOST_0_PENALTY -5000000)
(const GHOST_1_PENALTY -10000)
(const GHOST_2_PENALTY -1000)
(const GHOST_MANHATTAN_PENALTY -900000)
(const FRIGHT_GHOST_MANHATTAN_BONUS 1000000)
(const PILL_BONUS 100)
(const GHOST_CLOSE_PILL_BONUS 0)
(const POWER_PILL_BONUS 100000)
(const FRUIT_BONUS 1000)
(const PILL_DRIFT_BONUS 50)
(const FRUIT_DRIFT_BONUS 1000)
(const RECENTLY_VISITED_PENALTY_MULTIPLIER -20)
(const CLOSE_GHOST_POWER_PILL_DRIFT_BONUS 400)
(const DOUBLE_GHOST_CLOSE_THRESHOLD 6)

(defun main (world undefined)
  (cons
   (cons
    (find-fruit-and-walls (car world))
    (make-list 200 (cons 0 0)))
   step))

(defun for-each (f list)
  (if (atom list)
      (pass)
    (do (f (car list)) (for-each f (cdr list)))))

(defun max (a b) (if (>= a b) a b))

(defun step (state world)
  (let ((player (car (cdr world)))
        (world-map (car world))
        (ghost-info (tuple-nth world 4 2))
        (recently-visited (cdr state))
        (fruit-pos (car state))
        ;(wall-list (cdr (car state)))
        (fruit-timer (cdr (cdr (cdr world)))))
  (let ((all-pills (pills world-map))
        (player-pos (car (cdr player)))
        (player-dir (tuple-nth player 5 2)))
  (let ((power-pills (car all-pills))
        (pills (cdr all-pills)))
  (let ((player-x (car player-pos))
        (player-y (cdr player-pos))
        )
  (let ((dir-with-xy-v
    (lambda (dir)
      (let ((new-pos (inc player-x player-y dir)))
        (cons (cons dir (2d-nth world-map (car new-pos) (cdr new-pos))) new-pos)))))
    (let ((up 0)
          (down 0)
          (left 0)
          (right 0)
          (new-pills 0)
          (min-distance 1000000)
          (closest-pill-x 0)
          (closest-pill-y 0)
          (drift-bonus PILL_DRIFT_BONUS)
          (dist 0)
          (pill 0)
          (pill-x 0)
          (pill-y 0)
          (ghost-close 0)
          (ghost-pos 0)
          (cell-value 0)
          (ghost-x 0) (ghost-y 0)
          (mul-ghost-x 0) (mul-ghost-y 0)
          (ghost-dir 0) (ghost-status 0) (xy 0)
          (candidate-x 0) (candidate-y 0)
          (mul-candidate-x 0) (mul-candidate-y 0))
    (let
          ((places
           (cons (cons (dir-with-xy-v UP) (cons (lambda (d) (set up (+ up d))) (lambda () up)))
           (cons (cons (dir-with-xy-v DOWN) (cons (lambda (d) (set down (+ down d))) (lambda () down)))
           (cons (cons (dir-with-xy-v LEFT) (cons (lambda (d) (set left (+ left d))) (lambda () left)))
           (cons (cons (dir-with-xy-v RIGHT) (cons (lambda (d) (set right (+ right d))) (lambda () right)))
                 0)))))
          ; these do computations on each of the (((dir, value), xy), function) pairs
          ; above, calling the function with an adjustment to make for
          ; the score of that direction
          (rating-functions
            (cons (lambda (dir-xy-v score)
                    (if (not-a-wall (cdr (car dir-xy-v)))
                        (pass)
                      (score WALL_PENALTY))) ; wall test

            (cons (lambda (dir-xy-v score)
              (score
                (* (count-with-index-weighting recently-visited (cdr dir-xy-v))
                   RECENTLY_VISITED_PENALTY_MULTIPLIER))
            ) ; recently visited cells

            (cons
             (lambda (dir-xy-v score)
               (if (not-a-wall (cdr (car dir-xy-v)))
                   (for-each
                    (lambda (ghost)
                      (set ghost-pos (car (cdr ghost)))
                      (set ghost-x (car ghost-pos))
                      (set ghost-y (cdr ghost-pos))
                      (set mul-ghost-x (* 2 ghost-x))
                      (set mul-ghost-y (* 2 ghost-y))
                      (set ghost-dir (cdr (cdr ghost)))
                      (set ghost-status (car ghost))
                      (set xy (cdr dir-xy-v))
                      (set candidate-x (car xy))
                      (set candidate-y (cdr xy))
                      (set mul-candidate-x (* 2 candidate-x))
                      (set mul-candidate-y (* 2 candidate-y))

                      (set dist (+ (abs (- mul-ghost-x mul-candidate-x))
                                   (abs (- mul-ghost-y mul-candidate-y))))
                      (if (= ghost-status G_FRIGHT)
                          (pass)
                        (do
                            (if (> DOUBLE_GHOST_CLOSE_THRESHOLD dist)
                                (set ghost-close 1)
                              (pass))

                            (if (> mul-ghost-x mul-candidate-x)
                                (if (= ghost-dir RIGHT)
                                    (set mul-ghost-x (+ mul-ghost-x 1))
                                  (if (not-a-wall (2d-nth world-map (+ candidate-x 1) candidate-y))
                                      (pass)
                                    (set mul-ghost-x (+ mul-ghost-x 2))))
                              (if (> mul-candidate-x mul-ghost-x)
                                  (if (= ghost-dir LEFT)
                                      (set mul-ghost-x (- mul-ghost-x 1))
                                    (if (not-a-wall
                                         (2d-nth world-map (- candidate-x 1) candidate-y))
                                        (pass)
                                      (set mul-ghost-x (- mul-ghost-x 2))))
                                (pass)))
                          (if (> mul-ghost-y mul-candidate-y)
                              (if (= ghost-dir DOWN)
                                  (set mul-ghost-y (+ mul-ghost-y 1))
                                (if (not-a-wall (2d-nth world-map candidate-x (+ candidate-y 1)))
                                    (pass)
                                  (set mul-ghost-y (+ mul-ghost-y 2))))
                            (if (> mul-candidate-y mul-ghost-y)
                                (if (= ghost-dir UP)
                                    (set mul-ghost-y (- mul-ghost-y 1))
                                  (if (not-a-wall (2d-nth world-map candidate-x (- candidate-y 1)))
                                      (pass)
                                    (set mul-ghost-y (- mul-ghost-y 2))))
                              (pass)))

                          (set dist (+ (abs (- mul-ghost-x mul-candidate-x))
                                       (abs (- mul-ghost-y mul-candidate-y))))
                          ))
                      (set dist (+ 1 (/ dist 2)))


                      (if (= ghost-status G_FRIGHT)
                          (score (/ FRIGHT_GHOST_MANHATTAN_BONUS (* dist dist dist dist)))
                        (score (/ GHOST_MANHATTAN_PENALTY (* dist dist dist dist dist dist))))
                      )
                    ghost-info)
                 (pass)))
           (cons (lambda (dir-xy-v score)
                   (set cell-value (cdr (car dir-xy-v)))
                   (if (= cell-value PILL)
                       (score (if ghost-close GHOST_CLOSE_PILL_BONUS PILL_BONUS ))
                     (if (and (= cell-value POWER_PILL) ghost-close)
                         (score POWER_PILL_BONUS)
                         (if (and (= cell-value FRUIT) fruit-timer)
                             (score FRUIT_BONUS)
                             (pass)))))
           ; ...
             0))))))

      ; run each of those on each dimension
      (for-each
       (lambda (score)
         (for-each
          (lambda (rate)
            (if (> -1000000 ((cdr (cdr score))))
              (pass)
              (rate (car score) (car (cdr score))))
            )
          rating-functions))
       places)


    (if fruit-timer
      (do
        (set drift-bonus FRUIT_DRIFT_BONUS)
        (set closest-pill-x (car fruit-pos))
        (set closest-pill-y (cdr fruit-pos))
      )
      ; no fruit
      (let ((pills
             (if (and ghost-close (atom power-pills))
                 (do (set drift-bonus CLOSE_GHOST_POWER_PILL_DRIFT_BONUS)
                     power-pills)
               pills)))
        (until (atom pills)
               (set pill (car pills))
               (set pill-x (car pill))
               (set pill-y (cdr pill))
                                        ; use this pill
               (set dist (+ (abs (- pill-x player-x)) (- pill-y player-y)))
               (if (> min-distance dist)
                   (do
                       (set min-distance dist)
                       (set closest-pill-x pill-x)
                     (set closest-pill-y pill-y))
                 (pass))
               (set pills (cdr pills))
               )))

    (if (> closest-pill-x player-x)
      (set right (+ right drift-bonus))
      (if (> player-x closest-pill-x)
        (set left (+ left drift-bonus))
        (pass)
      )
    )

    (if (> closest-pill-y player-y)
      (set down (+ down drift-bonus))
      (if (> player-y closest-pill-y)
        (set up (+ up drift-bonus))
        (pass)
      )
    )

      ; find the best one
      ;(dbug (cons (cons up down) (cons left right)))
      (let ((m (max up (max down (max left right)))))
        (cons
         (cons
          ;(cons fruit-pos wall-list)
          fruit-pos
          (update-list recently-visited player-pos))
         (if (= m up) UP
          (if (= m down) DOWN
            (if (= m left) LEFT
              RIGHT))))
      )))))))))

(defun update-list (list elem) (push-back (cdr list) elem))


(defun find-fruit-and-walls (world)
  (let
      ((row (car world))
       (future_rows (cdr world))
       (x 0) (y 0)
       (cell_status 0)
       (fruit_cell 0))
    (while (atom fruit_cell)
     (if (atom row)
         (do
             (set row (car future_rows))
             (set future_rows (cdr future_rows))
           (set x 0)
           (set y (+ y 1))
           )
       (pass))

     (if (= (car row) FRUIT)
         (set fruit_cell (cons x y))
       (pass))
     (set row (cdr row))
     (set x (+ x 1)))
    fruit_cell
  ))

(defun pills (world)
  (let
      ((row (car world))
       (future_rows (cdr world))
       (x 0) (y 0)
       (pills 0)
       (power-pills 0))
    ; stop when both are empty
    (until (if (atom row) (atom future_rows) 0)
     (if (atom row)
         (do
             (set row (car future_rows))
             (set future_rows (cdr future_rows))
           (set x 0)
           (set y (+ y 1))
           )
       (pass))
     (if (= (car row) PILL)
         (set pills (cons (cons x y) pills))
       (if (= (car row) POWER_PILL)
           (set power-pills (cons (cons x y) power-pills))
         (pass)))
     (set row (cdr row))
     (set x (+ x 1)))

    (cons power-pills pills)))
