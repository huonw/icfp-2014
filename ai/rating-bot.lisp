(include "consts.lisp")
(include "functions.lisp")
(include "ghost.lisp")

(const WALL_PENALTY -1000000)
(const GHOST_0_PENALTY -5000000)
(const GHOST_1_PENALTY -10000)
(const GHOST_2_PENALTY -1000)
(const GHOST_MANHATTAN_PENALTY -1000000)
(const PILL_BONUS 100)
(const PILL_DRIFT_BONUS 50)
(const FRUIT_DRIFT_BONUS 1000)
(const RECENTLY_VISITED_PENALTY_MULTIPLIER -5)

(defun main (world undefined)
  (let ((fruit-and-pills (pills (car world))))
    (cons (cons fruit-and-pills (make-list 200 (cons 0 0))) step)))

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
        (fruit-and-pills (car state))
        (fruit-timer (cdr (cdr (cdr world)))))
  (let ((fruit-pos (car fruit-and-pills))
        (pills (cdr fruit-and-pills))
        (player-pos (car (cdr player))) (player-dir (tuple-nth player 5 2)))
  (let ((player-x (car player-pos))
        (player-y (cdr player-pos))
        (ghost-0 (generate-ghost-pos-0 ghost-info))
        (ghost-1 (generate-ghost-pos-1 ghost-info))
        (ghost-2 (generate-ghost-pos-2 ghost-info)))
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
          (drift-bonus PILL_DRIFT_BONUS))
    (if fruit-timer
      (do 
        (set drift-bonus FRUIT_DRIFT_BONUS)
        (set closest-pill-x (car fruit-pos))
        (set closest-pill-y (cdr fruit-pos))
      )
      ; no fruit
      (do
        (until (atom pills)
          (let ((pill (car pills)))
          (let ((pill-x (car pill)) (pill-y (cdr pill)))
            (if (pill-or-better (2d-nth world-map pill-x pill-y)) ; pill still there
              ; use this pill
              (do
                (let ((dist (+ (abs (- pill-x player-x)) (- pill-y player-y))))
                  (if (> min-distance dist) (do (set min-distance dist) (set closest-pill-x pill-x) (set closest-pill-y pill-y)) (pass))
                )
                (set new-pills (cons pill new-pills))
              )
              (pass))
            (set pills (cdr pills))
          )))
        (set pills new-pills)
      )
    )
  
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
    (let
          ((places
           (cons (cons (dir-with-xy-v UP) (lambda (d) (set up (+ up d))))
           (cons (cons (dir-with-xy-v DOWN) (lambda (d) (set down (+ down d))))
           (cons (cons (dir-with-xy-v LEFT) (lambda (d) (set left (+ left d))))
           (cons (cons (dir-with-xy-v RIGHT) (lambda (d) (set right (+ right d))))
                 0)))))
          ; these do computations on each of the (((dir, value), xy), function) pairs
          ; above, calling the function with an adjustment to make for
          ; the score of that direction
          (rating-functions
            (cons (lambda (s)
              (let ((value (cdr (car (car s)))))
                (if (not-a-wall value)
                  (pass) ((cdr s) WALL_PENALTY)))) ; wall test
            (cons (lambda (s) (let ((value (cdr (car (car s)))))(if (pill-or-better value) ((cdr s) PILL_BONUS) (pass))))
            ;(cons (lambda (s) (let ((value (cdr (car (car s)))) (xy (cdr (car s))) (score (cdr s)))
            ;  (if (point-not-in-list xy ghost-0)
            ;    (if (point-not-in-list xy ghost-1)
            ;      (if (point-not-in-list xy ghost-2) (pass) (score GHOST_2_PENALTY)) ; resp. safe, 2 away
            ;      (score GHOST_1_PENALTY)) ; 1 away from ghost
            ;    (score GHOST_0_PENALTY)) ; square is a ghost
            ;  ))
            (cons (lambda (s)
              ((cdr s)
                (* (count recently-visited (cdr (car s)))
                   RECENTLY_VISITED_PENALTY_MULTIPLIER))
            ) ; recently visited cells
            (cons (lambda (s) (for-each
              (lambda (ghost)
                (let ((ghost-pos (car (cdr ghost)))
                      (ghost-dir (cdr (cdr ghost)))
                      (ghost-status (car ghost))
                      (score (cdr s))
                      (xy (cdr (car s))))
                (let ((dist (+ 1 (abs (- (car ghost-pos) (car xy))) (abs (- (cdr ghost-pos) (cdr xy))))))
                  (score (/ (/ GHOST_MANHATTAN_PENALTY (* dist dist dist dist dist dist)) 
                            (if (= ghost-status G_STANDARD) 1 (if (= ghost-status G_FRIGHT) -1 2))))
                ))
              )
            ghost-info))
           ; ...
             0)))))
          )
      ; run each of those on each dimension
      (for-each
       (lambda (score)
         (for-each
          (lambda (rate) (rate score))
          rating-functions))
       places)
      ; find the best one
      ;(dbug (cons (cons up down) (cons left right)))
      (let ((m (max up (max down (max left right)))))
        (cons (cons (cons fruit-pos pills) (update-list recently-visited player-pos)) (if (= m up) UP
          (if (= m down) DOWN
            (if (= m left) LEFT
              RIGHT))))
      ))))))))

(defun update-list (list elem) (push-back (cdr list) elem))

(defun pills (world)
  (__pills-inner (car world) (cdr world) 0 0)
)

(defun __pills-inner (row future_rows x y)
  (if (atom row)
    (if (atom future_rows) (cons 0 0) (__pills-inner (car future_rows) (cdr future_rows) 0 (+ y 1)))
    (let ((future_pills (__pills-inner (cdr row) future_rows (+ x 1) y))
          (cell_status (car row)))
    (let ((actual_pills (cdr future_pills)) (fruit (car future_pills)))
      (if (if (= cell_status PILL) 1 (= cell_status POWER_PILL))
        (cons fruit (cons (cons x y) actual_pills)) ; pill
        (if (= cell_status FRUIT)
          (cons (cons x y) actual_pills)
          future_pills ; no changes at all
        )
      )
    ))
  )
)
