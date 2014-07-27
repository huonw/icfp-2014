(include "consts.lisp")
(include "functions.lisp")
(include "ghost.lisp")

(const WALL_PENALTY -1000000)
(const GHOST_0_PENALTY -5000000)
(const GHOST_1_PENALTY -10000)
(const GHOST_2_PENALTY -500)
(const PILL_BONUS 100)
(const RECENTLY_VISITED_PENALTY_MULTIPLIER -50)

(defun main (world undefined)
  (cons (make-list 20 (cons 0 0)) step))

(defun for-each (f list)
  (if (atom list)
      (pass)
    (do (f (car list)) (for-each f (cdr list)))))

(defun max (a b) (if (>= a b) a b))

(defun step (state world)
  (let ((player (car (cdr world)))
        (world-map (car world))
        (ghost-info (tuple-nth world 4 2))
        (recently-visited state))
  (let ((player-pos (car (cdr player))) (player-dir (tuple-nth player 5 2)))
  (let ((player-x (car player-pos))
        (player-y (cdr player-pos))
        (ghost-1 (generate-ghost-pos-1 ghost-info))
        (ghost-2 (generate-ghost-pos-2 ghost-info)))
  (let ((dir-with-xy-v
    (lambda (dir)
      (let ((new-pos (inc player-x player-y dir)))
        (cons (cons dir (2d-nth world-map (car new-pos) (cdr new-pos))) new-pos)))))
    (let ((up 0)
          (down 0)
          (left 0)
          (right 0))
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
            (cons (lambda (s) (let ((value (cdr (car (car s)))) (xy (cdr (car s))) (score (cdr s)))
              (if (= value GHOST_START) (score GHOST_0_PENALTY) ; square is a ghost
                (if (point-not-in-list xy ghost-1)
                  (if (point-not-in-list xy ghost-2) (pass) (score GHOST_2_PENALTY)) ; resp. safe, 2 away
                  (score GHOST_1_PENALTY))) ; 1 away from ghost
              ))
            (cons (lambda (s)
              ((cdr s)
                (* (count recently-visited (cdr (car s)))
                   RECENTLY_VISITED_PENALTY_MULTIPLIER))
            ) ; recently visited cells
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
        (cons (update-list recently-visited player-pos) (if (= m up) UP
          (if (= m down) DOWN
            (if (= m left) LEFT
              RIGHT))))
      ))))))))

(defun update-list (list elem) (push-back (cdr list) elem))