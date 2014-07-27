; follows lines of pills, otherwise just searching cyclically.

(include "consts.lisp")
(include "functions.lisp")

(defun main (world undefined)
  (let ((pill-list (pills (car world))))
    (cons
      (cons 0 (cons pill-list (make-list 200 (cons 0 0))))
      step)))

(defun step (state world)
  (let ((tick (car state))
        (pills (car (cdr state)));(maybe-with-fruit (car (cdr state)) world))
        (last-visited (cdr (cdr state)))
        (player (car (cdr world)))
        (world-map (car world))
        (ghost-info (tuple-nth world 4 2)))
    (let ((player-pos (car (cdr player))) (player-dir (tuple-nth player 5 2)))
          ;(all-ghosts (map pos-gen-fns ghost-info)))
      (let ((player-x (car player-pos))
            (player-y (cdr player-pos)))
        (let ((player-row (list-nth world-map player-y)))
          (let ((moves
            (map (lambda (f)
              (let ((ghosts (f ghost-info)))
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
                      ; no pill nearby, find distant pill to bear towards
                      (let ((result (choose-direction-and-update-pills pills player-pos player-dir player-row ghosts world-map)))
                        (set pills (cdr result))
                        (let ((maybe-distant-pill (car result)))
                            maybe-distant-pill))
                    maybe-pill))))
              (cons generate-ghost-pos-2 (cons generate-ghost-pos-1 (cons generate-ghost-pos-0 (cons generate-ghost-pos-none 0)))))
          ))
            (let ((move (car (cdr (find (lambda (x) (not (atom x))) moves)))))
              (cons
               (cons (+ tick 1) (cons pills (push-back (cdr last-visited) (cdr move))))
               (car move)))))))))

;(defun maybe-with-fruit (pills world)
;  (let ((fruit-timer (tuple-nth world 4 3)))
;    (if fruit-timer () (cdr pills))))

(defun pills (world)
  (__pills-inner (car world) (cdr world) 0 0)
)

(defun __pills-inner (row future_rows x y)
  (if (atom row)
    (if (atom future_rows) 0 (__pills-inner (car future_rows) (cdr future_rows) 0 (+ y 1)))
    (let ((future_pills (__pills-inner (cdr row) future_rows (+ x 1) y)))
      (if (if (= (car row) PILL) 1 (= (car row) POWER_PILL))
        (cons (cons x y) future_pills) ; pill
        future_pills ; not pill
      )
    )
  )
)

(defun choose-direction-and-update-pills (pills player-pos dir player-row ghosts map)
  (cons ; big indented expr block returns a direction (-1 for failure)
    (if (atom pills) -1 ; fail: just pick a direction
      (let ((pill (car pills)))
        (let ((pill-x (car pill)) (pill-y (cdr pill)))
          (if (pill-or-better (2d-nth map pill-x pill-y)) ; pill still there
            (let ((player-x (car player-pos)) (player-y (cdr player-pos)))
              ; choose direction taking us towards a pill
              (let ((abs-x (abs (- player-x pill-x))) (abs-y (abs (- player-y pill-y)))
                    (up-gt-down (> player-y pill-y)) (left-gt-right (> player-x pill-x)))
                (if (>= abs-x abs-y)
                  ; prefer across rather than vertical
                  (let ((pref1 (if left-gt-right LEFT RIGHT))
                        (pref2 (if up-gt-down UP DOWN))
                        (pref3 (if left-gt-right RIGHT LEFT))
                        (pref4 (if up-gt-down DOWN UP)))
                    (check-surrounds-by-pref map player-row
                                             player-x player-y dir
                                             pref1 pref2 pref3 pref4
                                             (lambda (value x y)
                                               (check-and-not-old-or-ghost not-a-wall
                                                                       value x y
                                                                       0
                                                                       ghosts)))
                  )
                  ; else prefer up/down rather than across
                  (let ((pref1 (if up-gt-down UP DOWN))
                        (pref2 (if left-gt-right LEFT RIGHT))
                        (pref3 (if up-gt-down DOWN UP))
                        (pref4 (if left-gt-right RIGHT LEFT)))
                    (check-surrounds-by-pref map player-row
                                             player-x player-y dir
                                             pref1 pref2 pref3 pref4
                                             (lambda (value x y)
                                               (check-and-not-old-or-ghost not-a-wall
                                                                       value x y
                                                                       0
                                                                       ghosts)))
                  )
                )
              )
            )
            ; else move onto next pill
            (let ((result (choose-direction-and-update-pills (cdr pills) player-pos dir player-row ghosts map)))
              (set pills (cdr result))
              (car result) ; return the direction chosen by recursive call
            )
          )
        )
      )
    )
  pills)) ; return (cons direction new-pill-list) pair

(defun find-valid-direction (map x y p1 p2 p3 p4)
  (let ((xy1 (inc x y p1)))
    (if (2d-nth map (car xy1) (cdr xy1))
      p1 ; can go p1
      (let ((xy2 (inc x y p2)))
        (if (2d-nth map (car xy2) (cdr xy2))
        p2 ; can go p2
        (let ((xy3 (inc x y p3)))
          (if (2d-nth map (car xy3) (cdr xy3))
            p3 ; can go p3
            p4)))))))

(defun inc (x y dir)
  (if (= dir RIGHT) (cons (+ x 1) y)
  (if (= dir DOWN) (cons x (+ y 1))
  (if (= dir LEFT) (cons (- x 1) y)
  (cons x (- y 1))))))

(defun generate-ghost-pos-2 (ghost-info)
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
                (generate-ghost-pos-2 (cdr ghost-info))
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

(defun generate-ghost-pos-0 (ghost-info) (map (lambda (ghost) (car (cdr ghost))) ghost-info))
(defun generate-ghost-pos-none (_) 0)
(defun generate-ghost-pos-1 (ghost-info)
  (if (atom ghost-info) 0
    (let ((ghost (car ghost-info)))
      (let ((ghost-pos (car (cdr ghost))))
        (let ((x (car ghost-pos)) (y (cdr ghost-pos)))
          (cons ghost-pos
            (cons (cons (+ x 1) y)
            (cons (cons (- x 1) y)
            (cons (cons x (+ y 1))
            (cons (cons x (- y 1))
                (generate-ghost-pos-1 (cdr ghost-info))
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

(defun not-a-wall (value) (>= value EMPTY))

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

(defun opt-get-sq (map player-row py x y)
  (if (= y py) (list-nth player-row x) (2d-nth map x y))
)
; call `f' on the cell values in each of the directions, returning the
; first returns true (or -1 if they're all false)
(defun check-surrounds-by-pref (map player-row player-x player-y dir p1 p2 p3 p4 f)
  (if (= p1 -1) 0
  (let ((xy (inc player-x player-y p1)))
    (let ((x (car xy)) (y (cdr xy)))
      (if (f (opt-get-sq map player-row player-y x y) x y)
        (if (opposed dir p1)
          (if (= p2 -1)
            (check-surrounds-by-pref map player-row player-x player-y NO_DIRECTION p1 p2 p3 p4 f)
            (if (= p3 -1)
              (check-surrounds-by-pref map player-row player-x player-y NO_DIRECTION p2 p1 p3 p4 f)
              (if (= p4 -1)
                (check-surrounds-by-pref map player-row player-x player-y NO_DIRECTION p2 p3 p1 p4 f)
                (check-surrounds-by-pref map player-row player-x player-y NO_DIRECTION p2 p3 p4 p1 f))))
          (cons p1 xy)
        )
        (check-surrounds-by-pref map player-row player-x player-y dir p2 p3 p4 -1 f)
      )
    )
  )))

(defun opposed (dir1 dir2) (= (abs (- dir1 dir2)) 2))

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
