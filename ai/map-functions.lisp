(include "consts.lisp")
(include "functions.lisp")

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

; (defun visit-cell (xy rest_of_row prev_row whole_row rest_of_world world seen)
(defun main (world ghost-prog)
  (dbug (pills (car world)))
  (cons 0 (lambda (state) (cons 0 LEFT)))
)
  ;(let ((x 1) (y 1) (trees (build-trees (car world))))
  ;  (dbug (2d-nth trees x y))
  ;(set world (car world))
  ;(let ((tree (visit-cell
  ;              (cons 1 1) ; xy
  ;              (cdr (list-nth world 1)) ; rest_of_row
  ;              (car world) ; prev_row
  ;              (list-nth world 1) ; whole_row
  ;              (nth-cell world 2) ; rest_of_world
  ;              world ; world
  ;              0 ; seen
  ;              )))
  ;  (dbug tree)
  ;  (cons 0 (lambda (state) (cons 0 LEFT)))
  ;)
;)

(defun dbug-trees () (dbug
  (let ((x 2) (y 0))
    (let ((first-row (cons 1 (cons 1 (cons 0 0))))
          (second-row (cons 0 (cons 1 (cons 1 0))))
          (third-row (cons 0 (cons 0 (cons 1 0))))
         )
      (let ((trees (build-trees (cons first-row (cons second-row (cons third-row 0))))))
        (dbug (2d-nth trees x y))
      )
    )
  )
))

(defun build-trees (world)
  (let ((rest_of_world world))
    (mapi
      (lambda (row y)
        (set rest_of_world (cdr rest_of_world))
        (build-trees-row
          0 y row ; initial_x y initial_rest_of_row
            (if y (list-nth world (- y 1)) 0) ; prev_row if available
            row ; whole_row
            rest_of_world
            world
        )
      )
      world
    )
  )
)

(defun build-trees-row (x y rest_of_row  prev_row whole_row rest_of_world world)
  (if (atom rest_of_row) 0
    (cons
      (car (visit-cell
            (cons x y) ; xy
            rest_of_row ; rest_of_row
            prev_row ; prev_row
            whole_row ; whole_row
            rest_of_world ; rest_of_world
            world ; world
            0 ; seen
      ))
      (build-trees-row (+ x 1) y (cdr rest_of_row) prev_row whole_row rest_of_world world)
    )
  )
)


(defun visit-cell (xy rest_of_row prev_row whole_row rest_of_world world seen)
  (if (if (car rest_of_row) (not (point-not-in-list xy seen)) 1) ; exit via the true branch
    (cons 0 seen) ; return immediately

    (let ((children 0) (seen (cons xy seen)) (x (car xy)) (y (cdr xy)))
      ; (dbug xy)
      ; (dbug seen)
  
      ; right-move (cheapest)
      (if (atom (cdr rest_of_row)) ; end of row?
        (set children (cons 0 children)) ; don't try to move right or add tree-right to the children list
        (let ((result (visit-cell (cons (+ x 1) y) (cdr rest_of_row)
                                  prev_row whole_row rest_of_world world seen)))
          (set children (cons (car result) children))
          (set seen (cdr result))
        )
      )

      ;(if (not (+ x y)) (dbug children) (pass))
      ;(if (and (= x 2) (= y 1)) (do (dbug rest_of_world) (brk)) (pass))
      ;(dbug xy)
      ; down-move
      (if (atom rest_of_world)
        (set children (cons 0 children)) ; can't move down
        (let ((result (visit-cell (cons x (+ y 1)) (nth-cell (car rest_of_world) x)
                       whole_row (car rest_of_world) (cdr rest_of_world) world seen)))
          (set children (cons (car result) children))
          (set seen (cdr result))
        )
      )

      ;(if (not (+ x y)) (dbug children) (pass))

      ; left-move
      (if x
        (let ((result (visit-cell (cons (- x 1) y)
                      (cons (list-nth whole_row (- x 1)) rest_of_row) ; push left-element on
                      prev_row whole_row rest_of_world world seen)))
          (set children (cons (car result) children))
          (set seen (cdr result))
        )
        (set children (cons 0 children)) ; couldn't move left
      )

      ;(if (not (+ x y)) (dbug children) (pass))

      ; up-move
      (if y
        (let ((result (visit-cell (cons x (- y 1)) (nth-cell prev_row x)
                                  (if (= y 1) 0 (list-nth world (- y 2))) ; prev-prev-row
                                  prev_row (cons whole_row rest_of_world) world seen)))
          (set children (cons (car result) children))
          (set seen (cdr result))
        )
        (set children (cons 0 children)) ; couldn't move up
      )

      ;(if (not (+ x y)) (dbug children) (pass))

      (set children (cons (car rest_of_row) children)) ; add my cell on

      ;(if (not (+ x y)) (dbug children) (pass))

      (cons children seen) ; return the created tuple and new seen list
    )
  )
)