(include "consts.lisp")
(defun main (world undefined)
  (cons 0 step))

(defun for-each (f list)
  (if (atom list)
      (pass)
    (do (f (car list)) (for-each f (cdr list)))))

(defun max (a b) (if (>= a b) a b))

(defun step (state world)
  (let ((up 0)
        (down 0)
        (left 0)
        (right 0)

        (places
         (cons (cons UP (lambda (d) (set up (+ up d))))
         (cons (cons DOWN (lambda (d) (set down (+ down d))))
         (cons (cons LEFT (lambda (d) (set left (+ left d))))
         (cons (cons RIGHT (lambda (d) (set right (+ right d))))
               0)))))
        ; these do computations on each of the (dir, function) pairs
        ; above, calling the function with an adjustment to make for
        ; the score of that direction
        (rating-functions
         (cons (lambda (dir_scorer) ...)
         (cons (lambda (dir_scorer) ...)
         (cons (lambda (dir_scorer) ...)
          ...
           0)))))
    ; run each of those on each dimension
    (for-each
     (lambda (score)
       (for-each
        (lambda (rate) (rate state world))
        rating-functions))
     places)
    ; find the best one
    (let ((m (max up (max down (max left right)))))
      (if (= m up) UP
        (if (= m down) DOWN
          (if (= m left) LEFT
            RIGHT))))))
