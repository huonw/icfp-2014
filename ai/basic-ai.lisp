(include "consts.lisp")
(defun main (world undefined)
  (cons UP step))

; walk in each of the four directions, cyclically, with no checking.
(defun step (state world)
  (if
      (= state LEFT)
      (cons 0 state)
    (cons (+ state 1) state)))
