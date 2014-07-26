(include "consts.lisp")
(defun main ()
  (cons UP step))

; walk in each of the four directions, cyclically, with no checking.
(defun step (state world)
  (if
      (eq state LEFT)
      (cons 0 state)
    (cons (add state 1) state)))
