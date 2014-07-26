; the nth cons cell in a (x (y (z ...))) list
(defun nth-cell (array n)
  (if n (nth-cell (cdr array) (sub n 1)) array))

; the nth element of list (doesn't check for out-of-bounds access)
(defun list-nth (list n)
  (car (nth-cell list n)))

; the nth element of a tuple of length n (or, the last one, if n >= length)
(defun tuple-nth (tup length n)
  (if (ge n length)
      ; get the cdr of the last cell
      (cdr (nth-cell tup (sub n 1)))
    ; a tuple index is the same as an list index except for the last one
   (list-nth tup n)))

; booleans r fun
(defun or (x y) (if x x y))
(defun and (x y) (if x y 0))
(defun not (x) (if x 0 1))

; place x onto the stack twice
(defun dup (x) (do x x))
