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

(defun foldl (f z list)
  (if list
      z
    (foldl f (f z (car list)) (cdr list))))
(defun foldr (f z list)
  (if list
      z
    (f (car list) (foldr f z (cdr list)))))

; create a new list by applying f to each element of list
(defun map (f list)
  (if
      list
      ((f (car list)) (map f (cdr list)))
    0))

; find the first element of list for which f is true returning (index,
; (element, f element)), or 0 if it is not found.
(defun find (f list)
  (__find f list 0))

(defun __find (f list i)
  (if list
      (let ((head (car list)))
        (let ((result (f head)))
          (if result
              (cons i (cons head result))
            (__find f (cdr list) (add i 1)))))
    0))

; create a new list with elem on the back
(defun push-back (list elem)
  (if list
      (cons (car list) (push-back (cdr list) elem))
    (cons elem 0)))

; append the two lists a and b
(defun append (a b)
  (if a
      (cons (car a) (append (cdr a) b))
    b))

; booleans r fun
(defun or (x y) (if x x y))
(defun and (x y) (if x y 0))
(defun not (x) (if x 0 1))

; place x onto the stack twice
(defun dup (x) (do x x))
