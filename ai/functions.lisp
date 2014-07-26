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

(defun foldl (f args z list)
  (if (atom list)
      z
    (foldl f args (f args z (car list)) (cdr list))))
(defun foldr (f args z list)
  (if (atom list)
      z
    (f args (car list) (foldr f args z (cdr list)))))

; create a new list by applying f to each element of list
;
; (passing args to each f invocation)
(defun map (f args list)
  (if
      (atom list)
      0
      ((f args (car list)) (map f args (cdr list)))))

; find the first element of list for which f is true returning (index,
; (element, f element)), or 0 if it is not found.
;
; (f is called with args)
(defun find (f args list)
  (__find f args list 0))

(defun __find (f args list i)
  (if (atom list)
      0
    (let ((head (car list)))
      (let ((result (f args head)))
        (if result
            (cons i (cons head result))
          (__find f args (cdr list) (add i 1)))))))

; create a new list with elem on the back
(defun push-back (list elem)
  (if (atom list)
      (cons elem 0)
      (cons (car list) (push-back (cdr list) elem))))

; append the two lists a and b
(defun append (a b)
  (if (atom a)
      b
    (cons (car a) (append (cdr a) b))))

; booleans r fun
(defun or (x y) (if x x y))
(defun and (x y) (if x y 0))
(defun not (x) (if x 0 1))

; place x onto the stack twice
(defun dup (x) (do x x))
