; the nth cons cell in a (x (y (z ...))) list
(defun nth-cell (array n)
  (if n (nth-cell (cdr array) (- n 1)) array))

; the nth element of list (doesn't check for out-of-bounds access)
(defun list-nth (list n)
  (car (nth-cell list n)))

; get the (x, y)'th entry of the 2D row-major grid
(defun 2d-nth (grid x y)
  (list-nth (list-nth grid y) x))

; the nth element of a tuple of length n (or, the last one, if n >= length)
(defun tuple-nth (tup length n)
  (if (>= n length)
      ; get the cdr of the last cell
      (cdr (nth-cell tup (- n 1)))
    ; a tuple index is the same as an list index except for the last one
   (list-nth tup n)))

; length of list
(defun length (list) (if (atom list) 0 (+ 1 (length (cdr list)))))

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
      (cons (f args (car list)) (map f args (cdr list)))))

; create a new list applying f to each element of list
;
; (passes args and the index i to each invocation)
(defun mapi (f args list) (__mapi f args list 0))

(defun __mapi (f args list i)
  (if
      (atom list)
      0
      (cons (f args (car list) i) (__mapi f args (cdr list) (+ i 1)))))

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
          (__find f args (cdr list) (+ i 1)))))))

(defun point-not-in-list (point point-list)
  (atom (find point-equals point point-list)))

(defun point-equals (a b)
  (if (= (car a) (car b))
      (= (cdr a) (cdr b))
    0))

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
