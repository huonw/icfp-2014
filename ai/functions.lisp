; takes x, y and returns (x, y) pair in direction given
; needs consts.lisp to be included
(defun inc (x y dir)
  (if (= dir RIGHT) (cons (+ x 1) y)
  (if (= dir DOWN) (cons x (+ y 1))
  (if (= dir LEFT) (cons (- x 1) y)
  (cons x (- y 1))))))

; don't count the starting places
(defun pill-or-better (value)
  (and (>= value PILL) (>= POWER_PILL value)))

(defun not-a-wall (value) (>= value EMPTY))

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

; list of n copies of element
(defun make-list (n element)
  (if n (cons element (make-list (- n 1) element)) 0))

; length of list
(defun length (list) (if (atom list) 0 (+ 1 (length (cdr list)))))

(defun foldl (f z list)
  (if (atom list)
      z
    (foldl f (f z (car list)) (cdr list))))
(defun foldr (f z list)
  (if (atom list)
      z
    (f (car list) (foldr f z (cdr list)))))

; create a new list by applying f to each element of list
;
; (passing args to each f invocation)
(defun map (f list)
  (if
      (atom list)
      0
      (cons (f (car list)) (map f (cdr list)))))

; create a new list applying f to each element of list
;
; (passes args and the index i to each invocation)
(defun mapi (f list) (__mapi f  list 0))

(defun __mapi (f list i)
  (if
      (atom list)
      0
      (cons (f (car list) i) (__mapi f (cdr list) (+ i 1)))))

; find the first element of list for which f is true returning (index,
; (element, f element)), or 0 if it is not found.
(defun find (f list)
  (__find f list 0))

(defun __find (f list i)
  (if (atom list)
      0
    (let ((head (car list)))
      (let ((result (f head)))
        (if result
            (cons i (cons head result))
          (__find f (cdr list) (+ i 1)))))))

(defun point-not-in-list (point point-list)
  (let ((x (car point))
        (y (cdr point)))
    (atom (find
           (lambda (a) (if (= (car a) x) (= (cdr a) y) 0))
           point-list))))

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

(defun abs (x)
  (if (>= x 0) x (- 0 x)))

; place x onto the stack twice
(defun dup (x) (do x x))
