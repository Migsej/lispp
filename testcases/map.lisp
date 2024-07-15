(defun map (f a) (if (null a) (list) (cons (f (first a)) (map f (tail a)))))
(write (map (lambda (a) (+ a 1)) '(1 2 3 4)))
