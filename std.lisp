(defun or (x y) (if x true (if y true false)))

; O(n) lookup haters gonna hate
(defun nth (n x) (if (= n 0) 
                   (first x) 
                   (nth (- n 1) (tail x))))

(defun map (f a) (if (null a) (list) (cons (f (first a)) (map f (tail a)))))

(defun append (a l) (if (null a) l (cons (first a) (append (tail a) l))))
(defun rev (l) (if (null l) (list) (append (rev (tail l)) '((first l)))))

(defun length (l) (if (null l) 0 (+ 1 (length (tail l)))))
(defun last (l) (nth (- (length l) 1) l))

(defun when (test action) (if test action nil))



