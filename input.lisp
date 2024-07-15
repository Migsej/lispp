
(defun rec (n) (if (or (= n 1) (= n 0)) n (+ (rec (- n 1)) (rec (- n 2)))))

(defun gen (n) (if (= 0 n) (list) (cons n (gen (- n 1)))))


(write (last '(1 2 3 4)))
(write (length '(1 2 3 4)))

(defun palindrome (l) (= (rev l) l))

(write (palindrome '(1 2)))

(write (listp 1))
(write (listp '(1 2 3)))

(defun flatten (l) (if (null l) (list) (if (listp (first l)) 
					 (append (flatten (first l)) (flatten (tail l)))
					 (cons (first l) (flatten (tail l))))))
(write (flatten '(1 '(2 '(3 4) 5))))

(defun compress (l) (if (null l) (list)
		      (if (= 1 (length l)) l
			(if (= (first l) (nth 1 l)) 
			  (compress (tail l)) 
			  (cons (first l) (compress (tail l)))))))

(write  (compress '(1 1 1 1 2 3 3 1 1 4 5 5 5 5)))

(write (when false 2))
