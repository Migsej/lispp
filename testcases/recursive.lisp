(defun rec (n) (if (or (= n 1) (= n 0)) n 
		 (+ (rec (- n 1)) (rec (- n 2)))))

(write (rec 9))
