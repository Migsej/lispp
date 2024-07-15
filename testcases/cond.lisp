(defvar x 20)

(defun cond (l) (if (first (first l)) (last (first l)) (cond (tail l))))

(write (cond '('((= x 10) 69)
              '((= x 20) 420))))

