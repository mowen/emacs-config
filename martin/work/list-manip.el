(defun mo-move-element-up (lst i)
  "Move LST element at 1-base index I up one."
  (if (or (< i 2)
	  (> i (length lst)))
      (error "Index out of range.")
    (let ((element (nth (- i 1) lst))
	  (prev-element (nth (- i 2) lst)))
      (setcdr (nthcdr (- i 3) lst)
	      ;; cons next-element and element to cdr of next-element
	      ;; (1, 2, 3, 4, 5) becomes (1, 3, 2, 3, 4, 5), where i is 3
	      (cons element (cons prev-element (nthcdr i lst)))))
    (if (= i 2)
	(mo-remove-element-at lst 1)
      lst)))

(defun mo-move-element-down (lst i)
  "Move LST element at 1-based index I down one."
  (if (or (zerop i)
	  (>= i (length lst)))
      (error "Index out of range.")
    (let ((element (nth (- i 1) lst))
	  (next-element (nth i lst)))
      (setcdr (nthcdr (- i 1) lst)
	      ;; cons next-element and element to cdr of next-element
	      ;; (1, 2, 3, 4, 5) becomes (1, 2, 3, 2, 4, 5), where i is 2
	      (cons next-element (cons element (nthcdr (+ i 1) lst))))
      ;; Remove the duplicate of the element that has been moved down
      ;; So (1, 2, 3, 2, 4, 5) becomes (1, 3, 2, 4, 5) where i is 2
      (mo-remove-element-at lst i))))

(defun mo-remove-element-at (lst i)
  "Remove the element at index I."
  (if (= i 1)
      (cdr lst)
    (setcdr (nthcdr (- i 2) lst) (nthcdr i lst))
    lst))

(progn
  (setq my-list '(martin is great yes (he blah) is))
  (setq my-result '(martin is great (he blah) yes is))
  (setq my-list (mo-move-element-up my-list 5))
  (print my-list)
  (print my-result))