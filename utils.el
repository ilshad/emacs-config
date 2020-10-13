;;
;; UI: Transparency
;;

(setq transparency-started-p nil)

(defun transparency-toggle ()
  (interactive)

  (when (not transparency-started-p)
    (set-frame-parameter (selected-frame) 'alpha '(85 . 60))
    (add-to-list 'default-frame-alist '(alpha . (85 . 60)))
    (setq transparency-started-p t))

  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(85 . 60) '(100 . 100)))))

;;
;; Investments
;;

(defun investments-average-price (position quantity average-price price)
  (interactive "nCurrent position (quantity): \nnBuy quantity: \nnAverage Price at the moment: \nnBuy price: ")
  (let ((average-new (/ (+ (* position average-price)
			   (* quantity price))
			(+ position quantity))))
    (message "New Average Price: %f, Change: %d%%, Order cost: %d"
	     average-new
	     (* (/ (- average-new average-price)
		   (float average-price))
		100)
	     (* price quantity))))
