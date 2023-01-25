;;
;; UI
;;

(setq transparency-started-p nil)

(defun transparency-toggle ()
  (interactive)

  (when (not transparency-started-p)
    (set-frame-parameter (selected-frame) 'alpha '(90 . 60))
    (add-to-list 'default-frame-alist '(alpha . (90 . 60)))
    (setq transparency-started-p t))

  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(90 . 60) '(100 . 100)))))

(defun reset-frame ()
  (interactive)

  (when (frame-parameter nil 'fullscreen)
    (toggle-frame-fullscreen))

  (let ((width (display-pixel-width)))
    (cond

     ;; Monitor + laptop
     ((= width 4000)
      ;(set-frame-position nil -1538 -16)

      ;(set-frame-position nil -1538 -500) ; Monitor on left
      (set-frame-position nil 2462 -500) ; Monitor on right

      (set-frame-size nil 168 73))

     ;; Monitor
     ((= width 2560)
      (set-frame-position nil 1022 38)
      (set-frame-size nil 168 73))

     ;; Laptop
     ((= width 1440)
      (set-frame-position nil 700 40)
      (set-frame-size nil 80 43)))))

;;
;; Investments
;;

(defun invest-average-price (position quantity average-price price)
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

;;
;; System
;;

(defun tmp-dir ()
  (interactive)
  (dired "/var/folders/pl/cq6qq99s79q14mspf_kt7ncc0000gn/T/"))

;;
;; ANSI Colors
;;

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
