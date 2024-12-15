;;
;; UI
;;

;; (defun fix-font-size ()
;;   (interactive)
;;   ;(set-frame-font "DejaVu Sans Mono Book")
;;   ;(set-frame-font "DejaVu Sans Mono-14")
;;   ;(set-frame-font "Liberation Mono-14")
;;   (set-frame-font "-GOOG-Noto Sans Mono-regular-normal-normal-*-19-*-*-*-*-0-iso10646-1")
;;   ;(set-frame-font "-PfEd-DejaVu Sans Mono-regular-normal-normal-*-19-*-*-*-m-0-iso10646-1")
;;   ;(set-frame-font "DejaVu Sans Mono:pixelsize=19:foundry=PfEd:weight=regular:slant=normal:width=normal:spacing=50:scalable=true")
;;   )

(setq font-size-toggled-p nil)

(defun font-size-toggle ()
  (interactive)
  (set-frame-font (if font-size-toggled-p
		      "DejaVu Sans Mono-13"
		    "DejaVu Sans Mono-14"))
  (setq font-size-toggled-p (not font-size-toggled-p)))

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
     ((= width 4480)
      ;(set-frame-position nil -1538 -16)

      ;(set-frame-position nil -1538 -500) ; Monitor on left
      (set-frame-position nil 2462 -500) ; Monitor on right

      (set-frame-size nil 168 73))

     ;; Monitor
     ((= width 2560)
      (set-frame-position nil 840 20)
      (set-frame-size nil 170 63))

     ;; Laptop
     ((= width 1920)
      (set-frame-position nil 965 20)
      (set-frame-size nil 85 (if menu-bar-mode 46 47))))))

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
;; ANSI Colors
;;

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;;
;; Nyxt
;;

(defun my/nyxt-open-url (url)
  (defun true (&rest args) 't)
  (advice-add 'slime-check-version :override #'true)
  (slime-connect "localhost" "4006")
  (sleep-for 1)
  (advice-remove 'slime-check-version #'true)
  (if (slime-connected-p)
      (progn
	(slime-repl-send-string (format "(buffer-load \"%s\")" url))
	(slime-disconnect)
	(kill-buffer "*slime-repl sbcl*"))
    (error "SLIME is not connected to Nyxt.")))
