;;
;; Leuven
;;

(defun fix-color-theme-leuven ()
  (set-face-foreground 'font-lock-type-face "#276695")
  (set-face-foreground 'font-lock-constant-face "#276695"))

(defun color-theme-leuven ()
  (interactive)
  (load-theme 'leuven)
  (fix-color-theme-leuven))

;;
;; Nord
;;

(defun fix-color-theme-nord ()
  (set-background-color "#2d3646")
  (set-face-foreground 'font-lock-type-face "#81A1C1"))

;(use-package nord-theme :ensure t :config (fix-color-theme-nord))

(defun color-theme-nord ()
  (interactive)
  (load-theme 'nord)
  (fix-color-theme-nord))

;;
;; Zenburn
;;

;(use-package zenburn-theme :ensure t)

(defun color-theme-zenburn ()
  (interactive)
  (setq zenburn-use-variable-pitch      t
	zenburn-scale-org-headlines     t
	zenburn-scale-outline-headlines t
	zenburn-override-colors-alist   '(("zenburn-green+4" . "#7CB8BB")))
  (load-theme 'zenburn))

;;
;; Plan9
;;

;(use-package plan9-theme :ensure t)

(defun color-theme-plan9 ()
  (interactive)
  (load-theme 'plan9)
  (set-face-foreground 'font-lock-constant-face "#0287c8")
  (set-face-bold 'font-lock-constant-face nil)
  (set-face-bold 'font-lock-type-face nil))
