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
;; Leuven dark
;;

(defun fix-color-theme-dark-leuven ()
  (set-face-foreground 'font-lock-type-face "#34c8d8")
  (set-face-foreground 'font-lock-constant-face "#34c8d8"))

(defun color-theme-dark-leuven ()
  (interactive)
  (load-theme 'leuven-dark)
  (fix-color-theme-dark-leuven))

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

(use-package zenburn-theme :ensure t)

(defun color-theme-zenburn ()
  (interactive)
  (setq zenburn-use-variable-pitch      t
	zenburn-scale-org-headlines     t
	zenburn-scale-outline-headlines t
        zenburn-override-colors-alist   '(("zenburn-green+4" . "#7CB8BB")
					  ("zenburn-bg" . "#353535")))
  (load-theme 'zenburn))

;;
;; Plan9
;;

(defun color-mode-line-plan9 ()
  (interactive)
  (set-face-background 'mode-line "#007777")
  (set-face-foreground 'mode-line "#ffffe8")
  (set-face-foreground 'mode-line-buffer-id "#ffffe8")
  ;(set-face-background 'mode-line-inactive "#007777")
  ;(set-face-background 'mode-line-inactive "#888838")
  (set-face-background 'mode-line-inactive "#988D6D")
  )

(use-package plan9-theme :ensure t)

(defun color-theme-plan9 ()
  (interactive)
  (load-theme 'plan9)
  (set-face-foreground 'font-lock-constant-face "#0287c8")
  (set-face-bold 'font-lock-constant-face nil)
  (set-face-bold 'font-lock-type-face nil)
  (color-mode-line-plan9))

;;
;; Fix built-in themes
;;

(defun fix-color-theme-wheatgrass ()
  (interactive)
  (set-face-foreground 'font-lock-type-face "#78ded6")
  (set-face-foreground 'font-lock-constant-face "#78ded6"))
