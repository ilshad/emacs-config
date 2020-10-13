;;
;; MacOS / keyboard essentials
;;

(setq mac-command-modifier 'meta
      mac-option-modifier  'control)

;;
;; UI
;;

(set-default-font "Menlo 15")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(set-fringe-mode 0)

(setq inhibit-startup-message t
      calendar-week-start-day 1
      split-width-threshold   100
      initial-frame-alist     '((tool-bar-lines . 0)
				(top . 40) (left . 700)
				(width . 80) (height . 43)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq ns-use-proxy-icon  nil
      frame-title-format nil)

;;
;; Special
;;

(setq make-backup-files              nil
      inhibit-compacting-font-caches t)

;;
;; Scroll
;;

(setq scroll-margin                   10
      scroll-conservatively           50
      scroll-preserve-screen-position t)

;;
;; Utils
;;

(defun in-emacs-dir (file-name)
  (expand-file-name (concat user-emacs-directory file-name)))

;;
;; Misc global keys
;;

(global-set-key "\M-`" 'toggle-frame-fullscreen)
(global-set-key "\C-q" 'kill-buffer)
(global-set-key "\M-'" 'eshell)

(require 'windmove)

(windmove-default-keybindings)

;;
;; Dired
;;

(require 'dired)

(global-set-key "\M-m" (lambda () (interactive) (dired-jump)))
(eval-after-load "dired" '(require 'dired-x))
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
	  (lambda () (dired-omit-mode t)))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file "..")))))

;;
;; Package Management
;;

(package-initialize)

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; Paredit
;;

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings         'paredit
	sp-autoskip-closing-pair     'always
	sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1))

;;
;; Helm Framework
;;

(use-package helm
  :ensure t
  :bind (("M-x"     . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b"   . helm-mini)
	 ("C-x C-b" . helm-buffers-list))
  :config (setq helm-ff-keep-cached-candidates      nil
		helm-split-window-inside-p            t
		helm-buffers-fuzzy-matching           t
		helm-move-to-line-cycle-in-source     t
		helm-ff-search-library-in-sexp        t
		helm-ff-file-name-history-use-recentf t
		helm-ff-skip-boring-files             t
		helm-allow-mouse                      t))

;;
;; Company
;;

(use-package company
  :ensure t
  :config
  (setq	company-idle-delay                0.3
	company-minimum-prefix-length     2
	company-tooltip-flip-when-above   t
	company-tooltip-align-annotations t
	company-tooltip-limit             10)
  (global-company-mode 1))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort))

;;
;; Emacs Lisp
;;

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (eldoc-mode +1)
				  (smartparens-strict-mode +1)))

;;
;; Common Lisp
;;

(use-package slime
  :ensure t
  :init (load (expand-file-name "~/quicklisp/slime-helper.el"))
  :config
  (setq slime-lisp-implementations
	'((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))

  (setq slime-contribs '(slime-fancy slime-cl-indent))

  (setq slime-default-lisp              'sbcl
	slime-net-coding-system         'utf-8-unix
	slime-complete-symbol-function  'slime-fuzzy-complete-symbol
	slime-fuzzy-completion-in-place t)

  (setq common-lisp-hyperspec-root
	"file:///Users/ilshad/Dropbox/HyperSpec-7-0/HyperSpec/")

  (add-hook 'slime-repl-mode-hook
	    (lambda ()
	      (setq scroll-margin 10)
	      (smartparens-strict-mode +1)))

  (add-hook 'lisp-mode-hook
	    (lambda ()
	      (smartparens-strict-mode +1))))

(use-package slime-company
  :ensure t
  :init
  (require 'company)
  (slime-setup '(slime-fancy slime-company)))

;;
;; Clojure
;;

(defun my/clojure-hook ()
  (setq prettify-symbols-alist '(("fn" . 955)))
  (prettify-symbols-mode +1)
  (smartparens-strict-mode +1))

(use-package cider
  :ensure t
  :config
  (setq cider-repl-display-help-banner nil)
  (add-hook 'clojure-mode-hook    'my/clojure-hook)
  (add-hook 'cider-repl-mode-hook 'my/clojure-hook))

(defmacro my/clojure-defn-indents (&rest names)
  `(progn
     ,@(mapcar
	(lambda (name)
	  `(put-clojure-indent (quote ,name) :defn))
	names)))

(my/clojure-defn-indents
 swap! reset!
 assoc assoc-in update update-in get-in
 map filter reduce reduce-kv interpose
 mapv filterv
 map-indexed
 partial apply

 ;; Datomic
 d/transact d/pull

 ;; legacy Om stuff
 render render-state init-state
 om/set-state! om/update-state!
 dom/div dom/select)

;;
;; Org Mode
;;

(require 'org)
(require 'org-clock)
(require 'org-agenda)

(defun in-org-dir (file-name)
  (expand-file-name (concat "~/org/" file-name)))

(setq org-directory              (expand-file-name "~/org")
      org-src-fontify-natively   t
      org-src-tab-acts-natively  t
      org-goto-auto-isearch      nil
      org-link-frame-setup       '((file . find-file))
      org-blank-before-new-entry '((heading . auto) (plain-list-itme . auto))

      org-enforce-todo-dependencies     t
      org-fast-tag-selection-single-key t
      org-tags-column                   -60

      ;; Source code blocks
      org-confirm-babel-evaluate       nil
      org-edit-src-content-indentation 0

      ;; Clock
      org-clock-persist     'history
      org-clock-into-drawer 't

      ;; Agenda
      org-agenda-files (mapcar 'in-org-dir (list "home.org"
						 "projects.org"
						 "journal.org"))

      org-agenda-todo-list-sublevels nil
      org-refile-targets     '((org-agenda-files . (:maxlevel . 1)))
      org-default-notes-file (in-org-dir "journal.org")

      ;; Capture
      org-capture-templates
      '(("j" "Journal" entry
	 (file+datetree "~/org/journal.org")
	 "* %? %^g\n")

	("t" "TODO" entry
	 (file+headline "~/org/home.org" "Inbox")
	 "* TODO %? %^g\n")

	("u" "URL" item
	 (file+headline "~/org/journal.org" "Links")
	 "[[%^{URL}][%^{Title}]]")

	("r" "Read" item
	 (file+headline "~/org/journal.org" "Read")
	 "[[%^{URL}]]")))

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)

(org-clock-persistence-insinuate)

(use-package org-superstar
  :ensure t
  :custom (org-superstar-special-todo-items t)
  :init   (add-hook 'org-mode-hook (lambda () (org-superstar-mode +1))))

(defun home ()     (interactive) (find-file (in-org-dir "home.org")))
(defun journal ()  (interactive) (find-file (in-org-dir "journal.org")))
(defun projects () (interactive) (find-file (in-org-dir "projects.org")))

;;
;; Email
;;

(add-to-list 'exec-path "/usr/local/bin")

(use-package notmuch
  :ensure t
  :bind (:map notmuch-show-mode-map
	      ("S" . (lambda ()
		       (interactive)
		       (notmuch-show-tag (list "+spam" "-inbox")))))
  :config
  (setq notmuch-archive-tags        '("-inbox" "-unread" "+archive")
	notmuch-show-mark-read-tags '("-inbox" "-unread" "+archive")
	notmuch-search-oldest-first nil
	notmuch-saved-searches      '((:name "Inbox"
				       :query "tag:inbox AND tag:unread"
				       :key "i"
				       :search-type 'tree)
				      (:name "Hold On"
				       :query "tag:hold"
				       :key "h"
				       :search-type 'tree)
				      (:name "Feed"
				       :query "tag:feed AND tag:unread"
				       :key "f"
				       :search-type 'tree)
				      (:name "Paperwork"
				       :query "tag:paper"
				       :key "p"))

	notmuch-hello-sections      '(notmuch-hello-insert-header
				      notmuch-hello-insert-saved-searches
				      notmuch-hello-insert-search
				      notmuch-hello-insert-alltags)

	;; Send mail
	sendmail-program            (executable-find "msmtp")
	message-send-mail-function  'message-send-mail-with-sendmail
	user-mail-address           (notmuch-user-primary-email)
	user-full-name              (notmuch-user-name)
	mail-envelope-from          'header
	mail-specify-envelope-from  t))

;;
;; Calendar
;;


;;
;; Handy packages
;;

(use-package try             :ensure t)
(use-package rainbow-mode    :ensure t)
(use-package which-key       :ensure t :config (which-key-mode))
(use-package magit           :ensure t :bind   ([f6] . magit-status))
(use-package elpher          :ensure t)
(use-package dictionary      :ensure t)

;;
;; Various formats
;;

(use-package bnf-mode        :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package markdown-mode   :ensure t :mode (("\\.md\\'"  . gfm-mode)))
(use-package yaml-mode       :ensure t :mode (("\\.yml\\'" . yaml-mode)))
(use-package ttl-mode        :ensure t :mode (("\\.ttl\\'" . ttl-mode)))

;;
;; Games
;;

(use-package 2048-game :ensure t)

;;
;; Theming
;;

(defun fix-lock-face ()
  (interactive)
  (set-face-foreground 'font-lock-type-face
		       (face-foreground 'font-lock-constant-face)))


;;
;; Manage ~/.emacs.d directory structure
;;

(load (in-emacs-dir "utils.el"))
(load (in-emacs-dir "themes.el"))
(load (in-emacs-dir "private/erc.el"))
(load (in-emacs-dir "private/elfeed.el"))

(setq custom-file (in-emacs-dir "custom.el"))
