;; setup for terminal
(menu-bar-mode -1)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-faces nil)

(setq indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)
(setq-default tab-width 4)

(recentf-mode 1)
(setq recentf-max-saved-items 50)

(global-visual-line-mode t)
(show-paren-mode 1)
(transient-mark-mode t)
(global-font-lock-mode -1)

;; local package
;; (add-to-list 'load-path "~/.emacs.d/site-lisp")
;; (load "ace-jump-mode")

;; elpa
(package-initialize)

;; (global-git-gutter-mode t)
;; (which-key-mode 1)
;; (dumb-jump-mode 1)
;; (drag-stuff-global-mode t)
;; (drag-stuff-define-keys)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq flycheck-highlighting-mode 'lines)

(global-set-key (kbd "C-c =") 'er/expand-region)
(global-set-key (kbd "C-c SPC") 'set-mark-command)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c d") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-c e") 'magit-status)
(global-set-key (kbd "C-c f") 'ido-find-file)
(global-set-key (kbd "C-c g") 'dumb-jump-go)
(global-set-key (kbd "C-c j") 'ace-jump-mode)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c p") 'dumb-jump-back)
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c s") 'save-buffer)
(global-set-key (kbd "C-c t") 'emamux:send-command)
(global-set-key (kbd "C-c C-t") 'emamux:send-region)
(global-set-key (kbd "C-c v") 'ido-find-alternate-file)
(global-set-key (kbd "C-c w") 'ace-window)

(global-company-mode)
(global-flycheck-mode)

(custom-set-variables
 '(custom-enabled-themes (quote (tango-dark))))
