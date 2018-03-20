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

(transient-mark-mode t)

(global-set-key (kbd "C-c 2") 'set-mark-command)

