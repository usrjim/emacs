(defun my-depen ()
  (require 'go-mode-load)
  (require 'ace-jump-mode)
  (require 'php-mode)
  (autoload 'emmet-mode "emmet-mode")
  (autoload 'markdown-mode "markdown-mode")
  (autoload 'web-mode "web-mode")
  (autoload 'ecb-autoloads "ecb-autoloads"))

(defun my-env ()
  (set-language-environment "utf-8")
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq slime-net-coding-system 'utf-8-unix)
  (add-hook 'web-mode-hook  'emmet-mode)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
  (slime-setup '(slime-fancy))
  ;(setq inferior-lisp-program "/usr/bin/sbcl")
  (delete-selection-mode 1)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq inhibit-startup-message t)
  (cua-mode t)
  (projectile-global-mode)
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-faces nil)
  (setq indent-line-function 'insert-tab)
  (projectile-global-mode)
  ;(setq c-basic-offset 4)
  ;(setq c-basic-indent 4)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-always-indent t)
  (setq-default tab-width 4)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq ace-jump-mode-case-fold nil)
  (setq ace-jump-mode-submode-list
      '(ace-jump-char-mode ace-jump-word-mode ace-jump-line-mode))
  (recentf-mode 1)
  (setq recentf-max-saved-items 50)
  (global-visual-line-mode t)
  (show-paren-mode 1)
  ;(global-linum-mode t)
  (line-number-mode "on")
  (column-number-mode "on")
  (electric-pair-mode 1)
  ;(electric-indent-mode 1)
  (when (window-system)
    ;(nyan-mode 1)
    (set-frame-parameter (selected-frame) 'alpha '(95 65))
    (tool-bar-mode t)
    (scroll-bar-mode -1))
  (my-keys))

(defun my-keys ()
  ;;; c-c <-> c-i
  (keyboard-translate ?\C-i ?\C-c)
  (keyboard-translate ?\C-c ?\C-i)
  (keyboard-translate ?\C-m ?\C-x)
  (keyboard-translate ?\C-x ?\C-m)

  ;;; multiple cursors
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

  ;;; f12 group, keys are on left hand side
  (define-prefix-command 'f12-map)
  (global-set-key (kbd "<f12>") 'f12-map)
  (define-key f12-map (kbd "s") 'save-buffer)
  (define-key f12-map (kbd "r") 'ido-recentf-open)
  (define-key f12-map (kbd "f") 'ido-find-file)
  (define-key f12-map (kbd "e") 'eval-last-sexp)
  (define-key f12-map (kbd "1") 'delete-other-windows)
  (define-key f12-map (kbd "2") 'split-window-below)
  (define-key f12-map (kbd "3") 'split-window-right)
  (define-key f12-map (kbd "q") 'top-level)
  (define-key f12-map (kbd "b") 'ido-switch-buffer)

  ;;; f2 group, keys are on right hand side
  (define-prefix-command 'f2-map)
  (global-set-key (kbd "<f2>") 'f2-map)
  (define-key f2-map (kbd "0") 'delete-window)
  (define-key f2-map (kbd "o") 'other-window)
  (define-key f2-map (kbd "/") 'occur)
  (define-key f2-map (kbd "j") 'ace-jump-mode)
  (define-key f2-map (kbd "\\") 'dabbrev-expand)
  (define-key f2-map (kbd "k") 'kill-this-buffer)
  (define-key f2-map (kbd "p") 'projectile-command-map)
  (define-key f2-map (kbd "b") 'ido-switch-buffer)
  (define-key f2-map (kbd "m") 'mc/mark-all-like-this)
  (define-key f2-map (kbd "h") 'back-to-indentation)
  (define-key f2-map (kbd "u") 'beginning-of-visual-line)
  (define-key f2-map (kbd "i") 'end-of-visual-line)
  
  ;;; misc
  (global-set-key (kbd "<f7>") 'list-func)
  (global-set-key (kbd "C-<f11>") 'magit-status)
  (global-set-key (kbd "C-<f7>") 'ecb-activate))

(defun my-menu () 
  (define-key-after global-map [menu-bar usrj]
    (cons "usrj" (make-sparse-keymap "usrj")))

  (define-key global-map [menu-bar usrj usrj-slime]
    '("SLIME" . slime))
  (define-key global-map [menu-bar usrj usrj-erc]
    '("ERC" . erc))
  (define-key global-map [menu-bar usrj usrj-ecb]
    '("ECB" . ecb-activate))
  (define-key global-map [menu-bar usrj usrj-magit]
    '("Magit" . magit-status))

  (define-key global-map [menu-bar usrj usrj-sp1] '("--"))
  (define-key global-map [menu-bar usrj usrj-org-export-html-open]
    '("Org Export as HTML and Open" . org-export-as-html-and-open))

  (define-key global-map [menu-bar usrj usrj-sp2] '("--"))

  (define-key global-map [menu-bar usrj usrj-del-win]
    '("Delete window" . delete-window))
  (define-key global-map [menu-bar usrj usrj-one-win]
    '("One window" . delete-other-windows))
)

(defun php-setup ()
  (add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode)))

(defun php-scratch ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*php scratch*"))
  (php-mode)
  (local-set-key (kbd "<f6>") 'run-php-scratch))

(defun run-php-scratch (beg end)
  (interactive "r")
  (if (get-buffer "*PHP*")
      (kill-buffer "*PHP*")
    (message ""))
  (if (use-region-p)
      (php-send-region beg end)
    (php-send-region (point-min) (point-max)))
  (switch-to-buffer-other-window "*PHP*")
  (insert "\n\n")
  (switch-to-buffer-other-window "*php scratch*"))

(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun go-setup ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-i") 'go-import-add)))
  
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "<f7>") 'godoc)))

  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))))

  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "<f6>") 'go-run))))

(defun go-run ()
  (interactive)
  (compile (concat "go run " (buffer-file-name))))

(defun list-func ()
  (interactive)
  (occur "function"))

(defun ido-recentf-open ()
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
