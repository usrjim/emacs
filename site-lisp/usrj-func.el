(defun usrj/depen ()
  (require 'ace-jump-mode)
  (require 'expand-region)
  (require 'yasnippet)
  (autoload 'emmet-mode "emmet-mode")
  (autoload 'markdown-mode "markdown-mode")
  (autoload 'web-mode "web-mode")
  (autoload 'ecb-autoloads "ecb-autoloads"))

(defun usrj/env ()
  (yas-reload-all)
  (setq cursor-type 'bar)
  (setq blink-cursor-interval 0)
  (set-language-environment "utf-8")
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq slime-net-coding-system 'utf-8-unix)
  (setq buffer-file-coding-system 'utf-8-unix)
  (add-hook 'web-mode-hook  'emmet-mode)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
  (slime-setup '(slime-fancy))
  ;; (setq inferior-lisp-program "/usr/bin/sbcl")
  (delete-selection-mode 1)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq inhibit-startup-message t)
  (cua-mode t)
  ;; (projectile-global-mode)
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-faces nil)
  (setq indent-line-function 'insert-tab)
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
  ;; (global-linum-mode t)
  (line-number-mode "on")
  (column-number-mode "on")
  ;; (electric-pair-mode 1)
  ;; (electric-indent-mode 1)
  (when (window-system)
    ;; (nyan-mode 1)
    (set-frame-parameter (selected-frame) 'alpha '(95 65))
    (tool-bar-mode t)
    (scroll-bar-mode -1))
  (usrj/keys))

(defun usrj/keys ()
  ;;; c-c <-> c-i
  (keyboard-translate ?\C-i ?\C-c)
  (keyboard-translate ?\C-c ?\C-i)
  (keyboard-translate ?\C-m ?\C-x)
  (keyboard-translate ?\C-x ?\C-m)

  (global-set-key (kbd "C-=") 'er/expand-region)

  ;;; multiple cursors
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

  ;;; f12 group, keys are on left hand side
  (define-prefix-command 'f12-map)
  (global-set-key (kbd "<f12>") 'f12-map)
  (define-key f12-map (kbd "s") 'save-buffer)
  (define-key f12-map (kbd "r") 'usrj/ido-recentf-open)
  (define-key f12-map (kbd "f") 'ido-find-file)
  (define-key f12-map (kbd "e") 'eval-last-sexp)
  (define-key f12-map (kbd "1") 'delete-other-windows)
  (define-key f12-map (kbd "2") 'split-window-below)
  (define-key f12-map (kbd "3") 'split-window-right)
  (define-key f12-map (kbd "q") 'top-level)
  (define-key f12-map (kbd "b") 'ido-switch-buffer)
  (define-key f12-map (kbd "c") 'usrj/copy-filename)

  ;;; f2 group, keys are on right hand side
  (define-prefix-command 'f2-map)
  (global-set-key (kbd "<f2>") 'f2-map)
  (define-key f2-map (kbd "0") 'delete-window)
  (define-key f2-map (kbd "o") 'other-window)
  (define-key f2-map (kbd "/") 'occur)
  (define-key f2-map (kbd "j") 'ace-jump-mode)
  (define-key f2-map (kbd "\\") 'company-complete)
  (define-key f2-map (kbd "k") 'kill-this-buffer)
  (define-key f2-map (kbd "p") 'projectile-command-map)
  (define-key f2-map (kbd "b") 'ido-switch-buffer)
  (define-key f2-map (kbd "m") 'mc/mark-all-like-this)
  (define-key f2-map (kbd "h") 'back-to-indentation)
  (define-key f2-map (kbd "u") 'beginning-of-visual-line)
  (define-key f2-map (kbd "i") 'end-of-visual-line)
  (define-key f2-map (kbd "l") '(lambda()(interactive)(insert-char #x03bb)))
  
  ;;; misc
  (global-set-key (kbd "M-<up>") 'usrj/move-line-up)
  (global-set-key (kbd "M-<down>") 'usrj/move-line-down)
  (global-set-key (kbd "M-RET") 'usrj/new-line)
  (global-set-key (kbd "<f7>") 'usrj/list-func)
  (global-set-key (kbd "<f11>") 'ace-jump-mode)
  (global-set-key (kbd "C-8") 'usrj/asterisk)
  (global-set-key (kbd "C-<f11>") 'magit-status)
  (global-set-key (kbd "C-<f7>") 'ecb-activate))

(defun usrj/menu () 
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
    '("One window" . delete-other-windows)))

(defun usrj/tool-bar ()
  (tool-bar-add-item "separator" 'ignore 'ignore :help "" :enable nil)
  (tool-bar-add-item "connect" 'eshell 'usrj-tb-esh)
  (tool-bar-add-item "ezimage/box-minus" 'sos 'usrj-tb-sos)
  (tool-bar-add-item "ezimage/bits" 'shell-command 'usrj-tb-shell)
  (tool-bar-add-item "ezimage/page" 'delete-other-windows 'usrj-tb-one-win)
  (tool-bar-add-item "ezimage/page-minus" 'delete-window 'usrj-tb-del-win)
  (tool-bar-add-item "right-arrow" 'split-window-right 'usrj-tb-win-right)
  (tool-bar-add-item "up-arrow" 'split-window-below 'usrj-tb-win-below)
  (tool-bar-add-item "newsticker/narrow" 'list-buffers 'usrj-tb-list-bfs))

(defun usrj/java-setup()
  (require 'eclim)
  (require 'company-emacs-eclim)
  (company-emacs-eclim-setup)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (add-hook 'java-mode-hook '(lambda()
                               (eclim-mode t)
                               (yas-minor-mode)))
  (add-hook 'eclim-mode-hook '(lambda ()
                                (local-set-key (kbd "<f6>") 'eclim-run-class)
                                (define-key-after global-map [menu-bar usrj-eclim]
                                  (cons "usrj-eclim" (make-sparse-keymap "usrj-eclim")))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-project-build]
                                  '("Project Build" . eclim-project-build)) 
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-project-create]
                                  '("Project Create" . eclim-project-create))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-problem-correct]
                                  '("Problem Correct" . eclim-problems-correct)) 
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-hierarchy]
                                  '("Show Hierarchy" . eclim-java-hierarchy))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-refactor-rename]
                                  '("Refactor Rename" . eclim-java-refactor-rename-symbol-at-point))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-sp2] '("--"))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-find-type]
                                  '("Find Type" . eclim-java-find-type))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-find-declaration]
                                  '("Find Declaration" . eclim-java-find-declaration))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-find-references]
                                  '("Find References" . eclim-java-find-references))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-show-documentation]
                                  '("Show Documentation" . eclim-java-show-documentation-for-current-element))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-problems]
                                   '("Show Problems" . eclim-problems-open))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-sp1] '("--"))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-import-organize]
                                  '("Import Organize" . eclim-java-import-organize))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-format]
                                  '("Format" . eclim-java-format))
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-run-class]
                                  '("Run Class" . eclim-run-class)))))

(defun usrj/php-setup ()
  (require 'php-mode)
  (add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
  (add-hook 'php-mode-hook '(lambda()
                              (setq c-basic-offset 2)
                              (setq tab-width 2)
                              (yas-minor-mode))))

(defun usrj/php-scratch ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*php scratch*"))
  (php-mode)
  (local-set-key (kbd "<f6>") 'usrj/run-php-scratch))

(defun usrj/run-php-scratch (beg end)
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

(defun usrj/kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun usrj/go-setup ()
  (require 'go-mode-load)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-i") 'go-import-add)
                             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                             (local-set-key (kbd "<f7>") 'godoc)
                             (set (make-local-variable 'company-backends) '(company-go))
                             (local-set-key (kbd "<f6>") 'usrj/go-run)
                             (yas-minor-mode))))

(defun usrj/go-run ()
  (interactive)
  (compile (concat "go run " (buffer-file-name))))

(defun usrj/list-func ()
  (interactive)
  (occur "function"))

(defun usrj/ido-recentf-open ()
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun usrj/move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun usrj/move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (usrj/move-line (if (null n) -1 (- n))))

(defun usrj/move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (usrj/move-line (if (null n) 1 n)))

(defun usrj/new-line()
  (interactive)
  (end-of-visual-line)
  (newline-and-indent))

(defun usrj/blog-date()
  (interactive)
  (insert (format-time-string "%A, %B %d, %Y")))

(defun usrj/asterisk()
  (interactive)
  (save-excursion
    (occur (thing-at-point 'symbol))
    (other-window 1)))

(defun usrj/copy-filename()
  (interactive)
  (let ((f (buffer-file-name)))
    (when f
      (kill-new (buffer-file-name))
      (message "file path copied"))))
