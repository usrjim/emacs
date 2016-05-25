(defun usrj/depen ()
  (require 'ace-jump-mode)
  (require 'expand-region)
  (require 'yasnippet)
  (require 'undo-tree)
  (require 'git-gutter-fringe)
  (require 'evil-leader)
  ;;(require 'key-chord)
  (autoload 'scss-mode "scss-mode")
  (autoload 'emmet-mode "emmet-mode")
  (autoload 'markdown-mode "markdown-mode")
  (autoload 'web-mode "web-mode")
  (autoload 'kotlin-mode "kotlin-mode")
  (autoload 'ecb-autoloads "ecb-autoloads"))

(defun usrj/env ()
  (global-undo-tree-mode)
  (global-evil-leader-mode)
  (evil-mode 1)
  (setq evil-default-state 'normal)
  (setq evil-normal-state-cursor '(box "purple")
        evil-emacs-state-cursor '((bar . 3) "yellow"))
  (defalias 'evil-insert-state 'evil-emacs-state)
  ;;(key-chord-mode 1)
  ;;(key-chord-define evil-emacs-state-map  "jk" 'evil-normal-state)
  (define-key evil-emacs-state-map (kbd "M-n") 'evil-normal-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (evil-set-initial-state 'magit-status-mode 'emacs)
  (evil-set-initial-state 'magit-log-edit-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'nav-mode 'emacs)
  (evil-set-initial-state 'grep-mode 'emacs)
  (evil-set-initial-state 'cider-error-mode 'emacs)
  (evil-set-initial-state 'cider-repl-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (setq css-indent-offset 2)
  (setq ring-bell-function 'ignore)
  (global-git-gutter-mode t)
  ;; (set-face-attribute 'mode-line nil
  ;;                     :foreground "Black"
  ;;                     :background "DarkOrange"
  ;;                     :box nil)
  (yas-reload-all)
  (setq cursor-type 'bar)
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
  ;; (cua-mode t)
  ;; (projectile-global-mode)
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-faces nil)
  (setq indent-line-function 'insert-tab)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-always-indent t)
  (setq-default tab-width 2)
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
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (when (window-system)
    ;; (nyan-mode 1)
    ;;(set-frame-parameter (selected-frame) 'alpha '(95 65))
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1))
  (usrj/common-keys)
  (usrj/evil-keys))

(defun usrj/common-keys ()
  ;;; c-c <-> c-i
  (keyboard-translate ?\C-i ?\C-c)
  (keyboard-translate ?\C-c ?\C-i)
  (keyboard-translate ?\C-m ?\C-x)
  (keyboard-translate ?\C-x ?\C-m)

  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'redo)
  
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-\\") 'company-complete)

  ;;; multiple cursors
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "M-.") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-,") 'mc/mark-previous-like-this)

  ;;; misc
  (global-set-key (kbd "<f5>") 'execute-extended-command)
  (global-set-key (kbd "M-<up>") 'usrj/move-line-up)
  (global-set-key (kbd "M-<down>") 'usrj/move-line-down)
  (global-set-key (kbd "M-RET") 'usrj/new-line)
  (global-set-key (kbd "<f7>") 'usrj/list-func)
  (global-set-key (kbd "C-8") 'usrj/asterisk)
  (global-set-key (kbd "<f11>") 'magit-status)
  (global-set-key (kbd "C-<f7>") 'ecb-activate))

(defun usrj/evil-keys ()
  ;;; esc quits
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

   ;;; emacs normal keys
  (define-key evil-normal-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-y" 'yank)
  (define-key evil-visual-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-normal-state-map "\C-w" 'evil-delete)
  (define-key evil-insert-state-map "\C-w" 'evil-delete)
  (define-key evil-insert-state-map "\C-r" 'search-backward)
  (define-key evil-visual-state-map "\C-w" 'evil-delete)

  ;;; evil leader keys
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'scroll-up
    "3" 'scroll-down
    "7" 'menu-bar-mode
    "8" 'tool-bar-mode
    "a" 'beginning-of-visual-line
    "b" 'ido-switch-buffer
    "c" 'usrj/copy-filename
    "d" 'scroll-up
    "e" 'eval-last-sexp
    "f" 'ido-find-file
    "g" 'magit-status
    "h" 'back-to-indentation
    "i" 'end-of-visual-line
    "j" 'ace-jump-mode
    "k" 'kill-this-buffer
    "l" 'cider-load-buffer
    "m" 'mc/mark-all-like-this
    "n" 'cider-repl-set-ns
    "o" 'other-window
    "p" 'projectile-command-map
    "q" 'top-level
    "r" 'usrj/ido-recentf-open
    "s" 'occur
    "u" 'scroll-down
    "v" 'ido-find-alternate-file
    "w" 'ace-window
    "x" 'execute-extended-command
    "z" 'cider-switch-to-repl-buffer
    "]" 'abort-recursive-edit
    ">" 'git-gutter:next-hunk
    "<" 'git-gutter:previous-hunk
    "-" 'usrj/vsplit
    "|" 'usrj/split
    "/" 'occur
    ))

(defun usrj/keys ()
  ;;; M-j group
  (define-prefix-command 'mj-map)
  (global-set-key (kbd "M-j") 'mj-map)
  (define-key mj-map (kbd "0") 'delete-window)
  (define-key mj-map (kbd "1") 'delete-other-windows)
  (define-key mj-map (kbd "2") 'scroll-up)
  (define-key mj-map (kbd "3") 'scroll-down)
  (define-key mj-map (kbd "7") 'menu-bar-mode)
  (define-key mj-map (kbd "8") 'tool-bar-mode)
  (define-key mj-map (kbd "a") 'beginning-of-visual-line)
  (define-key mj-map (kbd "b") 'ido-switch-buffer)
  (define-key mj-map (kbd "c") 'usrj/copy-filename)
  (define-key mj-map (kbd "d") 'scroll-up)
  (define-key mj-map (kbd "e") 'eval-last-sexp)
  (define-key mj-map (kbd "f") 'ido-find-file)
  (define-key mj-map (kbd "g") 'magit-status)
  (define-key mj-map (kbd "h") 'back-to-indentation)
  (define-key mj-map (kbd "i") 'end-of-visual-line)
  (define-key mj-map (kbd "j") 'ace-jump-mode)
  (define-key mj-map (kbd "k") 'kill-this-buffer)
  (define-key mj-map (kbd "l") 'cider-load-buffer)
  (define-key mj-map (kbd "m") 'mc/mark-all-like-this)
  (define-key mj-map (kbd "n") 'cider-repl-set-ns)
  (define-key mj-map (kbd "o") 'other-window)
  (define-key mj-map (kbd "p") 'projectile-command-map)
  (define-key mj-map (kbd "q") 'top-level)
  (define-key mj-map (kbd "r") 'usrj/ido-recentf-open)
  (define-key mj-map (kbd "s") 'occur)
  (define-key mj-map (kbd "u") 'scroll-down)
  (define-key mj-map (kbd "v") 'ido-find-alternate-file)
  (define-key mj-map (kbd "w") 'ace-window)
  (define-key mj-map (kbd "x") 'execute-extended-command)
  (define-key mj-map (kbd "z") 'cider-switch-to-repl-buffer)
  (define-key mj-map (kbd "]") 'abort-recursive-edit)
  (define-key mj-map (kbd ">") 'git-gutter:next-hunk)
  (define-key mj-map (kbd "<") 'git-gutter:previous-hunk)
  (define-key mj-map (kbd "-") 'usrj/vsplit)
  (define-key mj-map (kbd "|") 'usrj/split)
  (define-key mj-map (kbd "/") 'occur)
  (define-key mj-map (kbd "\\") 'company-complete)
  (define-key mj-map (kbd "<down>") 'usrj/copy-line-down)
  (define-key mj-map (kbd "<up>") 'usrj/copy-line-up))

(defun usrj/menu () 
  (define-key-after global-map [menu-bar usrj]
    (cons "usrj" (make-sparse-keymap "usrj")))

  (define-key global-map [menu-bar usrj usrj-eclim]
    '("EClim project goto" . eclim-project-goto))
  (define-key global-map [menu-bar usrj usrj-cider]
    '("Cider jack in" . cider-jack-in))

  (define-key global-map [menu-bar usrj usrj-sp0] '("--"))
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
  (tool-bar-add-item "~/.emacs.d/img/terminal" 'eshell 'usrj-tb-esh)
  (tool-bar-add-item "~/.emacs.d/img/binary" 'shell-command 'usrj-tb-shell)
  (tool-bar-add-item "~/.emacs.d/img/maximize" 'delete-other-windows 'usrj-tb-one-win)
  (tool-bar-add-item "~/.emacs.d/img/minimize" 'delete-window 'usrj-tb-del-win)
  (tool-bar-add-item "~/.emacs.d/img/v-split" 'split-window-right 'usrj-tb-win-right)
  (tool-bar-add-item "~/.emacs.d/img/h-split" 'split-window-below 'usrj-tb-win-below)
  (tool-bar-add-item "~/.emacs.d/img/list" 'list-buffers 'usrj-tb-list-bfs)
  (tool-bar-add-item "~/.emacs.d/img/git" 'magit-status 'usrj-tb-magit)
  (tool-bar-add-item "~/.emacs.d/img/projectile" 'projectile-mode 'usrj-tb-projectile)
  (tool-bar-add-item "~/.emacs.d/img/org_mode" 'org-mode 'usrj-tb-org-mode)
  (tool-bar-add-item "~/.emacs.d/img/linum" 'linum-mode 'usrj-tb-linum)
  (tool-bar-add-item "~/.emacs.d/img/tree" 'undo-tree-visualize 'usrj-tb-undo-tree)
  (tool-bar-add-item "~/.emacs.d/img/paredit" 'paredit-mode 'usrj-tb-paredit)
  (tool-bar-add-item "~/.emacs.d/img/cider" 'cider-jack-in 'usrj-tb-cider))

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
                                (define-key global-map [menu-bar usrj-eclim usrj-eclim-project-goto]
                                  '("Project Goto" . eclim-project-goto)) 
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

(defun usrj/cider-setup()
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (setq cider-auto-select-error-buffer nil))

(defun usrj/kotlin-setup()
  (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))
  (add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-mode)))

(defun usrj/php-setup ()
  (require 'php-mode)
  (add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
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

(defun usrj/md-link(url)
  (interactive (list(read-string "url: ")))
  (let (title buf)
    (with-temp-buffer
      (url-insert-file-contents url)
      (setq buf (buffer-string))
      (string-match "<title>\\(.*\\)</title>" buf)
      (setq title (match-string 1 buf))
      (message ""))
    (insert (concat "[" title "](" url ")"))))

(defun usrj/copy-line-up()
  (interactive)
  (let (line)
    (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    (forward-line -1)
    (move-end-of-line nil)
    (newline)
    (insert line)))

(defun usrj/copy-line-down()
  (interactive)
  (let (line)
    (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    (move-end-of-line nil)
    (newline)
    (insert line)))

(defun usrj/vsplit ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (ido-find-file))

(defun usrj/split ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (ido-find-file))

(defun cider-diet-jack-in ()
  (interactive)
  (let* ((cider-diet-process (start-process "cider-diet-nrepl" "*cider-diet-nrepl*"
                                            "java" "-jar" cider-diet-path "7888")))
    (accept-process-output cider-diet-process)
    (cider-connect "localhost" 7888)))

