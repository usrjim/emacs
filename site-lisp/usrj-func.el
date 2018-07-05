(defun usrj/depen ()
  (require 'ace-jump-mode)
  (require 'expand-region)
  (require 'undo-tree)
  (require 'git-gutter-fringe)
  (require 'buffer-move)
  (require 'emamux)
  (autoload 'scss-mode "scss-mode")
  (autoload 'emmet-mode "emmet-mode")
  (autoload 'markdown-mode "markdown-mode")
  (autoload 'web-mode "web-mode")
  (autoload 'kotlin-mode "kotlin-mode")
  (autoload 'ecb-autoloads "ecb-autoloads"))

(defun usrj/env ()
  (package-initialize)
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (global-undo-tree-mode)
  (projectile-global-mode)
  (dumb-jump-mode 1)
  (which-key-mode 1)
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys)
  ;; (eyebrowse-mode 1)
  (setq css-indent-offset 2)
  (setq ring-bell-function 'ignore)
  (global-git-gutter-mode t)
  ;; (set-face-attribute 'mode-line nil
  ;;                     :foreground "Black"
  ;;                     :background "DarkOrange"
  ;;                     :box nil)
  (setq cursor-type 'bar)
  (set-language-environment "utf-8")
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.artist\\'" . artist-mode))
  (setq buffer-file-coding-system 'utf-8-unix)
  (add-hook 'web-mode-hook  'emmet-mode)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
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
  (electric-indent-mode -1)
  (winner-mode 1)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq treemacs-no-png-images t)
  (when (window-system)
    ;; (nyan-mode 1)
    ;;(set-frame-parameter (selected-frame) 'alpha '(95 65))
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1))
  (usrj/common-keys))

(defun usrj/translate-prefix-keys ()
  ;;; c-c <-> c-i
  ;; (when (display-graphic-p)
  ;;   (keyboard-translate ?\C-i ?\C-c)
  ;;   (keyboard-translate ?\C-c ?\C-i)
  ;;   (keyboard-translate ?\C-m ?\C-x)
  ;;   (keyboard-translate ?\C-x ?\C-m))
  )

(defun usrj/common-keys ()
  (usrj/translate-prefix-keys)
  (add-hook 'server-visit-hook 'usrj/translate-prefix-keys)
 
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'redo)
  (global-set-key "\C-cy" '(lambda ()
                             (interactive)
                             (popup-menu 'yank-menu)))
  ;; (global-set-key (kbd "C-c C-,") 'winner-undo)
  ;; (global-set-key (kbd "C-c C-.") 'winner-redo)
  (global-set-key (kbd "C-c m") emamux:keymap)
  (global-set-key (kbd "C-c C-l") 'emacs-lock-mode)
  (global-set-key (kbd "C-c w") 'ace-window)
  (global-set-key (kbd "C-c j") 'ace-jump-mode)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-\\") 'company-complete)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  ;;; multiple cursors
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "M-.") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-,") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

  ;;; misc
  ;; (global-set-key (kbd "<f12>") 'usrj/visit-ansi-term)
  (global-set-key (kbd "C-<tab>") 'other-window)
  (global-set-key (kbd "C-S-<tab>") '(lambda () (interactive) (other-window -1)))
  (global-set-key (kbd "<f5>") 'execute-extended-command)
  (global-set-key (kbd "M-<up>") 'usrj/move-line-up)
  (global-set-key (kbd "M-<down>") 'usrj/move-line-down)
  (global-set-key (kbd "M-RET") 'usrj/new-line)
  (global-set-key (kbd "<f7>") 'usrj/list-func)
  (global-set-key (kbd "<f8>") 'treemacs)
  (global-set-key (kbd "C-8") 'usrj/asterisk)
  (global-set-key (kbd "<f11>") 'magit-status)
  (global-set-key (kbd "C-<f7>") 'ecb-activate))

(defun usrj/keys ()
  ;;; M-j group
  (define-prefix-command 'mj-map)
  (global-set-key (kbd "C-,") 'mj-map)
  (define-key mj-map (kbd "1") 'delete-other-windows)
  ;;(define-key mj-map (kbd "2") ')
  ;;(define-key mj-map (kbd "3") ')
  (define-key mj-map (kbd "7") 'menu-bar-mode)
  (define-key mj-map (kbd "8") 'tool-bar-mode)
  (define-key mj-map (kbd "0") 'writeroom-mode)
  ;;(define-key mj-map (kbd "a") ')
  (define-key mj-map (kbd "b") 'ido-switch-buffer)
  ;;(define-key mj-map (kbd "c") ')
  (define-key mj-map (kbd "d") 'git-gutter:popup-hunk)
  ;;(define-key mj-map (kbd "e") ')
  (define-key mj-map (kbd "f") 'ido-find-file)
  (define-key mj-map (kbd "g") 'magit-status)
  (define-key mj-map (kbd "h") 'hs-toggle-hiding)
  ;;(define-key mj-map (kbd "i") ')
  (define-key mj-map (kbd "j") 'ace-jump-mode)
  (define-key mj-map (kbd "k") 'kill-buffer-and-window)
  (define-key mj-map (kbd "l") 'usrj/toggle-window-dedicated)
  (define-key mj-map (kbd "m") 'mc/mark-all-like-this)
  ;;(define-key mj-map (kbd "n") ')
  (define-key mj-map (kbd "o") 'other-window)
  (define-key mj-map (kbd "p") 'projectile-command-map)
  ;;(define-key mj-map (kbd "q") ')
  (define-key mj-map (kbd "r") 'usrj/ido-recentf-open)
  (define-key mj-map (kbd "s") 'save-buffer)
  ;;(define-key mj-map (kbd "t") ')
  ;;(define-key mj-map (kbd "u") ')
  (define-key mj-map (kbd "v") 'ido-find-alternate-file)
  (define-key mj-map (kbd "w") 'ace-window)
  (define-key mj-map (kbd "x") 'execute-extended-command)
  (define-key mj-map (kbd "y") 'usrj/copy-filename)
  (define-key mj-map (kbd "z") 'usrj/toggle-maximize-buffer)
  (define-key mj-map (kbd "-") 'winner-undo)
  (define-key mj-map (kbd "=") 'winner-redo)
  (define-key mj-map (kbd ">") 'git-gutter:next-hunk)
  (define-key mj-map (kbd "<") 'git-gutter:previous-hunk)
  (define-key mj-map (kbd "\"") 'usrj/vsplit)
  (define-key mj-map (kbd "%") 'usrj/split)
  (define-key mj-map (kbd "/") 'occur)
  (define-key mj-map (kbd "\\") 'company-complete)
  (define-key mj-map (kbd "}") 'buf-move-down)
  (define-key mj-map (kbd "{") 'buf-move-up)
  (define-key mj-map (kbd "[") 'buf-move-left)
  (define-key mj-map (kbd "]") 'buf-move-right))

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
  (tool-bar-add-item "~/.emacs.d/img/projectile" 'projectile-mode 'usrj-tb-projectile)
  (tool-bar-add-item "~/.emacs.d/img/org_mode" 'org-mode 'usrj-tb-org-mode)
  (tool-bar-add-item "~/.emacs.d/img/linum" 'linum-mode 'usrj-tb-linum)
  (tool-bar-add-item "~/.emacs.d/img/tree" 'undo-tree-visualize 'usrj-tb-undo-tree)
  (tool-bar-add-item "~/.emacs.d/img/paredit" 'paredit-mode 'usrj-tb-paredit)
  (tool-bar-add-item "~/.emacs.d/img/cider" 'cider-jack-in 'usrj-tb-cider))

(defun usrj/clojure-setup()
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  ;; (setq cider-auto-select-error-buffer nil)
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
  (add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode))
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))
  (add-hook 'clojure-mode-hook '(lambda ()
                                  (clj-refactor-mode 1)
                                  (yas-minor-mode 1)
                                  (cljr-add-keybindings-with-prefix "C-c C-m")
                                  (local-set-key (kbd "C-, c") 'cider-eval-defun-at-point)
                                  (local-set-key (kbd "C-, e") 'cider-eval-last-sexp)
                                  (local-set-key (kbd "C-, C-c") 'cider-eval-defun-at-point)
                                  (local-set-key (kbd "C-, C-e") 'cider-eval-last-sexp)
                                  (local-set-key (kbd "C-, i") 'cider-inspect-last-result)
                                  (paredit-mode)
                                  (rainbow-delimiters-mode))))

(defun usrj/run-lua-script ()
  (interactive)
  (save-buffer)
  (let ((f (buffer-file-name)))
    (message (format "running %s" f))
    (shell-command
     (format "luajit %s" (shell-quote-argument f)))))

(defun usrj/lua-setup()
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-hook 'lua-mode-hook '(lambda ()
                                 (electric-indent-mode -1)
                                 (local-set-key (kbd "C-c C-r") 'usrj/run-lua-script))))

(defun usrj/python-setup()
  (elpy-enable)
  ;; (elpy-use-ipython)
  ;; (add-hook 'python-mode-hook '(lambda ()
  ;;                                (jedi:setup)
  ;;                                (local-set-key (kbd "C-\\") 'company-jedi)))
  )

(defun usrj/php-setup ()
  (require 'php-mode)
  (add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-hook 'php-mode-hook '(lambda()
                              (setq c-basic-offset 4)
                              (setq tab-width 4))))

(defun usrj/php-scratch ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*php scratch*"))
  (php-mode)
  (local-set-key (kbd "C-c C-r") 'usrj/run-php-scratch))

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

(defun usrj/ruby-setup ()
  (add-hook 'ruby-mode-hook 'robe-mode)
  (eval-after-load 'company
  '(push 'company-robe company-backends)))

(defun usrj/kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun usrj/go-setup ()
  (require 'go-mode-autoloads)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-m") 'usrj/go-run)
                             (set (make-local-variable 'company-backends) '(company-go)))))

(defun usrj/go-run ()
  (interactive)
  (compile (concat "go run " (buffer-file-name))))

(defun usrj/list-func ()
  (interactive)
  (occur "function"))

(defun usrj/ido-recentf-open ()
  (interactive)
  (if (find-file-other-window (ido-completing-read "Find recent file: " recentf-list))
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
  ;; (interactive "cEnter b to select buffer or anything else to open file: ")
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  ;; (cond ((char-equal ?b c)
  ;;        (ido-switch-buffer-other-window))
  ;;       ((char-equal ?r c)
  ;;        (usrj/ido-recentf-open))
  ;;       (t (ido-find-file)))
  )

(defun usrj/split ()
  ;; (interactive "cEnter b to select buffer or anything else to open file: ")
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  ;; (cond ((char-equal ?b c)
  ;;        (ido-switch-buffer-other-window))
  ;;       ((char-equal ?r c)
  ;;        (usrj/ido-recentf-open))
  ;;       (t (ido-find-file)))
  )

(defun usrj/lock-scratch-buffer ()
  (set-buffer "*scratch*")
  (emacs-lock-mode))

(defun usrj/toggle-maximize-buffer ()
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_) 
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(require 'term)
(defun usrj/visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))


(defun usrj/toggle-window-dedicated ()
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

