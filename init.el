(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; fix projectile
(require 'subr-x)

;; basic
(set-language-environment "utf-8")
(add-to-list 'exec-path "/usr/local/bin")

(setq inhibit-startup-message t)
(setq counsel-find-file-at-point t)
(setq cursor-type 'bar)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inferior-lisp-program "sbcl")
(setq show-paren-delay 0)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq counsel-find-file-at-point t)
(setq counsel-find-file-ignore-regexp
      (concat
       ;; file names beginning with # or .
       "\\(?:\\`[#.]\\)"
       ;; file names ending with # or ~
       "\\|\\(?:[#~]\\'\\)"))

(projectile-global-mode)
(ivy-mode 1)
(recentf-mode 1)
(winner-mode 1)
(line-number-mode t)
(column-number-mode t)
(electric-indent-mode -1)
(tool-bar-mode -1)
(delete-selection-mode 1)
(scroll-bar-mode -1)
(show-paren-mode)

;; keys
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c 8") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c w") 'ace-window)
(global-set-key (kbd "C-c j") 'ace-jump-mode)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-c C-z") 'usrj/toggle-maximize-buffer)
(global-set-key (kbd "C-c r") 'counsel-recentf)
(global-set-key (kbd "C-c a") 'embrace-commander)
(global-set-key (kbd "M-<up>") 'usrj/move-line-up)
(global-set-key (kbd "M-<down>") 'usrj/move-line-down)
(global-set-key (kbd "M-S-<up>") 'usrj/copy-line-up)
(global-set-key (kbd "M-S-<down>") 'usrj/copy-line-down)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "s-b") 'counsel-switch-buffer-other-window)
(global-set-key (kbd "s-=") 'zoom)

(define-prefix-command 'usrj-map)
(global-set-key (kbd "M-,") 'usrj-map)
(define-key usrj-map (kbd "a") 'embrace-commander)
(define-key usrj-map (kbd "e") 'cider-eval-last-sexp)
(define-key usrj-map (kbd "m") 'cider-eval-defun-to-comment)
(define-key usrj-map (kbd "r") 'counsel-recentf)
(define-key usrj-map (kbd "<") 'winner-undo)
(define-key usrj-map (kbd ">") 'winner-redo)

;; projectile
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; wrap-region
(wrap-region-global-mode t)
(wrap-region-add-wrapper "#+BEGIN_EXAMPLE\n" "\n#+END_EXAMPLE" "#" 'org-mode)

;; hooks
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; org mode
(setq org-confirm-babel-evaluate nil)
(setq org-babel-clojure-backend 'cider)
(setq org-babel-python-command "python3")
;; (setq org-startup-folded nil)
(add-hook 'org-mode-hook (lambda ()
			   (org-indent-mode t)
			   (org-bullets-mode t)
			   (load-theme 'org-beautify)))
(setq org-babel-js-function-wrapper
      "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")
 
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (python . t)
   (shell . t)
   (js . t)
   (plantuml . t)))

(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-c t") 'org-babel-tangle-block)
     (define-key org-mode-map [(control tab)] nil)))

;; custom functions
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

(defun usrj/blog-date()
  (interactive)
  (insert (format-time-string "%A, %B %d, %Y")))

(defun usrj/toggle-maximize-buffer ()
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_) 
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

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
