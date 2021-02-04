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

(projectile-global-mode)
(ivy-mode 1)
(recentf-mode 1)
(winner-mode 1)
(line-number-mode "on")
(column-number-mode "on")
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

(define-prefix-command 'usrj-map)
(global-set-key (kbd "M-,") 'usrj-map)
(define-key usrj-map (kbd "a") 'embrace-commander)
(define-key usrj-map (kbd "e") 'cider-eval-last-sexp)
(define-key usrj-map (kbd "m") 'cider-eval-defun-to-comment)
(define-key usrj-map (kbd "r") 'counsel-recentf)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; hooks
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

;; org mode
(setq org-confirm-babel-evaluate nil)
(setq org-babel-clojure-backend 'cider)
(setq org-startup-folded nil)
(add-hook 'org-mode-hook 'org-indent-mode)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (python . t)
   (shell . t)
   (plantuml . t)))

(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-c t") 'org-babel-tangle-block)))

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
