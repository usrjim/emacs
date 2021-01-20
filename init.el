(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; basic
(set-language-environment "utf-8")

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

(setq inhibit-startup-message t)
(setq counsel-find-file-at-point t)
(setq cursor-type 'bar)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; keys
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c 8") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-=") 'er/expand-region)

(define-prefix-command 'usrj-map)
(global-set-key (kbd "C-,") 'usrj-map)
(define-key usrj-map (kbd "a") 'embrace-commander)
(define-key usrj-map (kbd "e") 'cider-eval-last-sexp)
(define-key usrj-map (kbd "m") 'cider-eval-defun-to-comment)
(define-key usrj-map (kbd "r") 'counsel-recentf)

;; hooks
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

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

;; custom variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
