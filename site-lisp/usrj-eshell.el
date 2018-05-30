
(require 'eshell)
(require 'em-smart)

;;;
;;; custom path
;;;
(add-hook 'eshell-mode-hook
          '(lambda nil
             (setq eshell-path-env (concat "/Users/user/homebin:/usr/local/bin:" eshell-path-env))
             (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
             ;; (eshell/export "EPOCROOT=\\Paragon\\")
             (eshell-smart-initialize)
             (local-set-key "\C-u" 'eshell-kill-input)))

;;
;; custom commands
;;
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/x ()
  "Quit eshell and kill window."
  (insert "exit")
  (eshell-send-input)
  (delete-window))


;;
;; util functions
;;
(defun quarter-window-vertically ()
  "Create a new window a quarter size of the current window."
  (split-window-vertically)
  (other-window 1)
  (split-window-vertically)
  (other-window -1)
  (delete-window))

(defun open-mini-eshell ()
  "Open a mini-eshell in a small window at the bottom of the current window."
  (interactive)
  (quarter-window-vertically)
  (other-window 1)
  (eshell))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))
(global-set-key (kbd "<f12>") 'eshell-here)

;;
;; git branch name prompt
;;
(defun git-prompt-branch-name ()
  "Get current git branch name."
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (with-temp-buffer
      (apply #'process-file "git" nil (list t nil) nil args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun 4lex1v:eshell-prompt ()
  (let ((branch-name (git-prompt-branch-name)))
    (concat
     "\n# " (user-login-name) " in " (abbreviate-file-name (eshell/pwd)) "\n"
     (if branch-name (format "git:(%s) >> " branch-name) ">> "))))

;;
;; setup eshell
;;
(setq eshell-prompt-function #'4lex1v:eshell-prompt
      eshell-prompt-regexp ".*>>+ ")

(setq eww-search-prefix "https://www.google.com/search?q=")

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq tramp-ssh-controlmaster-options "")
