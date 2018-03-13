(require 'package)

;;(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
                      clj-refactor
                      inf-clojure
                      flycheck
                      flycheck-clojure
                      company-go
                      company-jedi
                      elpy
                      magit
                      dumb-jump
                      ace-window
                      cider
                      writeroom-mode
                      emamux
                      eyebrowse
                      expand-region
                      git-gutter-fringe
                      robe
                      projectile
                      paredit
                      rainbow-delimiters
                      markdown-mode
                      multiple-cursors
                      web-mode
                      lua-mode
                      alchemist
                      undo-tree
                      which-key
                      restclient))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defvar infra-packages '(puppet-mode
                         nginx-mode
                         vcl-mode))

(setq install-infra-packges nil)

(setq clojure-jar-path "/opt/clojure/clojure-1.8.0.jar")
(setq inf-clojure-program "boot -C repl")

;; (setq mac-option-key-is-meta nil
;;       mac-command-key-is-meta t
;;       mac-command-modifier 'meta
;;       mac-option-modifier 'none)

(require 'server)
(unless (server-running-p)
  (server-start))

(add-hook 'after-init-hook #'global-flycheck-mode)

(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))) 
(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)

(add-hook 'after-init-hook 'usrj/lock-scratch-buffer t)

(when install-infra-packges
  (dolist (p infra-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setenv "GOROOT" "/opt/go")
(setenv "GOPATH" "/opt/gohome")
(add-to-list 'exec-path (concat (getenv "GOROOT") "/bin"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
(add-to-list 'exec-path "/usr/local/bin")

(load "usrj-func")
(usrj/depen)
(usrj/env)
(usrj/menu)
(usrj/keys)
(usrj/tool-bar)
(usrj/php-setup)
(usrj/ruby-setup)
(usrj/go-setup)
(usrj/clojure-setup)
(usrj/lua-setup)
(usrj/python-setup)

(setq default-directory "~/")
(blink-cursor-mode -1)
;;(setq-default mode-line-format nil)

(global-font-lock-mode 0)
(setq font-lock-global-modes nil)

(add-hook 'after-init-hook 'usrj/lock-scratch-buffer t)

(require 'server)
(unless (server-running-p)
  (server-start)) 

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (misterioso)))
 '(show-paren-mode t)
 '(tool-bar-style (quote image)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
