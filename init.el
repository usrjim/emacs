(require 'package)

;;(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      clojure-mode
                      company-go
                      emacs-eclim
                      groovy-mode
                      cider
                      evil
                      evil-leader
                      expand-region
                      git-gutter-fringe
                      magit
                      projectile
                      paredit
                      rainbow-delimiters
                      markdown-mode
                      multiple-cursors
                      web-mode
                      yasnippet
                      slime
                      undo-tree
                      key-chord
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defvar infra-packages '(
                      puppet-mode
                      nginx-mode
                      vcl-mode
                      ))

(setq install-infra-packges nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq key-chord-two-keys-delay 0.5)

(when install-infra-packges
  (dolist (p infra-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(load "usrj-func")
(usrj/depen)
(usrj/env)
(usrj/menu)
(usrj/tool-bar)
(usrj/php-setup)
(usrj/go-setup)
(usrj/cider-setup)
(usrj/kotlin-setup)
;;(usrj/java-setup)

(setq default-directory "~/")
(blink-cursor-mode -1)
(setq cursor-type 'bar)
(evil-mode 1)

;;(require 'server)
;;(unless (server-running-p)
;;  (server-start)) 

;; (load "sos")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(eclim-eclipse-dirs (quote ("/opt/eclipse")))
 '(eclim-executable "/opt/eclipse/eclim")
 '(evil-toggle-key "C-`")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(show-paren-mode t)
 '(tool-bar-style (quote image)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
