(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      clojure-mode
                      company-go
                      emacs-eclim
                      cider
                      evil
                      evil-leader
                      expand-region
                      git-gutter-fringe
                      magit
                      paredit
                      rainbow-delimiters
                      markdown-mode
                      multiple-cursors
                      web-mode
                      yasnippet
                      slime
                      undo-tree
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(load "usrj-func")
(usrj/depen)
(usrj/env)
(usrj/menu)
(usrj/tool-bar)
(usrj/php-setup)
(usrj/go-setup)
(usrj/cider-setup)
;;(usrj/java-setup)

(setq default-directory "~/")
(blink-cursor-mode -1)
(setq cursor-type 'bar)
(evil-mode 1)

;; (load "sos")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(custom-enabled-themes (quote (tango-dark)))
 '(tool-bar-style (quote image))
 '(eclim-eclipse-dirs '("/opt/eclipse"))
 '(eclim-executable "/opt/eclipse/eclim"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
