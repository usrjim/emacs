(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(load "usrj-func")
(usrj/depen)
(usrj/env)
(usrj/menu)
(usrj/tool-bar)
(usrj/php-setup)
(usrj/go-setup)
(usrj/java-setup)

(setq default-directory "/opt")
(setq frame-title-format "ed")

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
