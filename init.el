(require 'package)

;;(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
                      inf-clojure
                      company-go
                      ;;emacs-eclim
                      groovy-mode
                      ace-window
                      cider
                      evil
                      evil-leader
                      expand-region
                      git-gutter-fringe
                      robe
                      magit
                      projectile
                      paredit
                      rainbow-delimiters
                      markdown-mode
                      multiple-cursors
                      web-mode
                      yasnippet
                      slime
                      lua-mode
                      alchemist
                      undo-tree
                      which-key
                      restclient
                      key-chord))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defvar infra-packages '(puppet-mode
                      nginx-mode
                      vcl-mode))

(setq install-infra-packges nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq key-chord-two-keys-delay 0.5)
(setq cider-diet-path (expand-file-name (concat (getenv "HOME")
                                                "/homebin/cider-diet-0.1.0-SNAPSHOT-standalone.jar")))
(setq clojure-jar-path "/opt/clojure/clojure-1.8.0.jar")
(setq inf-clojure-program "boot -C repl")

(when install-infra-packges
  (dolist (p infra-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

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
(usrj/kotlin-setup)
(usrj/lua-setup)
;;(usrj/java-setup)

(setq default-directory "~/")
(blink-cursor-mode -1)
(setq cursor-type 'bar)
(evil-mode 1)

(setenv "GOROOT" "/opt/go")
(setenv "GOPATH" "/opt/gohome")
(add-to-list 'exec-path (concat (getenv "GOROOT") "/bin"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))

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
 '(custom-enabled-themes (quote (tango-dark)))
 ;;'(eclim-eclipse-dirs (quote ("/opt/eclipse")))
 ;;'(eclim-executable "/opt/eclipse/eclim")
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

