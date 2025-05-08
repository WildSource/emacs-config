;; Melpa package repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Initial setups
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Add line number
(global-display-line-numbers-mode)

;; Package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; packages
(use-package magit
  :ensure t) ;; git gui

(use-package autothemer
  :ensure t) ;; theme manager

(use-package vterm
  :ensure t) ;; terminal emulator

(use-package web-mode
  :ensure t)

(use-package php-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package eglot
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  )

;; THEME
;; Set up path for custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Load your theme — this should work if the file provides it
(load-theme 'automata t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(autothemer fireplace haskell-mode magit org-modern php-mode
		vterm web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
