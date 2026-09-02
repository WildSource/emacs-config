 ;;; init.el --- My emacs config -*- lexical-binding: t -*-

;;; Commentary: debian packages to install -> libtool-bin fzf cmake

;;; Code:

;; Initialize package system and add package archives
;; Melpa package repository
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; EMACS SPECIFIC CONFIG

;; abbrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (sql . t)
   (ruby . t)
   (haskell . t)))
(setq org-image-actual-width nil)

;; autocompletion on file searching in dired mode
(ido-mode 1)
(setq ido-show-dot-for-dired t) ;; enable entering current dir

;; Enable abbreviations (expandable macros basically)
(setq-default abbrev-mode t)

;; Remove annoying ass bell
(setq ring-bell-function 'ignore)

;; Remove tool-bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Add line number
(global-display-line-numbers-mode)

;; Set initial buffer to *scratch*
(setq initial-buffer-choice t)

;; Suppress compiler warnings from web-mode (deprecated stuff)
;;(setq warning-suppress-types '((comp)))

;; USE-PACKAGES -------------------------

;; Ensure use-package is installed
;; Package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; EMACS ESSENTIALS

;; Multiple cursor keybinds
(use-package multiple-cursors
  :ensure t
  :bind (("C-c n" . mc/mark-next-like-this)
	 ("C-c p" . mc/mark-previous-like-this)
	 ("C-c a" . mc/mark-all-like-this)))

(use-package magit
  :ensure t)
(setenv "GIT_AUTHOR_NAME" "WildSource")
(setenv "GIT_AUTHOR_EMAIL" "ilovetrap123@hotmail.com")
(setenv "GIT_COMMITTER_NAME" "WildSource")
(setenv "GIT_COMMITTER_EMAIL" "ilovetrap123@hotmail.com")

(use-package autothemer
  :ensure t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(use-package vterm
  :ensure t)

(use-package consult
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments '("-i"))
  :config
  (exec-path-from-shell-initialize))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install :no-query)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package ivy-hoogle
  :ensure t
  :bind ("C-c h" . ivy-hoogle)
  :config
  (ivy-mode 1))

(use-package fzf
  :ensure t
  :bind ("C-c f" . fzf)
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; QUALITY OF LIFE
(use-package beacon
  :ensure t)
(beacon-mode 1)

;; MAJOR MODES ---------------------------

(use-package eglot
  :ensure t
  :hook ((haskell-mode . eglot-ensure)
         (elm-mode . eglot-ensure)
	 (ruby-mode .eglot-ensure))
  :config
  (setq eglot-confirm-server-edits nil))

(use-package web-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package ruby-mode
  :ensure t)
(with-eval-after-load 'eglot
 (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

;; AESTHETICS  -----------------------

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))  ;; Enable nyan-mode

;; KEYBINDS -----------------

(global-set-key (kbd "C-c i") 'open-init)
(global-set-key (kbd "C-c h") 'ivy-hoogle)

;; CUSTOM COMMANDS ------------------------------------

(defun open-init ()
  (interactive)
  (find-file "~/emacs-config/init.el"))

(defun battery-notification ()
  "If battery power under 20% notifies user through emacs"
  (let* ((battery-power-str (string-trim (shell-command-to-string "cat /sys/class/power_supply/BAT0/capacity")))
	 (battery-power-num (string-to-number battery-power-str))
	 (formatted-message (concat "Careful ! Your battery is at " battery-power-str)))
    (when (<= battery-power-num 20)
            (shell-command (format "notify-send \"%s\"\"" formatted-message)))))
    
;; STUFF TO RUN AND ENABLE

(electric-pair-mode 1)

(when (not (string-equal (system-name) "tux"))
   (run-with-timer 300 300 #'battery-notification)
   (message "battery notification daemon started !"))

;; Load your theme — this should work if the file provides it
(load-theme 'automata t)
;;(load-theme 'green-phosphor t)


;; BELOW IS MANAGED BY EMACS ITSELF
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(autothemer beacon consult elm-mode
		exec-path-from-shell focus fzf 
		haskell-mode ivy-hoogle magit multiple-cursors
		nyan-mode pdf-tools vterm web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
