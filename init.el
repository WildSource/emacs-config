;;; init.el --- My emacs config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; debian apt command to run before evaluating the config file
;;
;; sudo apt update && sudo apt install -y git fzf build-essential cmake libtool-bin libvterm-dev libpoppler-glib-dev libpoppler-private-dev zlib1g-dev libpng-dev imagemagick qrencode texlive-latex-recommended texlive-fonts-recommended texlive-latex-extra	
;; l-cli on github

;; Initialize package system and add package archives
;; Melpa package repository
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; EMACS SPECIFIC CONFIG

;; font
(add-to-list 'default-frame-alist
	     '(font . "TempleOS-150"))

;; Change font size
(set-face-attribute 'default nil :height 150)

;; org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (sql . t)
   (ruby . t)
   (haskell . t)))
(setq org-image-actual-width nil)
(setq org-agenda-files '("~/emacs-notes/todo"))

;; autocompletion on file searching in dired mode
(ido-mode 1)
(setq ido-show-dot-for-dired t) ;; enable entering current dir

;; Enable abbreviations (expandable macros basically)
(setq-default abbrev-mode t)

;; Remove annoying ass bell
(setq ring-bell-function 'ignore)

;; Make buffer transparent
(set-frame-parameter nil 'alpha-background 70)
(add-to-list 'default-frame-alist '(alpha-background . 70))

;; Remove tool-bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Add line number
(global-display-line-numbers-mode)

;; Set initial buffer to *scratch*
(setq initial-buffer-choice t)
p
;; remove symlink prompt for splash and init.el
(defun my-suppress-vc-symlink-prompt ()
  (when (or (string-equal (file-truename buffer-file-name) "~/.emacs.d/splash")
	    (string-equal (file-truename buffer-file-name) "~/.emacs.d/init.el")))
    (setq-local vc-follow-symlinks t))
(add-hook 'find-file-hook #'my-suppress-vc-symlink-prompt)

;; Suppress compiler warnings from web-mode (deprecated stuff)
(setq warning-suppress-types '((comp)))

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

(use-package focus
  :ensure t
  :config
  (focus-mode 1))

(use-package beacon
  :ensure t)
(beacon-mode 1)

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

;; MAJOR MODES ---------------------------

(use-package eglot
  :ensure t
  :hook ((haskell-mode . eglot-ensure)
         (elm-mode . eglot-ensure)
	 (dart-mode . eglot-ensure)
	 (java-mode . eglot-ensure)
	 (arduino-mode . eglot-ensure))
  :config
  (setq eglot-confirm-server-edits nil))

(use-package web-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package dart-mode
  :ensure t)

(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload)))

(use-package elm-mode
  :ensure t)

(use-package arduino-mode
  :ensure t)

(use-package arduino-cli-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(setq plantuml-jar-path "~/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-output-type "svg")

;; AESTHETICS  -----------------------

(use-package darktooth-theme
  :ensure t)

(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (setq dimmer-fraction 0.50)
  (dimmer-mode t))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))  ;; Enable nyan-mode

;; KEYBINDS -----------------

(global-set-key (kbd "C-c i") 'open-init)
(global-set-key (kbd "C-c h") 'ivy-hoogle)

;; CUSTOM COMMANDS ------------------------------------

(defun export-plantuml-svg ()
  "Execute shell command of plantuml.jar to export to svg of opened .plantuml file in current buffer."
  (interactive)
  (let
      ((plantuml-exec-path "~/plantuml.jar"))
    (shell-command (format "java -jar %s -tsvg %s" plantuml-exec-path (buffer-file-name)))))

(with-eval-after-load 'plantuml-mode
  (define-key plantuml-mode-map (kbd "C-c C-o") 'export-plantuml-svg))

(defun open-init ()
  (interactive)
  (find-file "/home/wildsource/emacs-config/init.el"))
    
;; STUFF TO RUN AND ENABLE

(electric-pair-mode 1)

;; Load your theme — this should work if the file provides it
;;(load-theme 'darktooth t)
;;(load-theme 'automata t)
(load-theme 'purple-haze t)

;; BELOW IS MANAGED BY EMACS ITSELF

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(0blayout angular-mode arduino-cli-mode arduino-mode beacon chronos
	      consult darktooth-theme dart-mode dimmer
	      dired-video-thumbnail elm-mode exec-path-from-shell
	      ffmpeg-player flutter flycheck-eglot flycheck-elm
	      flycheck-haskell focus fzf gdscript-mode golden-ratio
	      ivy-hoogle magit move-text multiple-cursors nasm-mode
	      nyan-mode pdf-tools plantuml-mode rainbow-delimiters
	      signel slime squirrel-mode vterm web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
