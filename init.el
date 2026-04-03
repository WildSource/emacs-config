;;; init.el --- My emacs config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; debian apt command to run before evaluating the config file
;;
;; sudo apt update && sudo apt install -y git fzf build-essential cmake libtool-bin libvterm-dev libpoppler-glib-dev libpoppler-private-dev zlib1g-dev libpng-dev imagemagick qrencode texlive-latex-recommended texlive-fonts-recommended texlive-latex-extra
;; install signal-cli on github

;; Initialize package system and add package archives
;; Melpa package repository
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; EMACS SPECIFIC CONFIG

;; org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (ruby . t)))
(setq org-image-actual-width nil)

;; 2-second refresh rate on emacs equivalent of htop
(setq proced-auto-update-flag t)
(setq proced-auto-update-interval 2)

;; autocompletion on file searching in dired mode
(ido-mode 1)
(setq ido-show-dot-for-dired t) ;; enable entering current dir

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

;; Change font size
(set-face-attribute 'default nil :height 150)

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install :no-query)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

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

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; MAJOR MODES ---------------------------

(use-package web-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package eglot
  :ensure t
  :hook ((haskell-mode . eglot-ensure)
         (elm-mode . eglot-ensure))
  :config
  (setq eglot-confirm-server-edits nil))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package haskell-mode
  :ensure t)

(use-package flycheck-haskell
  :ensure t)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

(use-package elm-mode
  :ensure t)

(use-package flycheck-elm
  :ensure t)
(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)

(use-package arduino-mode
  :ensure t)

(use-package arduino-cli-mode
  :ensure t)

;; AESTHETICS  -----------------------

(use-package darktooth-theme
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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

;; STUFF

;; PN is short for phone number
;; It is the file containing it so that it's not
;; hardcoded. Since the init.el is public on discord
(use-package signel
  :ensure t
  :config
  ;; REQUIRED: Your registered phone number
  (setq signel-account (with-temp-buffer
			 (insert-file-contents "~/PN")
			 (buffer-string)))
  (setq signel-command "/usr/local/bin/signal-cli"))

;; KEYBINDS -----------------

(global-set-key (kbd "C-c i") 'open-init)

;; recompile keybind
(global-set-key (kbd "C-c r") 'recompile)

;; CUSTOM COMMANDS ------------------------------------

(defun open-init ()
  (interactive)
  (find-file "/home/wildsource/emacs-config/init.el"))

(defun new-package (package-name short-summary)
  "Create elisp file in /lisp with header and footer.

PACKAGE-NAME lowercase string.

SHORT-SUMMARY preferably lowercase but not mandatory.
in the bottom example.

';;; example.el --- SHORT-SUMMARY -*- lexical-binding: t; -*-'."
  (interactive (list (read-string "Enter package name: ")
		     (read-string "Enter short summary: ")))
  (let ((filename (format "lisp/%s.el" package-name))
	(header (format ";;; %s.el --- %s -*- lexical-binding: t; -*-" package-name short-summary))
	(footer (format "(provide '%s)\n;;; %s.el ends here" package-name package-name)))
    (with-temp-file filename
      (insert header)
      (insert ";;; Commentary:")
      (insert ";;; Code:")
      (insert footer)))
  (message "(New package was created)"))
    
(defun toggle-window-split ()
  "Toggle between vertical and horizontal split with 2 windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (split-vertically-p
              (= (car this-win-edges)
                 (car next-win-edges)))
             (splitter
              (if split-vertically-p
                  #'split-window-horizontally
                #'split-window-vertically)))
        (delete-other-windows)
        (let ((first-window (selected-window)))
          (funcall splitter)
          (if split-vertically-p
              (set-window-buffer (next-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer))
          (set-window-buffer first-window
                             (if split-vertically-p
                                 next-win-buffer
                               this-win-buffer)))
        (other-window 1))))

(defalias 'tws 'toggle-window-split)

;; swap buffers left to right and vice versa for both vertical and horizontal split

(defun swap-window-buffers ()
  "Swap buffers between two windows."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((win1 (selected-window))
           (win2 (next-window))
           (buf1 (window-buffer win1))
           (buf2 (window-buffer win2)))
      (set-window-buffer win1 buf2)
      (set-window-buffer win2 buf1)
      (select-window win2))))

(defalias 'swb 'swap-window-buffers)
 
;; STUFF TO RUN AND ENABLE

(electric-pair-mode 1)

;; Load your theme — this should work if the file provides it
(load-theme 'darktooth t)
;;(load-theme 'automata t)
;;(load-theme 'purple-haze t)

;; BELOW IS MANAGED BY EMACS ITSELF

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(package-selected-packages
   '(arduino-cli-mode arduino-mode beacon darktooth-theme dimmer elm-mode
		      exec-path-from-shell flycheck-eglot flycheck-elm
		      flycheck-haskell focus fzf golden-ratio magit
		      move-text multiple-cursors nyan-mode pdf-tools
		      rainbow-delimiters signel vterm web-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "All good !")
