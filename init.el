;; Initialize package system and add package archives
;; Melpa package repository
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; autocompletion on file searching in dired mode
(ido-mode 1)
(setq ido-show-dot-for-dired t) ;; enable entering current dir

;; Remove tool-bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Add line number
(global-display-line-numbers-mode)

;; Custom splash screen
(setq initial-buffer-choice "~/.emacs.d/splash")

;; Change font size
(set-face-attribute 'default nil :height 200)

;; remove symlink prompt for splash and init.el
(defun my-suppress-vc-symlink-prompt ()
  (when (or (string-equal (file-truename buffer-file-name) "~/.emacs.d/splash")
	    (string-equal (file-truename buffer-file-name) "~/.emacs.d/init.el")))
    (setq-local vc-follow-symlinks t))

(add-hook 'find-file-hook #'my-suppress-vc-symlink-prompt)

;; Use web-mode for Blade templates
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))

;; Customize web-mode for Blade
(defun my-web-mode-hook ()
  (when (string-equal "blade.php" (file-name-extension buffer-file-name))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Suppress compiler warnings from web-mode (deprecated stuff)
(setq warning-suppress-types '((comp)))

;; Ensure use-package is installed
;; Package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Install and configure packages

;; essentials-------------------------

(use-package magit
  :ensure t) ;; git gui

;; Magit github account setup
(setenv "GIT_AUTHOR_NAME" "WildSource")
(setenv "GIT_AUTHOR_EMAIL" "ilovetrap123@hotmail.com")
(setenv "GIT_COMMITTER_NAME" "WildSource")
(setenv "GIT_COMMITTER_EMAIL" "ilovetrap123@hotmail.com")

(use-package autothemer
  :ensure t) ;; theme manager

(use-package vterm
  :ensure t) ;; terminal emulator

(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; dev modes---------------------------

(use-package web-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package php-mode
  :ensure t)

;; lsp -------------------------------

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

;; aesthetics -----------------------

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))  ;; Enable nyan-mode

;; Set up path for custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Load your theme — this should work if the file provides it
(load-theme 'automata t)

;; custom commands ------------------------------------

;; switch 2 buffers from vertical to horizontal split and vice versa
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(package-selected-packages
   '(0blayout autothemer exec-path-from-shell haskell-mode magit
	      nyan-mode pdf-tools php-mode slime vterm web-mode))
 '(warning-suppress-log-types '((use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
