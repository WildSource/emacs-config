(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package magit
  :ensure t)

(use-package autothemer
  :ensure t)

;; Set up path for custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Load your theme — this should work if the file provides it
(load-theme 'automata t)

(message "init.el loaded ✅")
