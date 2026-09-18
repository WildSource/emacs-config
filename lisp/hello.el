;;; hello.el --- A brief description of my package -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Your Name

;; Author: Your Name <you@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: lisp, extensions
;; URL: https://github.com

;;; Commentary:
;; Put a longer description of what your package does right here.

;;; Code:

(defun my-package-hello ()
  "Say hello to the user."
  (interactive)
  (message "Hello from my custom package!"))

(provide 'hello)
;;; my-package.el ends here
