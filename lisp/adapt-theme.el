;;; adapt-theme.el --- changes theme based on sunset/sunrise -*- lexical-binding: t; -*-

;; Copyright (C) 2026 WildSource

;; Author: WildSource <ilovetrap123@hotmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: lisp, extensions
;; URL: https://github.com

;;; Commentary:
;; The package takes 2 themes (light and dark) and changes between them when it's sunrise or sunset

;;; Code:

(setq latitude 46.893796)
(setq longitude -71.198849)

(request "http://httpbin.org/get"
  :parser 'json-read
  :success (cl-function
            (lambda (&key data &allow-other-keys)
              (message "I sent: %S" (assoc-default 'args data)))))

(provide 'adapt-theme)
;;; adapt-theme.el ends here
