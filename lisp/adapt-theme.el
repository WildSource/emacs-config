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

(defun get-sun-data ()
  ""
  (interactive)
  (request "https://api.sunrise-sunset.org/v2?lat=36.7201600&lng=-4.4203400"
  :parser  'json-read
  :success (cl-function
	    (lambda (&key data &allow-other-keys)
	      (let* ((sunrise-cons (decode-sunrise data))
		    (sunset-cons (decode-sunset data))
		    (sunrise (cdr sunrise-cons))
		    (sunset (cdr sunset-cons)))
		(insert (format "%s, %s" sunrise sunset)))))))

(defun decode-sunrise (data)
  "get sunrise time from json data"
  (assoc 'sunrise data))

(defun decode-sunset (data)
  "get sunset time from json data"
  (assoc 'sunset data))

(defun get-current-time ()
  "get time from clock"
  (insert (format-time-string "%H:%M")))
  
(provide 'adapt-theme)
;;; adapt-theme.el ends here
