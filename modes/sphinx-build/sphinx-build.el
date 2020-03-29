;;; sphinx-build.el --- Minor mode to build and display Sphinx Documentation

;; Copyright (C) 2020 Simeon Ehrig

;; Author: Simeon Ehrig
;; Version: 1.0.0
;; URL: https://github.com/SimeonEhrig/emacs-sphinx-build
;; Compatibility: GNU Emacs 26.x
;; Keywords: sphinx, tools, reStructed text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides `sphinx-build-mode', a minor mode for reStructed text
;;
;; Usage:
;;     (add-hook 'rst-mode-hook 'sphinx-build-mode)

;;; Code:


(defgroup sphinx-build nil
  "Helper functions to build and display a shpinx doc"
  )


(defcustom sphinx-makefile-path ""
  "Path to the Sphinx Makefile."
  :type 'string
  :group 'sphinx-build)

(defcustom sphinx-webbrowser "Mozilla Firefox"
  "Name of web browser, where the index.html is displayed."
  :type 'string
  :options '("Mozilla Firefox", "Chromium")
  :group 'sphinx-build
  )

(defun sphinx-set-build-path()
  "Set Path to the folder where the Sphinx Makefile is."
  (interactive)
  (setq sphinx-makefile-path (read-directory-name "Path to the Sphinx Makefile:"))
)

(defun sphinx-build ()
  "Run `make html`."
  (interactive)
  (save-buffer)
  (if (= (length sphinx-makefile-path) 0)
      (sphinx-set-build-path)
    ()
    )
  (let ((default-directory sphinx-makefile-path))
    (if (= (call-process "make" nil (get-buffer-create "sphinx-build-output") nil "html") 0)
	(message "build sphinx doc successful")
      (display-buffer "sphinx-build-output"))
    )
  )

(defun sphinx-get-webbrowser-search-string ()
  "Return a xdotool search string depending on the web browser."
  (if (string= sphinx-webbrowser "Mozilla Firefox")
      (concat "WID=$(xdotool search --name \"Mozilla Firefox\")")
    (if (string= sphinx-webbrowser "Chromium")
	(concat "WID=$(xdotool search --onlyvisible --class \"chromium\")")
      (concat "")
      )
    )
  )

; add while loop with sleep and abort counter
(defun sphinx-webbrowser-refresh ()
  "Run refresh on the current site in the web browser."
  (shell-command (concat
		  "CURRENT_WID=$(xdotool getwindowfocus) && "
		  (sphinx-get-webbrowser-search-string) " && "
		  "xdotool windowactivate $WID && "
		  "xdotool key F5 && "
		  "xdotool windowactivate $CURRENT_WID")
		 nil nil))

(defun sphinx-build-and-refresh()
  "Build Sphinx doc and refresh web browser."
  (interactive)
  (sphinx-build)
  (sphinx-webbrowser-refresh)
  )

(define-minor-mode sphinx-build-mode
  "Build a sphinx doc and view it in a webbroswer."
  :lighter " SphB"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c b") 'sphinx-build-and-refresh)
	    map))

(provide 'sphinx-build)
;;; sphinx.build.el ends here
