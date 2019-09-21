(defun disable-company-irony ()
  """Disable the company-irony backend in irony mode.

It removes the company-irony entry from company-backends.
"""
  (interactive)
  (setq company-backends (delete 'company-irony company-backends)))

;; enable irony mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; enable the company-irony by default
(add-hook 'irony-mode-hook
	  (lambda ()
	    (add-to-list 'company-backends 'company-irony)))

(defun irony-print-diagnostic ()
  """Print the clang diagnostic of the last completion command in the message buffer.

Attention, check that the irony-server is running before you run the command.
"""
  (interactive)
  (message "%S" (irony--run-task (irony--diagnostics-task))))
