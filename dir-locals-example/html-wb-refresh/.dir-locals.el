((nil . ((eval . (add-hook 'mhtml-mode-hook
				 (lambda () (add-hook 'after-save-hook #'rfwb-webbrowser-refresh nil t)))
		     ))
	    ))
