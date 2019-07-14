;; gud is a general interface for debugger
(require 'gud)
(define-key gud-mode-map (kbd "<f6>") 'gud-step)
(define-key gud-mode-map (kbd "<f7>") 'gud-next)
(define-key gud-mode-map (kbd "<f8>") 'gud-finish)
(add-hook 'gud-mode
	  (lambda ()
	    (setq completion-at-point-functions nil)))
