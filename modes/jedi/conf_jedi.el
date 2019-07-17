;; jedi is a python mode (https://github.com/tkf/emacs-jedi)
;; it improves developing python in emacs
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:environment-root "jedi")  ; jedi is the name of the environment
;; company completion on jedi (https://github.com/syohex/emacs-company-jedi)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; run interactive python interpreter on script
;; 1. M-x run-python
;; C-c C-z
(setq python-shell-interpreter
      (expand-file-name
       (concat (getenv "HOME") "/.emacs.d/.python-environments/jedi/bin/python3")))

;; sphinx-doc generate documentation templates in python
;; source (fork): https://github.com/zasma/sphinx-doc.el
;; package-file is in $HOME/.emacs.d/lisp/sphinx-doc.el
;; shortcut: C-c M-d
(add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)))
;; enable documentation templates for *args and  **kwarg
(setq sphinx-doc-all-arguments t)
