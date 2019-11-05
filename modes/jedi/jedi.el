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

;; allows the flycheck modes pycompile and mypy to work together
;; http://wikemacs.org/wiki/Python#MyPy_checks
(require 'flycheck)
(flycheck-define-checker
    python-mypy ""
    :command ("mypy"
              "--ignore-missing-imports"
	      "--check-untyped-defs"
              "--python-version" "3.7"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)

(add-to-list 'flycheck-checkers 'python-pycompile t)
(flycheck-add-next-checker 'python-pycompile 'python-mypy t)
