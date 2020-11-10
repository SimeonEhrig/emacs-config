;; jedi is a python mode (https://github.com/tkf/emacs-jedi)
;; it improves developing python in emacs
(add-hook 'python-mode-hook 'jedi:setup)

(add-hook
 'jedi-mode-hook
 (lambda()
   ;; show completion hints
   (define-key jedi-mode-map (kbd "M-#") (function jedi:complete))
   ;; accept a completion
   (define-key ac-complete-mode-map (kbd "M-#") 'ac-expand)
   )
 )

;; the function sets the Python environment of the Jedi auto-completion
;; there is a order which environment is used if the environment before is not set
;; 1. the environment which is set by the environment variable VIRTUAL_ENV
;; 2. the environment which is set ny the Emacs lisp variable my-python-default-env
;; 3. the environments in which the Jedi server is installed
(defun my/default_python_env ()
  (if (and (not (getenv "VIRTUAL_ENV")) (boundp 'my-python-default-env))
    (set (make-local-variable 'jedi:server-args) (list "--virtual-env" my-python-default-env))
    )
  )
(add-hook 'python-mode-hook 'my/default_python_env)

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

;; if variable is true, enable blacken mode in this buffer
(setq my-enbale-blacken-mode nil)

(add-hook
 'python-mode-local-vars-hook
 (lambda()
   (if my-enbale-blacken-mode
       (blacken-mode)
     )
   )
 )
