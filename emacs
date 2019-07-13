;; install melpa package repository for extra packages
(require 'package)
;; run 'M-x package-refresh-contents RET' to get the melpa packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(package-selected-packages
   (quote
    (cmake-mode company company-jedi jedi auto-complete epc markdown-mode flyspell-popup xclip))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; the ido mode improve the path navigation in the mini buffer
(require 'ido)
(ido-mode t)

;; enable c++-mode at cuda cu.files
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; add llvm ir highlighting (from offical repo: https://github.com/llvm-mirror/llvm/tree/e74acf4ba7bb2df0b2a04548061b4c886efc368a/utils/emacs)
(setq load-path
    (cons (expand-file-name (concat (getenv "HOME") "/.emacs.d/lisp")) load-path))
  (require 'llvm-mode)

;; save all backup files to a central directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; self-defined general functions
(load-file (expand-file-name (concat (getenv "HOME") "/.emacs.d/lisp/help_function/autoload.el")))

; bind them to emacs's default shortcut keys:
(global-set-key (kbd "C-j") 'my-delete-line-backward) ; Ctrl+Shift+k
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "<C-delete>") 'my-delete-word)
(global-set-key (kbd "<M-DEL>") 'my-backward-delete-word)

 ;; enable copy/paste between emacs and x11
 ;; to install use following command in emcas (RET means key enter): M-x package-install RET xclip RET
(xclip-mode 1)
(setq select-enable-clipboard t)

 ;; swap shortcut of move to begin and move to indentation
(global-set-key (kbd "M-m") 'move-beginning-of-line)
(global-set-key (kbd "C-a") 'back-to-indentation)

 ;; show cursor position within line
(column-number-mode 1)

 ;; enable on the fly spellcheck
 ;;  spellcheck for the complete text
(add-hook 'text-mode-hook 'flyspell-mode)
 ;;  spellcheck just for comments and strings (depends on the prog language)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; flyspell popup-menu -> see:
;; install
;;  1. add melpa repository -> see beginning of file
;;  2. run 'M-x package-install RET flyspell-popup RET'
;; this defines a shortcut, to open the spellcheck pop-menu
(global-set-key (kbd "M-$") 'flyspell-popup-correct)
;; when this mode is enabled, the spellchecker pop menu opens if the cursor stays on the wrong word for more than one second
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

;; company is a backend for auto completion in different modes (https://company-mode.github.io/)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-#") 'company-complete)

;; remove trailing whitespaces in source code files
(defun nuke_traling ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook #'nuke_traling)

;; windmove is a built-in function that allows you to change the window selectively
;; it improves the functionality of 'C-x o'.
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; add function to reopen a file with root permission
;; C-x C-f <filename> RET M-x sudo-edit RET
;; emacs itself runs with user permission
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; the original shortcut is M-|
;; but some German keyboards are too cheep to execute this combination
(global-set-key (kbd "M-_")  'shell-command-on-region)

;; original shortcut is C-x z
(global-set-key (kbd "M-<f3>")  'repeat)

;; another shortcut for switching framework
(global-set-key (kbd "C-x <down>") 'other-frame)

;; another shortcut for comment-line
(global-set-key (kbd "C-x ;") 'comment-line)

;; gud is a general interface for debugger
(require 'gud)
(define-key gud-mode-map (kbd "<f6>") 'gud-step)
(define-key gud-mode-map (kbd "<f7>") 'gud-next)
(define-key gud-mode-map (kbd "<f8>") 'gud-finish)
(add-hook 'gud-mode
	  (lambda ()
	    (setq completion-at-point-functions nil)))

;; symon is a hardware monitor, which display cpu, memory and more in the mini-buffer
;; https://github.com/zk-phi/symon
(require 'symon)

(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-print-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c j") 'eval-defun)

;; ================== IDE extensions ==================
;; IDE extensions are really hard to handle. Therefore, it is sometimes better to disable them when they are not in use.

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

;; ====================== Macros ======================
;; load macros from file and maybe bind to key
;;(load-file "~/.emacs.d/macros/gen.macs")
;;(global-company-mode "\C-x\C-kT" 'myMacro)
