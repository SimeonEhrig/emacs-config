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
    (company company-jedi jedi auto-complete epc markdown-mode flyspell-popup xclip))))
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

;; delete word and lines forwards and backwards without saving content in the kill-ring
;; the following functions complement the built-in emacs functions
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (backward-char 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

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

;; sphinx-doc generate documentation templates in python
;; source (fork): https://github.com/zasma/sphinx-doc.el
;; package-file is in $HOME/.emacs.d/lisp/sphinx-doc.el
;; shortcut: C-c M-d
(add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)))
;; enable documentation templates for *args and  **kwarg
(setq sphinx-doc-all-arguments t)
