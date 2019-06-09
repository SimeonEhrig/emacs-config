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
 '(package-selected-packages (quote (markdown-mode flyspell-popup xclip))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; C-j kills from the cursor back to the beginning of the line
(global-set-key "\C-j" '(lambda () (interactive) (kill-line 0)) )

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
