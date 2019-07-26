;; absolute path of ~/.emacs.d
(setq emacs-home (expand-file-name (concat (getenv "HOME") "/.emacs.d/")))

;; =============================================================================
;; =========================== melpa package manager ===========================
;; =============================================================================

;; melpa package repository for extra packages
(require 'package)
;; run 'M-x package-refresh-contents RET' to get the melpa packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; =============================================================================
;; ============================== auto generated ===============================
;; =============================================================================

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
    (s dash dictcc helm-gtags helm-themes helm cmake-mode company company-jedi jedi epc markdown-mode flyspell-popup xclip))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:foreground "White" :background "brown"))))
 '(ediff-even-diff-A ((t (:background "blue"))))
 '(ediff-even-diff-Ancestor ((t (:background "blue"))))
 '(ediff-even-diff-B ((t (:background "blue"))))
 '(ediff-even-diff-C ((t (:background "blue"))))
 '(ediff-odd-diff-A ((t (:background "blue"))))
 '(ediff-odd-diff-Ancestor ((t (:background "blue"))))
 '(ediff-odd-diff-B ((t (:background "blue"))))
 '(ediff-odd-diff-C ((t (:background "blue")))))

;; =============================================================================
;; =================== install packages via package manager ====================
;; =============================================================================

;; all packages, which have to installed via emacs package manager
(setq my-package-list '(cmake-mode company company-jedi
				   dictcc epc flyspell-popup
				   helm helm-gtags helm-themes jedi
				   markdown-mode s xclip))

;; The file run_melpa contains the variable run-melpa. If the variable is true,
;; new packages must be installed.
;; The var is set by the setup.py script.
(load (concat emacs-home "run_melpa.el"))
(when run-melpa
  (progn
      (unless package-archive-contents
	(package-refresh-contents))
      (dolist (my-package my-package-list)
	(unless (package-installed-p my-package)
	  (progn
	    (package-install my-package)
	    (message "install package: %s" my-package)
	    )))
      ;; Set the variable to nil, otherwise all packages will be checked at
      ;; every start, which unnecessarily increases the start time.
      (write-region "(setq run-melpa nil)\n" nil (concat emacs-home "run_melpa.el")
		    )))


;; =============================================================================
;; ================ load emacs lisp functions from extra files  ================
;; =============================================================================

;; the directory contains packages which are not available at the package manager
(setq load-path (cons (concat emacs-home "lisp") load-path))
;; self-defined general functions
(load-file (concat emacs-home "lisp/help_function/autoload.el"))
;; configuration for different modes
(dolist (config (directory-files (concat emacs-home "modes") t ".el"))
  (load-file config))

;; =============================================================================
;; =============================== enable modes ================================
;; =============================================================================

;; the ido mode improve the path navigation in the mini buffer
(require 'ido)
(ido-mode t)
;; add llvm ir highlighting
;; from offical repo: https://github.com/llvm-mirror/llvm/tree/e74acf4ba7bb2df0b2a04548061b4c886efc368a/utils/emacs
(require 'llvm-mode)
;; symon is a hardware monitor, which display cpu, memory usage and more in the mini-buffer
;; https://github.com/zk-phi/symon
(require 'symon)

;; helm is a framework, which improve the UI of emacs
;; https://github.com/emacs-helm/helm
(require 'helm)
(helm-mode t)

;; =============================================================================
;; =========================== configure main usage ============================
;; =============================================================================

;; enable c++-mode at cuda cu.files
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; enable copy/paste between emacs and x11
;; to install use following command in emcas (RET means key enter): M-x package-install RET xclip RET
(xclip-mode 1)
(setq select-enable-clipboard t)

;; show cursor position within line
(column-number-mode 1)

;; enable on the fly spellcheck
;; spellcheck for the complete text
(add-hook 'text-mode-hook 'flyspell-mode)
;;  spellcheck just for comments and strings (depends on the prog language)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; when this mode is enabled, the spellchecker pop menu opens if the cursor stays on the wrong word for more than one second
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

;; company is a backend for auto completion in different modes (https://company-mode.github.io/)
(add-hook 'after-init-hook 'global-company-mode)

;; =============================================================================
;; ============================ global key bindings ============================
;; =============================================================================

;; replace the default emacs functions and do the same things without kill-ring
;; bind them to emacs's default shortcut keys:
(global-set-key (kbd "C-j") 'my-delete-line-backward) ; Ctrl+Shift+k
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "<C-delete>") 'my-delete-word)
(global-set-key (kbd "<M-DEL>") 'my-backward-delete-word)

;; swap shortcut of move to begin and move to indentation
(global-set-key (kbd "M-m") 'move-beginning-of-line)
(global-set-key (kbd "C-a") 'back-to-indentation)

;; shortcut to open the spellcheck pop-menu
(global-set-key (kbd "M-$") 'flyspell-popup-correct)

;; open company completion popup
(global-set-key (kbd "M-#") 'company-complete)

;; windmove is a built-in function that allows you to change the window selectively
;; it improves the functionality of 'C-x o'.
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
;; if the C-c bindings are overwritten by another mode
(global-set-key (kbd "C-x C-<left>")  'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)
(global-set-key (kbd "C-x C-<up>")    'windmove-up)
(global-set-key (kbd "C-x C-<down>")  'windmove-down)

;; the original shortcut is M-|
;; but some German keyboards are too cheep to execute this combination
(global-set-key (kbd "M-_")  'shell-command-on-region)

;; original shortcut is C-x z
(global-set-key (kbd "M-<f3>")  'repeat)

;; another shortcut for switching framework
(global-set-key (kbd "C-x <down>") 'other-frame)

;; another shortcut for comment-line
(global-set-key (kbd "C-x ;") 'comment-line)

;; add shortcut to the dired-mode
(add-hook
 'dired-mode-hook
 (lambda()
   (define-key dired-mode-map "J" 'ido-dired)))

;; replace default emacs functions with helm functions
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") #'helm-buffers-list)

;; allow smart completion with <TAB>
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)

;; =============================================================================
;; =================================== other ===================================
;; =============================================================================

;; save all backup files to a central directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; remove trailing whitespaces in source code files
(defun nuke_traling ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook #'nuke_traling)

;; =============================================================================
;; ================================== Macros ===================================
;; =============================================================================

;; load macros from file and maybe bind to key
;; (load-file "~/.emacs.d/macros/gen.macs")
;; (global-company-mode "\C-x\C-kT" 'myMacro)