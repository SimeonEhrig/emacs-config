;; gtags tags different constructs in source-code and allows fast navigation
;; https://github.com/syohex/emacs-helm-gtags
;; https://primitattive.blogspot.com/2015/04/emacs-c-ide-gnu-global-helm-gtags-ask.html
(require 'helm-config)
(require 'helm-gtags)

;; Enable helm-gtags-mode
(setq helm-gtags-path-style 'relative)
(setq helm-gtags-ignore-case t)
;; FIXME: most of the ggtags process doesn't terminate
;;(setq helm-gtags-auto-update t)

;; helm-gtags local key bindings
(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "C-M-o") 'helm-gtags-find-files))

(defun gtags-init (path)
  "Create gtags in the specified path.
Runs the shell command: gtags --gtagslabel=new-ctags <path>"
  (interactive "D")
  (shell-command (concat "gtags --gtagslabel=new-ctags " path)))
