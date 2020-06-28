;; =============================================================================
;; ============================== web-mode config ==============================
;; =============================================================================

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; mode 2: Auto-close on > and </
(setq web-mode-auto-close-style 2)
;; highlight open and related close tag for element on current cursor position
(setq web-mode-enable-current-element-highlight t)

;; =============================================================================
;; ========================== web-mode-company config ==========================
;; =============================================================================

(require 'company)                                   ; load company mode
(require 'company-web-html)                          ; load company mode html backend
(require 'company-web-jade)                          ; load company mode jade backend
(require 'company-web-slim)                          ; load company mode slim backend

;; add company-web-html backend
(add-hook 'web-mode-hook
	  (lambda ()
	    (add-to-list (make-local-variable 'company-backends) '(company-web-html))
	    (company-mode t)))

;; enable flycheck and add html-tidy
(eval-after-load 'flycheck '(flycheck-add-mode 'html-tidy 'web-mode))

;; =============================================================================
;; =========================== impatient-mode config ===========================
;; =============================================================================

(require 'impatient-mode)
(add-hook 'web-mode-hook 'impatient-mode)
(add-hook 'css-mode-hook 'impatient-mode)

;; =============================================================================
;; ================================ scss config ================================
;; =============================================================================

;; use company-css backend because there is not company-scss backend
(add-hook 'scss-mode-hook
	  (lambda ()
	    (add-to-list (make-local-variable 'company-backends) '(company-css))
	    (company-mode t)))

;; The impatient mode has a list of watched files for every file, which was opened
;; in the webbrowser. If some of the files was modified, the webbrowser do a
;; refresh. By default, the impatient mode extract the information from the file
;; which was opened in the webbrowser. For scss files it doesn't work because,
;; they are not direct included in html files.
(defun imp-add-to-related-files (path)
  "Add file to imp-related-files of the current buffer."
  (interactive "sPath: ")
  (add-to-list 'imp-related-files path))

;; Add a refresh command to the save hook.
;; scss has a compiler step. So the css is just changing, when the scss file is
;; saved. The impatient mode just refresh, if the current buffer is modified.
;; Without the hook, you have to save the file and insert a new char in the
;; buffer, before the webbrowser refresh.
(defun impatient-update-scss ()
  (when (eq major-mode 'scss-mode)
    (sleep-for 0.5)
    (imp--on-change)))
(add-hook 'after-save-hook #'impatient-update-scss)

(defvar scss-exe "scss")

(defun run-scss-watch (input output)
  "Run a scss watchdog process."
  (interactive "DInput: \nDOutput: ")
  (start-process "scss" "scss" scss-exe "--watch" (concat input ":" output))
  )
