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

;; =============================================================================
;; =========================== impatient-mode config ===========================
;; =============================================================================

(require 'impatient-mode)
(add-hook 'web-mode-hook 'impatient-mode)
