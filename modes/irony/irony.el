;; =============================================================================
;; =============================== irony config ================================
;; =============================================================================

(defun disable-company-irony ()
  """Disable the company-irony backend in irony mode.

It removes the company-irony entry from company-backends.
"""
  (interactive)
  (setq company-backends (delete 'company-irony company-backends)))

;; enable irony mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun irony-print-diagnostic ()
  """Print the clang diagnostic of the last completion command in the message buffer.

Attention, check that the irony-server is running before you run the command.
"""
  (interactive)
  (message "%S" (irony--run-task (irony--diagnostics-task))))

;; =============================================================================
;; =============================== rtags config ================================
;; =============================================================================

;; https://github.com/Andersbakken/rtags/wiki/Usage
(setq rtags-use-helm t)
(setq rtags-display-result-backend 'helm)

(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

(defun tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
      (gtags-find-tag)))
(defun tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
      (gtags-find-rtag)))
(defun tags-find-symbol ()
  (interactive)
  (call-interactively (if (use-rtags) 'rtags-find-symbol 'gtags-find-symbol)))
(defun tags-find-references ()
  (interactive)
  (call-interactively (if (use-rtags) 'rtags-find-references 'gtags-find-rtag)))
(defun tags-find-file ()
  (interactive)
  (call-interactively (if (use-rtags t) 'rtags-find-file 'gtags-find-file)))
(defun tags-imenu ()
  (interactive)
  (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

(require 'cc-mode)
(define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
(define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
(define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
(define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
(define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
(define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

(define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
(define-key global-map (kbd "M-,") (function tags-find-references-at-point))
(define-key global-map (kbd "M-;") (function tags-find-file))
(define-key global-map (kbd "C-.") (function tags-find-symbol))
(define-key global-map (kbd "C-,") (function tags-find-references))
(define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
(define-key global-map (kbd "M-i") (function tags-imenu))


;; =============================================================================
;; ================================ tags setup =================================
;; =============================================================================
;; FIXME: ggtags causes some key binding errors, so it es disabled at the moment

;; enable the ggtags mode in the c++ mode to use it together with projectile
;; (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode)
;;                 (ggtags-mode 1))
;; 	      (define-key ggtags-mode-map "\M-." nil)
;; 	      (define-key ggtags-mode-map "\M-]" nil)
;; 	      (define-key ggtags-mode-map "\C-M-." nil)
;; 	      (setq ggtags-mode-prefix-key "\C-c")
;; 	      ))
;; (setq projectile-tags-backend 'ggtags)

;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)

;; https://github.com/Andersbakken/rtags/wiki/Usage#fall-back-to-other-taggers
(defun use-rtags (&optional useFileManager)
  (and (rtags-executable-find "rc")
       (cond ((not (gtags-get-rootpath)) t)
             ((and (not (eq major-mode 'c++-mode))
                   (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
             (useFileManager (rtags-has-filemanager))
             (t (rtags-is-indexed)))))

;; =============================================================================
;; =============================== company setup ===============================
;; =============================================================================

;; enable the company-irony by default
(add-hook 'irony-mode-hook
	  (lambda ()
	    (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

;; =============================================================================
;; ============================== flycheck setup ===============================
;; =============================================================================

;; enable flycheck-irony
(require 'flycheck)
;; to check if we are in a project
(require 'projectile)

;; the flycheck-hook is before the <mode>-local-vars-hook
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; ==============================
;; ==== irony flycheck setup ====
;; ==============================

;; for internal usage: enable irony flycheck
(setq my/cpp-enable-irony-flycheck t)
;; for external usage: enable irony flycheck in any case
(setq my/cpp-force-irony-flycheck nil)

;; run irony flycheck verification after loading the .dir-locals file
(add-hook 'c++-mode-local-vars-hook 'my/cpp-irony-verify-flycheck)

(defun my/cpp-irony-verify-flycheck ()
  "Disable irony flycheck in projects, which has no manual configuration."
  (progn
    ;; if irony flycheck is forced, enable it
    ;; used for example in project files
    (if my/cpp-force-irony-flycheck
	(progn
	  (message "my/cpp: force enable irony flycheck")
	  (setq-local my/cpp-enable-irony-flycheck t)
	  )
      ;; check if the C++ file is in a project
      ;; a project has a high probability that it may contain dependencies that
      ;; cannot be resolved automatically
      (if (projectile-project-p)
	  (progn
	    (message "my/cpp: found project: %s" (projectile-project-p))
	    (setq-local my/cpp-enable-irony-flycheck nil)
	    )
	)
      )
    ;; disable irony flycheck if necessary
    (if (eq my/cpp-enable-irony-flycheck nil)
	(progn
	  (message "my/cpp: disable irony flycheck")
	  (make-local-variable 'flycheck-disabled-checkers)
	  (add-to-list 'flycheck-disabled-checkers 'irony)
	  )
      )
    )
  )

;; ==============================
;; ==== rtags flycheck setup ====
;; ==============================

;; https://github.com/Andersbakken/rtags/wiki/Usage#rtags-flycheck-integration
;; add rtags flychek to flycheck
(require 'flycheck-rtags)

;; for external usage: enable rtags flycheck in any case
(setq my/cpp-force-rtags-flycheck nil)

;; run rtags flycheck verification after loading the .dir-locals file
(add-hook 'c++-mode-local-vars-hook #'my/cpp-flycheck-rtags-setup)

(defun my/cpp-flycheck-rtags-setup ()
  "Deactivate rtags flycheck afterwards.
Leave it active only when manually forced."
  ;; if enforced, configure rtags flycheck
  (if my/cpp-force-rtags-flycheck
      (progn
	(setq rtags-autostart-diagnostics t)
	(flycheck-select-checker 'rtags)
	(setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
	(setq-local flycheck-check-syntax-automatically nil)
	)
    ;; disable flychek rtags
    (progn
      (message "my/cpp: disable rtags flycheck")
      (make-local-variable 'flycheck-disabled-checkers)
      (add-to-list 'flycheck-disabled-checkers 'rtags)
      )
    )
  )

;; =============================================================================
;; ========================= projectile cmake extras ===========================
;; =============================================================================
;; some helper functions for projectile project configuration files for cmake

(setq cm-build-dir "build")
(setq cm-build-type "RELEASE")
;; %s is replaced by the build directory path
(setq cm-configure-cmd "cmake %s")
(setq cm-run-cmd "")
;; if conda environment is set, configure, compile and run command are prefixed
;; with conda run -n <env_name> <command>
(setq cm-conda-env "")
;; if conda environment is set, configure, compile and run command are prefixed
;; with singularity exec <cm-singularity-env-args> <image.sif> <command>
(setq cm-singularity-env "")
(setq cm-singularity-env-args "")

(defun cm-print-build-type ()
  "Print the CMAKE_BUILD_TYPE."
  (interactive)
  (message "CMake Build type: %S" cm-build-type)
  )

(defun cm-get-env-run-cmd (input-cmd)
  "Set a prefix command at INPUT-CMD, if conda or singularity environment is set."
  (setq prefix "")
  (if (not (string= "" cm-conda-env))
      (setq prefix (concat conda-env-home-directory "/condabin/conda run -n " cm-conda-env " "))
    )
  (if (not (string= "" cm-singularity-env))
      (setq prefix (concat "singularity exec " cm-singularity-env-args " " cm-singularity-env " "))
    )
  (concat prefix input-cmd)
  )

(defun cm-set-projectile-project-type ()
  "Run projectile-register-project-type with cm-* variables."
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
				    :compilation-dir (concat cm-build-dir "_" (downcase cm-build-type))
				    :configure (cm-get-env-run-cmd
						(concat
						 cm-configure-cmd
						 " -DCMAKE_BUILD_TYPE="
						 cm-build-type
						 ))
				    :compile (cm-get-env-run-cmd "cmake --build .")
				    :run (cm-get-env-run-cmd cm-run-cmd)
				    :test "ctest")
  )

(defun cm-switch ()
  "Switch between DEBUG and RELEASE BUILD_TYPE."
  (interactive)
  (progn
    (if (string= "RELEASE" cm-build-type)
	(progn
	  (setq cm-build-type "DEBUG")
	  (message "Set CMake Build type to: DEBUG"))
      (progn
	(setq cm-build-type "RELEASE")
	(message "Set CMake Build type to: RELEASE"))
      )
    (cm-set-projectile-project-type)
    )
  )
