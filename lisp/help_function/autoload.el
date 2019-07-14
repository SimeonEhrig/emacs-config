;; The function is from the book "Learning GNU Emacs"
;; Publisher: O'REILLY
;; Author: Debra Cameron, James Elliott, Marc Loy, Eric Raymond & Bill Rosenblatt
(defun find-library-file (library)
  "Takes a single argument LIBRARY, being a library file to search for.
Searches for LIBRARY directly (in case relative to current directory,
or absolute) and then searches directories in load-path in order. It
will test LIBRARY with no added extension, then with .el, and finally
with .elc. If a file is found in the search, it is visited. If none
is found, an error is signaled. Note that order of extension searching
is reversed from that of the load function."
  (interactive "sFind library file: ")
  (let ((path (cons "" load-path)) exact match elc test found)
    (while (and (not match) path)
      (setq test (concat (car path) "/" library)
	    match (if (condition-case nil
			  (file-readable-p test)
			(error nil))
		      test)
	    path (cdr path)))
    (setq path (cons "" load-path))
    (or match
	(while (and (not elc) path)
	  (setq test (concat (car path) "/" library ".elc")
		elc (if (condition-case nil
			    (file-readable-p test)
			  (error nil))
			test)
		path (cdr path))))
    (setq path (cons "" load-path))
    (while (and (not match) path)
      (setq test (concat (car path) "/" library ".el")
	    match (if (condition-case nil
			  (file-readable-p test)
			(error nil))
		      test)
	    path (cdr path)))
    (setq found (or match elc))
    (if found
	(progn
	  (find-file found)
	  (and match elc
	       (message "(library file %s exists)" elc)
	       (sit-for 1))
	  (message "Found library file %s" found))
      (error "Library file \"%s\" not found." library))))

;; delete word and lines forwards and backwards without saving content in the kill-ring
;; the following functions complement the built-in emacs functions
;; source: http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  ;; if end of line, don't delete the word on the next or previous line
  ;; just remove the new line
  (if (eolp)
      (if (> arg 0)
	  (delete-forward-char 1)
	(backward-delete-char-untabify 1))
    (progn
      (delete-region
       (point)
       (progn
	 (forward-word arg)
	 (point)))
      )))

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
  (if (eolp)
      (delete-forward-char 1)
    (progn
      (delete-region
       (point)
       (progn (end-of-line 1) (backward-char 1) (point)))
      (delete-char 1))))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (if (bolp)
      (backward-delete-char-untabify 1)
    (let (p1 p2)
      (setq p1 (point))
      (beginning-of-line 1)
      (setq p2 (point))
      (delete-region p1 p2))))

(defun expand-headline-to-80 ()
  "Transform lisp comment into well-formatted headline with a length of 80.

Example:
Original : ;; Headline 1
Formatted: ;; ================================ Headline 1 =================================

The comment must begin with \";; \" and must not longer than 76 characters. If it is an empty comment line, the rest of the line is filled with =.
"
  (interactive)
  (save-excursion
    (setq currentLine (filter-buffer-substring (line-beginning-position) (line-end-position)))
    ;; string must begin with comment char and must not to be longer than 76 chars
    ;; at least 4 chars are necessary for "= <text> ="
    (if (and (string-prefix-p ";; " currentLine)
	     (<= (length currentLine) 76))
	(if (= (length currentLine) 3)
	    ;; line is empty
	    (progn
	      (goto-char (+ (line-beginning-position) 3))
	      (insert-char ?= 77))
	  (let* ((numberPrefix (/ (- 78 (length currentLine)) 2))
		 (numberSuffix (- 78 (length currentLine) numberPrefix)))
	    (progn
	      (goto-char (+ (line-beginning-position) 3))
	      (insert-char ?= numberPrefix)
	      ;; add whitespace
	      (insert-char ?\s 1)
	      (goto-char (line-end-position))
	      (insert-char ?\s 1)
	      (insert-char ?= numberSuffix))))))
  (move-end-of-line 1))

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
