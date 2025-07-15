;;; sp3-mode.el --- major mode for sp3
(eval-when-compile
  (require 'rx))

(defconst sp3--rx-label
  '(: bol (* space) (+ word) (* space) (? (: "["  (* space) (+ word) (* space) "]" (* space))) ":" (* space) eol))

(defconst sp3--font-lock-defaults
  (let ((keywords '("else" "elsif" "end" "for" "function" "if" "shader" "var"))
	(types '()))
    `(((,(rx-to-string `(: symbol-start (or ,@keywords) symbol-end)) 0 font-lock-keyword-face)
       (,(rx-to-string sp3--rx-label) 0 font-lock-constant-face)))))

(defvar sp3-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?_ "w" st)
    st))

(defmacro sp3--current-indentation ()
  (let ((keywords '("else" "elsif" "end")))
    `(let ((done nil)
	   (indent (current-indentation)))
       (save-excursion
	 (while (not done)
	   (if (= (forward-line -1) -1)
	       (setq done t)
	     (back-to-indentation)
	     (pcase (thing-at-point 'symbol t)
	       ((or "function" "for" "if" "elsif" "else" "shader")
		(setq indent (+ (current-indentation) tab-width))
		(setq done t))
	       ((pred (lambda (_)
			(string-match (rx-to-string sp3--rx-label) (thing-at-point 'line t))) nil))
	       (token (when token
			(setq indent (current-indentation))
			(setq done t)))))))
       (save-excursion
	 (back-to-indentation)
	 (pcase (thing-at-point 'line t)
	   ((rx (: bol (* space) (or ,@keywords) eow (* anything)))
	    (setq indent (- indent tab-width)))
	   ((rx ,sp3--rx-label)
	    (setq indent 0))
	   ))
       indent)))

(defun sp3--indent-to (column)
  (let ((old-column (current-column))
	new-column)
    (save-excursion
      (back-to-indentation)
      (let ((col (current-column)))
	(if (>= column col)
	    (indent-to column)
	  (delete-char (- column col))))
      (setq new-column (current-column)))
    (if (< old-column new-column)
	(back-to-indentation))))

(defun sp3-indent-line ()
  "Indent current line."
  (let ((indent (sp3--current-indentation)))
    (sp3--indent-to indent)))

(defun sp3-align-instructions ()
  (interactive)
  (align-regexp (region-beginning)
		(region-end)
		(rx-to-string '(: bol (* space) (+ word) (group (+ space))))))

;;;###autoload
(define-derived-mode sp3-mode prog-mode "sp3"
  "Major mode for sp3 files."
  (setq font-lock-defaults sp3--font-lock-defaults)
  (setq-local indent-line-function #'sp3-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sp3$" . sp3-mode))
	      
(provide 'sp3-mode)
