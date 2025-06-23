;;; sp3-mode.el --- major mode for sp3

(eval-when-compile
  (require 'rx))

(defconst sp3--font-lock-defaults
  (let ((keywords '("var" "function" "if" "for" "end"))
	(types '()))
    `(((,(rx-to-string `(: (or ,@keywords))) 0 font-lock-keyword-face)
       (,(rx-to-string `(: (+ word) ":")) 0 font-lock-constant-face)))))

(defvar sp3-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defun sp3-indent-line ()
  "Indent current line."
  )

;;;###autoload
(define-derived-mode sp3-mode prog-mode "sp3"
  "Major mode for sp3 files."
  (setq font-lock-defaults sp3--font-lock-defaults)
  (setq comment-start "#")
  (setq comment-end "")
  (setq-local indent-line-function #'sp3-indent-line)
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sp3" . sp3-mode))
	      
(provide 'sp3-mode)
