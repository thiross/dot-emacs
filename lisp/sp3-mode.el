;;; -*- lexical-binding: t; -*-
;;; sp3-mode.el --- major mode for sp3
(eval-when-compile
  (require 'rx))

(defconst sp3--rx-label
  '(: bol (* blank)
      (group (+ word) (* blank)
             (? (: "[" (+ (not (any "[]\n"))) "]" (* blank)))
             ":")
      (* blank) (or eol "//" "/*"))
  "Match a label definition, with the label and colon in group 1.
Allow expressions in the subscript and a trailing comment.")

(defconst sp3--font-lock-defaults
  (let ((keywords '("else" "elsif" "end" "for" "function" "if" "in" "label"
                    "param" "print" "reg" "shader" "var" "return" "while" "until" "repeat"))
        (builtins '("assert" "assigned" "get_dword_offset" "get_reg_index"
                    "len" "raw_field" "rawf2i" "zeros")))
    `(((,(rx-to-string `(: symbol-start (or ,@keywords) symbol-end)) 0 font-lock-keyword-face)
       (,(rx-to-string `(: symbol-start (or ,@builtins) symbol-end)) 0 font-lock-builtin-face)
       (,(rx-to-string '(: bol (* blank)
                          (group "#" (* blank) "include" symbol-end)))
        1 font-lock-preprocessor-face)
       (,(rx-to-string '(: bol (* blank) "label" (+ blank)
                          (group (+ word)) symbol-end)) 1 font-lock-constant-face)
       (,(rx-to-string sp3--rx-label) 1 font-lock-constant-face)))))

(defvar sp3-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "." st)
    st))

(defun sp3--line-continued-p ()
  "Return non-nil if this code line ends with an unescaped backslash."
  (save-excursion
    (end-of-line)
    (when (eq (char-before) ?\\)
      (let ((state (syntax-ppss (1- (point)))))
        (not (or (nth 3 state) (nth 4 state) (nth 5 state)))))))

(defun sp3--beginning-of-logical-line ()
  "Move to the first physical line of the current continued statement."
  (beginning-of-line)
  (while (and (not (bobp))
              (save-excursion
                (forward-line -1)
                (sp3--line-continued-p)))
    (forward-line -1)))

(defun sp3--current-indentation ()
  "Return the indentation for the current line, accounting for continuations."
  (save-excursion
    (beginning-of-line)
    (let ((line-start (point)))
      (sp3--beginning-of-logical-line)
      (if (< (point) line-start)
          ;; Every continuation uses the statement's first line as its anchor.
          (+ (current-indentation) tab-width)
        (let ((done nil)
              (indent (current-indentation)))
          (save-excursion
            (while (not done)
              (if (/= (forward-line -1) 0)
                  (setq done t)
                ;; A completed continuation contributes the indentation and
                ;; block keyword of its first line, not its last fragment.
                (sp3--beginning-of-logical-line)
                (back-to-indentation)
                (pcase (thing-at-point 'symbol t)
                  ((or "function" "for" "if" "elsif" "else" "shader" "while" "repeat")
                   (setq indent (+ (current-indentation) tab-width)
                         done t))
                  ((pred (lambda (_)
                           (string-match-p (rx-to-string sp3--rx-label)
                                           (thing-at-point 'line t)))) nil)
                  (token
                   (when token
                     (setq indent (current-indentation)
                           done t)))))))
          (cond
           ((looking-at-p (rx bol (* blank) (or "else" "elsif" "end" "until") symbol-end))
            (setq indent (- indent tab-width)))
           ((looking-at-p (rx-to-string sp3--rx-label))
            (setq indent 0)))
          indent)))))

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
