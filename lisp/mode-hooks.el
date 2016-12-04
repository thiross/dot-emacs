;;; define mode options
(defvar mode-hooks-defined-modes
  '((c-mode-hook (:offset . 8)
		 (c-set-offset 'case-label '+)
		 (bind-key "<f5>" 'clang-format-buffer c-mode-base-map))
    (c++-mode-hook (:offset . 4)
		   (:expand-tab t)
		   ;; (c-set-offset 'substatement-open 0)
		   (c-set-offset 'case-label '+)
		   (c-set-offset 'innamespace 0)
		   (c-macro-cppflags . "--std=c++0x -x c++")
		   (bind-key "<f5>" 'clang-format-buffer c-mode-base-map)
		   (lambda ()
		     (if (fboundp 'gtags-mode)
			 (funcall 'gtags-mode 1))))
    (java-mode-hook (:offset . 4)
		    (:tab-width . 4))
    (cperl-mode-hook (cperl-indent-level . 4))
    (html-mode-hook (:expand-tab t))
    (lua-mode-hook (:expand-tab t))
    ))

;;; tab width
(defun mode-hooks-set-tab-width (width)
  (setq tab-width width)
  (setq tab-stop-list '())
  (dotimes (i 14)
    (setq tab-stop-list
	  (append tab-stop-list
		  (list (* (1+ i) width))))))

;;; find options
(defun mode-hooks-find-options (mode)
  (let ((selected nil))
    (catch 'return
      (dolist (option mode-hooks-defined-modes)
	(if (not (string=
		  (symbol-name (car option))
		  (concat (symbol-name mode) "-hook")))
	    nil
	  (setq selected option)
	  (throw 'return t))))
    selected))
  
;;; change options
(defun mode-hooks-change-options (mode)
  (let ((options (mode-hooks-find-options mode)) key value)
    (dolist (option (cdr options))
      (if (not (listp option))
	  (setq key option)
	(setq key (car option))
	(setq value (cdr option)))
      (cond ((eq :offset key)
	     (setq c-basic-offset value))
	    ((eq :expand-tab key)
	     (setq indent-tabs-mode (not value)))
	    ((eq :tab-width key)
	     (mode-hooks-set-tab-width value))
	    ((fboundp key)
	     (eval `(,key ,@value)))
	    (t
	     (set key value))
	    ))))

;;; default-mode-hook
(defun mode-hooks-default-hook ()
  (mode-hooks-change-options major-mode))

;;; install hook
(defun mode-hooks-install-hook (options)
  (let ((mode (car options)))
    (add-hook mode 'mode-hooks-default-hook)))

;;; install hooks
(defun mode-hooks-install-hooks ()
  (mapc 'mode-hooks-install-hook mode-hooks-defined-modes))

(provide 'mode-hooks)
