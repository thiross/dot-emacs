;; -*- coding: utf-8 -*-
(require 'bind-key)
;;; language&font config
(defun settings-lang&font ()
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8)
  (modify-coding-system-alist 'process "ghci" 'utf-8)
  (let* ((font-name "Input")
	(en-font
	 (cond
	  ((eq system-type 'darwin)
	   (font-spec :family font-name
		      :size 13
		      :weight 'semi-bold))
	  ((eq system-type 'gnu/linux)
	   (font-spec :family font-name
		      :size 20.0
		      :weight 'semi-bold))
	  (t
	   (font-spec :family font-name
		      :size 9.0)))))
    (set-frame-font en-font))
  (let ((zh-font
	 (cond
	  ((eq system-type 'darwin)
	   (font-spec :family "手札体-简"
		      :size 16.0))
	  (t
	   (font-spec :family "微软雅黑"
		      :size 12.0))
	  )
	 ))
    (set-fontset-font t 'han zh-font)
    (set-fontset-font t 'symbol zh-font)
    (set-fontset-font t 'cjk-misc zh-font)
    (set-fontset-font t 'bopomofo zh-font))
  (setq-default line-spacing 0.2))

(defun settings-gui ()
  ;; disable menu bar
  (menu-bar-mode -1)
  ;; disable tool bar
  (tool-bar-mode -1)
  ;; disable scroll bar
  (scroll-bar-mode -1)
  ;; show paren mode
  (show-paren-mode 1)
  ;; no transient mark mode
  (transient-mark-mode -1)
  ;; no visible bell
  (setq visible-bell nil)
  ;; no startup message
  (setq inhibit-startup-message t)
  ;; recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; set frame size.
  (if (display-graphic-p)
      (progn
	(setq initial-frame-alist
	      '((width . 80)
		(height . 30)))
	(setq default-frame-alist
	      '((width . 80)
		(height . 30)))
	))
  ;; frame title format
  (setq frame-title-format
        '(buffer-file-name "%f"
                           (dired-directory dired-directory "%b"))))

;;; file config
(defun settings-file ()
  ;; do not backup files
  (setq make-backup-files nil)
  ;; do not auto save files
  (setq auto-save-default nil)
  ;; set default directory
  (if (eq system-type 'darwin)
      (setq default-directory "~/")))

;;; open line function
(defun settings-open-line (n)
  (interactive "p")
  (if (= n 4)
      (beginning-of-line)
    (end-of-line))
  (open-line 1)
  (if (/= n 4)
      (forward-line 1))
  (funcall indent-line-function))

;; bind keys globally
(defun settings-bind-global-keys ()
  (bind-key "<f1>" 'ff-find-other-file)
  (bind-key "M-/" 'hippie-expand)
  (bind-key "S-SPC" 'set-mark-command)
  (bind-key "C-o" 'settings-open-line))

(defun settings-set-all ()
  (settings-lang&font)
  (settings-gui)
  (settings-file)
  (settings-bind-global-keys)
  (setq ispell-program-name "aspell")
  (setq gc-cons-threshold (* 1024 1024 200))
  (setq read-process-output-max (* 1024 1024)))

(provide 'settings)
