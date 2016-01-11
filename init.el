;; add subdirectories to load-path
(mapc (lambda (path)
	(add-to-list 'load-path
		     (expand-file-name path user-emacs-directory)))
      '("lisp/use-package"))

;; set archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

;; libraries
(require 'package)
(require 'bind-key)
(require 'use-package)

;;; language&font config
(defun zale-config-lang&font ()
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'gb18030)
  (modify-coding-system-alist 'process "ghci" 'gb18030)
  (let ((en-font "Source Code Pro-10.5"))
    (cond
     ((eq system-type 'darwin)
      (setq en-font "Monaco-11")))
    (set-frame-font en-font))
  (let ((spec (font-spec :family "Microsoft Yahei" :size 12)))
    (set-fontset-font (frame-parameter nil 'font) 'han spec)
    (set-fontset-font (frame-parameter nil 'font) 'symbol spec)
    (set-fontset-font (frame-parameter nil 'font) 'cjk-misc spec)
    (set-fontset-font (frame-parameter nil 'font) 'bopomofo spec)))

(defun zale-config-gui ()
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
  ;; frame title format
  (setq frame-title-format
        '(buffer-file-name "%f"
                           (dired-directory dired-directory "%b"))))

;;; file config
(defun zale-config-file ()
  ;; do not backup files
  (setq make-backup-files nil)
  ;; do not auto save files
  (setq auto-save-default nil))

;;; open line function
(defun zale-open-line (n)
  (interactive "p")
  (if (= n 4)
      (beginning-of-line)
    (end-of-line))
  (open-line 1)
  (if (/= n 4)
      (forward-line 1))
  (funcall indent-line-function))

;; bind keys globally
(defun zale-bind-global-keys ()
  (bind-key "M-/" 'hippie-expand)
  (bind-key "S-SPC" 'set-mark-command)
  (bind-key "C-o" 'zale-open-line))

(defun zale-add-path (path)
  (if (and path
	   (not (member path exec-path)))
      (add-to-list 'exec-path path)))

(defun zale-setup-stack ()
  (if (executable-find "stack")
      (let ((output (shell-command-to-string "stack path"))
	    (table (make-hash-table :test 'equal)))
	(dolist (pair (split-string output "\n" t))
	  (let ((i (cl-search ": " pair)))
	    (if i
		(puthash (substring pair 0 i)
			 (substring pair (+ i 2))
			 table))))
	(zale-add-path (gethash "local-bin-path" table))
	(dolist (path (split-string (gethash "bin-path" table) ";" t))
	  (zale-add-path path)))))

(package-initialize)

(use-package leuven-theme
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package monokai-theme
  :ensure t
  :defer t)

(use-package molokai-theme
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :config
  (bind-key "<f2>" 'magit-status))

(use-package helm
  :ensure t
  :config
  (bind-key "M-x" 'helm-M-x)
  (bind-key "<tab>"
	    'helm-execute-persistent-action helm-map)
  (bind-key "C-i"
	    'helm-execute-persistent-action helm-map)
  (bind-key "C-z"
	    'helm-select-action)
  (helm-mode 1))

(if (executable-find "ag")
    (use-package helm-ag
      :ensure t))

(use-package auctex
  :ensure t
  :defer t)

(use-package lua-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package ghc
  :ensure t
  :config
  (zale-setup-stack)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package markdown-mode
  :ensure t)

(zale-config-lang&font)
(zale-config-gui)
(zale-config-file)
(zale-bind-global-keys)
