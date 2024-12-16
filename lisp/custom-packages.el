(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(require 'package)

(require 'use-package)

(use-package emacs
  :init
  (let* ((font-name "0xProto")
	 (en (cond ((eq system-type 'darwin)
		    (font-spec :family font-name
			       :size 14
			       :weight 'normal))
		   ((eq system-type 'gnu/linux)
		    (font-spec :family font-name
			       :size 18.0
			       :weight 'normal))
		   (t
		    (font-spec :family font-name
			     :size 9.0)))))
       (set-frame-font en))
  (let* ((zh (cond ((eq system-type 'darwin)
		    (font-spec :family "手札体-简"
			       :size 15.0))
		   ((eq system-type 'gnu/linux)
		    (font-spec :family "微软雅黑"
			       :size 16.0))
		   (t
		    (font-spec :family "微软雅黑"
			       :size 8.0)))))
    (set-fontset-font t 'han zh)
    (set-fontset-font t 'symbol zh)
    (set-fontset-font t 'cjk-misc zh)
    (set-fontset-font t 'bopomofo zh))
  (setq-default line-spacing 0.1)
  (prefer-coding-system 'utf-8)
  (modify-coding-system-alist 'process "ghci" 'utf-8)
  (setq file-name-coding-system 'utf-8)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1)
  (transient-mark-mode -1)
  (setq visible-bell nil)
  (setq inhibit-startup-message t)
  (setq enable-recursive-minibuffers t)
  (if (display-graphic-p)
      (progn
	(setq initial-frame-alist
	      '((width . 80)
		(height . 30)))
	(setq default-frame-alist
	      '((width . 80)
		(height . 30)))
	))
  (setq frame-title-format
        '(buffer-file-name "%f"
                           (dired-directory dired-directory "%b")))
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (if (eq system-type 'darwin)
      (setq default-directory "~/"))
  (setq read-process-output-max (* 1024 1024)
	gc-cons-percentage 0.5
	gc-cons-threshold (* 1024 1024 500))
  :diminish eldoc-mode
  :bind (("S-SPC" . set-mark-command)))

(use-package composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda ()
	(setq-local composition-function-table
		    composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when (version<= "27.0" emacs-version)
    (let ((alist
	   '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
	     (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
	     (36 . ".\\(?:\\(>\\)>?\\)")
	     (37 . ".\\(?:\\(%\\)%?\\)")
	     (38 . ".\\(?:\\(&\\)&?\\)")
	     (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
	     ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
	     (43 . ".\\(?:\\([>]\\)>?\\)")
	     ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
	     (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
	     ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
	     (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
	     (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
	     ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
	     (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
	     ;; (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
	     (59 . ".\\(?:\\(;\\);?\\)")
	     (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
	     (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
	     (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
	     (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
	     (91 . ".\\(?:\\(|\\)[]|]?\\)")
	     ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
	     (94 . ".\\(?:\\(=\\)=?\\)")
	     (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
	     (119 . ".\\(?:\\(ww\\)w?\\)")
	     (123 . ".\\(?:\\(|\\)[|}]?\\)")
	     (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
	     (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
	(set-char-table-range composition-ligature-table (car char-regexp)
			      `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((rust-mode . rainbow-delimiters-mode)))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package crux
  :ensure t
  :bind (("C-o" . crux-smart-open-line)))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package time
  :custom
  (display-time-interval 60)
  (display-time-mode t)
  (display-time-use-mail-icon t))

(use-package diminish
  :ensure t)

(use-package align
  :commands align
  :bind (("M-[" . align-code)
	 ("C-c [" . align-regexp))
  :custom
  (align-to-tab-stop nil)
  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
	(align beg end)
      (let ((end-mark (copy-marker end))
	    (indent-region beg end-mark nil)
	    (align beg end-mark))))))

(use-package magit
  :ensure t
  :config
  :bind (("<f2>". magit-status)))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("M-y" . consult-yank-pop)))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)))

(use-package auctex
  :defer t
  :ensure t)

(use-package lua-mode
  :ensure t
  :config
  :hook (lua-mode . (lambda ()
		      (electric-indent-mode -1))))

(if (not (eq system-type 'gnu/linux))
    (use-package cmake-mode
      :ensure t))

(use-package dockerfile-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  :hook (haskell-mode
	 . (lambda ()
	     (haskell-indentation-mode)
	     (setq haskell-compile-cabal-build-command "stack build")))
  :bind (:map haskell-mode-map
	      ("C-c C-c C-b" . haskell-compile)
	      ("C-c C-f" . ormolu-format-buffer)
	      :map haskell-cabal-mode-map
	      ("C-c C-c C-b" . haskell-compile)))

(use-package ormolu
  :ensure t)

(use-package project
  :ensure t)

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :hook ((haskell-mode . git-gutter-mode)
	 (rust-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap
    'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap
    'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap
    'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  (lsp-inlay-hint-enable t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-ui-sideline-enable nil)
  :commands lsp
  :bind
  (("C-c a a" . lsp-execute-code-action)
   ("C-c a r" . lsp-rename)
   ("C-c a o" . lsp-organize-imports)
   ("C-c d d" . lsp-describe-thing-at-point)
   ("C-c g d" . xref-find-definitions)
   ("C-c g r" . xref-find-references))
  :hook
  ((rust-mode . lsp)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind
  (("C-c g o" . lsp-ui-imenu)))

(use-package corfu
  :ensure t
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.25)
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  :bind (("C-c . p" . completion-at-point)
	 ("C-c . t" . complete-tag)
	 ("C-c . d" . cape-dabbrev)
	 ("C-c . h" . cape-history)
	 ("C-c . f" . cape-file)
         ("C-c . k" . cape-keyword)
         ("C-c . s" . cape-elisp-symbol)
         ("C-c . e" . cape-elisp-block)
         ("C-c . a" . cape-abbrev)
         ("C-c . l" . cape-line)
         ("C-c . w" . cape-dict)
         ("C-c . :" . cape-emoji)
         ("C-c . \\" . cape-tex)
         ("C-c . _" . cape-tex)
         ("C-c . ^" . cape-tex)
         ("C-c . &" . cape-sgml)
         ("C-c . r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file styles partial-completion))))

(use-package yasnippet
  :ensure t)

(use-package dabbrev
  :ensure t
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-display-errors-function nil))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package elm-mode
  :ensure t
  :config)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package dhall-mode
  :ensure t
  :config
  (setq
   dhall-format-arguments (\` ("--ascii"))
   dhall-use-header-line nil))

(use-package org
  :custom
  (org-export-backends '(ascii html icalendar latex beamer md)))

(use-package org-babel
  :no-require t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (latex . t)
     (python . t))))

(use-package org-roam
  :defer t
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n t" . org-roam-buffer-toggle))
  :config
  (setq org-roam-directory "~/notes/")
  (org-roam-setup))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(provide 'custom-packages)
