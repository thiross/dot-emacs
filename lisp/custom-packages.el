;; set archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

;; libraries
(require 'package)
(require 'use-package)

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

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package magit
  :ensure t
  :config
  :bind (("<f2>". magit-status)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ivy
  :ensure t
  :config
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
	 ("M-x" . counsel-M-x)))

(use-package swiper
  :bind (("C-s" . swiper)
	 ("<f10>" . counsel-ag))
  :config
  (setq counsel-ag-base-command "ag --vimgrep --nocolor --nogroup %s"))

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
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-signature-auto-activate nil
	lsp-signature-render-documentation nil)
  :hook
  ((rust-mode . lsp)
   (haskell-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil)
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-display-errors-function nil))

(use-package company
  :ensure t
  :init
  (global-company-mode))

(use-package rust-mode
  :ensure t)

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

(use-package dhall-mode
  :ensure t
  :config
  (setq
   dhall-format-arguments (\` ("--ascii"))
   dhall-use-header-line nil))

(use-package auctex
  :defer t
  :ensure t)

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
