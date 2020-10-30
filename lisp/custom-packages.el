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
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
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
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
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

;; (use-package solarized-theme
;;   :ensure t
;;   :init
;;   (setq solarized-use-less-bold t)
;;   (setq solarized-use-more-italic t)
;;   (setq solarized-high-contrast-mode-line t)
;;   (setq solarized-use-variable-pitch nil)
;;   (setq solarized-scale-org-headlines nil)
;;   (setq solarized-height-minus-1 1)
;;   (setq solarized-height-plus-1 1)
;;   (setq solarized-height-plus-2 1)
;;   (setq solarized-height-plus-3 1)
;;   (setq solarized-height-plus-4 1)
;;   :config
;;   (load-theme 'solarized-dark t))

(use-package night-owl-theme
  :ensure t
  :config
  (setq night-owl-height-minus-1 1)
  (setq night-owl-height-plus-1 1)
  (setq night-owl-height-plus-2 1)
  (setq night-owl-height-plus-3 1)
  (setq night-owl-height-plus-4 1)  
  (load-theme 'night-owl t))

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

(use-package cmake-mode
  :ensure t)

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
	      ("<f4>" . haskell-compile)
	      ("C-c C-f" . haskell-cabal-visit-file)
	      ("M-q" . (lambda ()
			 (interactive)
			 (haskell-mode-stylish-buffer)
			 (haskell-mode-buffer-apply-command "brittany")))
	      :map haskell-cabal-mode-map
	      ("<f4>" . haskell-compile)
	      ("C-c C-c" . haskell-compile))
  :config
  (defun brittany()
    (interactive)
    (haskell-mode-buffer-apply-command "brittany")))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((python-mode . lsp)
	 (rust-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-enable-snippet nil)
  (setq lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil))

(use-package lsp-ivy
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package company
  :ensure t)

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

(use-package auctex
  :defer t
  :ensure t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(provide 'custom-packages)
