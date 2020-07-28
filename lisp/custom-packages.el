;; set archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

;; libraries
(require 'package)
(require 'bind-key)
(require 'use-package)

(package-initialize)

(use-package solarized-theme
  :init
  (setq solarized-use-less-bold t)
  (setq solarized-use-more-italic t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-height-minus-1 1)
  (setq solarized-height-plus-1 1)
  (setq solarized-height-plus-2 1)
  (setq solarized-height-plus-3 1)
  (setq solarized-height-plus-4 1)
  :ensure t
  :config
  (load-theme 'solarized-dark t))

;; (use-package atom-dark-theme
;;   :ensure t)

;; (use-package monokai-theme
;;   :ensure t)

;; (use-package molokai-theme
;;   :ensure t)

;; (use-package atom-one-dark-theme
;;   :ensure t)

;; (use-package moe-theme
;;   :ensure t
;;   :config
;;   (load-theme 'moe-dark t))

;; (use-package material-theme
;;   :ensure t
;;   :config
;;   (load-theme 'material t))

;; (use-package nord-theme
;;   :ensure t
;;   :config
;;   (load-theme 'nord t))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t
;; 	doom-themes-enable-italic t)
;;   (load-theme 'doom-one t))

;; (use-package night-owl-theme
;;   :ensure t
;;   :config
;;   (setq night-owl-height-minus-1 1)
;;   (setq night-owl-height-plus-1 1)
;;   (setq night-owl-height-plus-2 1)
;;   (setq night-owl-height-plus-3 1)
;;   (setq night-owl-height-plus-4 1)  
;;   (load-theme 'night-owl t))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package magit
  :ensure t
  :config
  (bind-key "<f2>" 'magit-status))

(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 :map helm-map
	 ("<tab>" . helm-execute-persistent-action)))

(use-package helm-ag
  :ensure t
  :bind (("<f10>" . helm-ag)))

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
  :config
  (bind-key "C-c C-f" 'haskell-cabal-visit-file haskell-mode-map)
  (bind-key "<f4>" 'haskell-compile haskell-cabal-mode-map)
  (bind-key "C-c C-c" 'haskell-compile haskell-cabal-mode-map)
  (bind-key "<f4>" 'haskell-compile haskell-mode-map)
  (defun brittany()
    (interactive)
    (haskell-mode-buffer-apply-command "brittany"))
  (bind-key "M-q" (lambda ()
			(interactive)
			(haskell-mode-stylish-buffer)
			(haskell-mode-buffer-apply-command "brittany"))
	    haskell-mode-map))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (python-mode . lsp)
  :config
  (require 'lsp-clients)
  (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package flycheck
  :ensure t)

(use-package company
  :ensure t)

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package auctex
  :defer t
  :ensure t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(provide 'custom-packages)
