;; set archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

;; libraries
(require 'package)
(require 'bind-key)
(require 'use-package)

(package-initialize)

;; (use-package solarized-theme
;;   :init
;;   (setq solarized-use-more-italic nil)
;;   (setq solarized-high-contrast-mode-line t)
;;   (setq solarized-use-variable-pitch nil)
;;   (setq solarized-scale-org-headlines nil)
;;   (setq solarized-height-minus-1 1)
;;   (setq solarized-height-plus-1 1)
;;   (setq solarized-height-plus-2 1)
;;   (setq solarized-height-plus-3 1)
;;   (setq solarized-height-plus-4 1)
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t))

;; (use-package atom-dark-theme
;;   :ensure t)

;; (use-package monokai-theme
;;   :ensure t)

;; (use-package zenburn-theme
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

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-neotree-config))

;; (use-package night-owl-theme
;;   :ensure t
;;   :config
;;   (load-theme 'night-owl t))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package magit
  :ensure t
  :config
  (bind-key "<f2>" 'magit-status))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :ensure t
  :config
  (bind-key "C-x C-f" 'counsel-find-file))

(use-package swiper
  :ensure t
  :config
  (bind-key "C-s" 'swiper)
  (setq counsel-ag-base-command "ag --vimgrep --nocolor --nogroup %s")
  (bind-key "<f10>" 'counsel-ag))

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

(use-package neotree
  :ensure t
  :config
  (bind-key "<f9>" 'neotree-toggle))

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
  (bind-key "C-c C-q" (lambda ()
			(interactive)
			(haskell-mode-stylish-buffer)
			(haskell-mode-buffer-apply-command "brittany"))
	    haskell-mode-map))

(use-package intero
  :ensure t
  :hook ((haskell-mode . intero-mode))
  :config
  (require 'flycheck)
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
  ;; (add-hook 'haskell-mode-hook 'intero-mode)
  (bind-key "C-c C-c" 'haskell-compile intero-mode-map))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config (require 'lsp-clients))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t)

(use-package rust-mode
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
