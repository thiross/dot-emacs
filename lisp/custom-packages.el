;; set archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

;; libraries
(require 'package)
(require 'bind-key)
(require 'use-package)

(package-initialize)

(use-package leuven-theme
  :defer t
  :ensure t)

(use-package solarized-theme
  :defer t
  :init
  (setq solarized-use-more-italic t)
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package atom-dark-theme
  :defer t
  :ensure t)

(use-package monokai-theme
  :defer t
  :ensure t)

(use-package zenburn-theme
  :defer t
  :ensure t)

(use-package molokai-theme
  :defer t
  :ensure t)

(use-package atom-one-dark-theme
  :defer t
  :ensure t)

(use-package moe-theme
  ;; :defer t
  :ensure t
  :config
  (load-theme 'moe-dark t))

(use-package material-theme
  :defer t
  :ensure t
  :config
  (load-theme 'material t))

(use-package nord-theme
  :defer t
  :ensure t
  :config
  (load-theme 'nord t))

(use-package all-the-icons)

(use-package doom-themes
  :defer t
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-neotree-config))

;; (require 'doom-theme)
;; (load-theme 'doom-one t)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package magit
  :ensure t
  :config
  (bind-key "<f2>" 'magit-status))

(use-package clang-format
  :ensure t
  :config)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
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

(use-package auctex
  :ensure t
  :defer t)

(use-package lua-mode
  :ensure t
  :config
  (add-hook 'lua-mode-hook
	    (lambda ()
	      (electric-indent-mode -1))))

(use-package cmake-mode
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
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook
	    (lambda ()
	      (setq haskell-compile-cabal-build-command "stack build")))
  (bind-key "C-c C-f" 'haskell-cabal-visit-file haskell-mode-map)
  (bind-key "<f4>" 'haskell-compile haskell-cabal-mode-map)
  (bind-key "C-c C-c" 'haskell-compile haskell-cabal-mode-map)
  (bind-key "<f4>" 'haskell-compile haskell-mode-map)
  (bind-key "C-c C-q" 'haskell-mode-stylish-buffer haskell-mode-map))

(use-package intero
  :ensure t
  :config
  (require 'flycheck)
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
  (add-hook 'haskell-mode-hook 'intero-mode)
  (bind-key "C-c C-c" 'haskell-compile intero-mode-map))

(use-package markdown-mode
  :ensure t)

(provide 'custom-packages)
