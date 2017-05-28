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
  :defer t
  :ensure t
  :config
  (load-theme 'moe-dark t))

(require 'doom-theme)
(load-theme 'doom-one t)

(use-package magit
  :ensure t
  :config
  (bind-key "<f2>" 'magit-status))

(use-package clang-format
  :ensure t
  :config)

(defun use-helm ()
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
    (bind-key "C-x C-f" 'helm-find-files)
    (helm-mode 1))
  
  (if (executable-find "ag")
      (use-package helm-ag
	:ensure t)))

(defun use-ivy ()
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
    (bind-key "<f10>" 'counsel-ag)))

(if nil
    (use-helm)
  (use-ivy))

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
  (bind-key "<f1>" 'haskell-cabal-visit-file)
  (bind-key "<f4>" 'haskell-compile haskell-cabal-mode-map)
  (bind-key "<f4>" 'haskell-compile haskell-mode-map)
  (bind-key "<f5>" 'haskell-mode-stylish-buffer haskell-mode-map))

(use-package intero
  :ensure t
  :config
  (require 'flycheck)
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package markdown-mode
  :ensure t)

(provide 'custom-packages)
