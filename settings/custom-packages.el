;; set archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(defvar custom-packages-stack-initialized nil)

(defun custom-packages-setup-stack ()
  (if (and (not custom-packages-stack-initialized)
	   (executable-find "stack"))
      (let ((output (shell-command-to-string "stack path"))
	    (table (make-hash-table :test 'equal)))
	(setq custom-packages-stack-initialized t)
	(dolist (pair (split-string output "\n" t))
	  (let ((i (cl-search ": " pair)))
	    (if i
		(puthash (substring pair 0 i)
			 (substring pair (+ i 2))
			 table))))
	(add-executable-path (gethash "local-bin-path" table))
	(dolist (path (split-string (gethash "bin-path" table) ";" t))
	  (add-executable-path path)))))

;; libraries
(require 'package)
(require 'bind-key)
(require 'use-package)

(package-initialize)

(use-package leuven-theme
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :defer t
  :config
  (load-theme 'solarized-dark t))

(use-package atom-dark-theme
  :ensure t)

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
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook 'ghc-init)
  (add-hook 'haskell-mode-hook 'custom-packages-setup-stack))

(use-package markdown-mode
  :ensure t)

(provide 'custom-packages)
