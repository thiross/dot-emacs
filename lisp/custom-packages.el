;; set archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(defun custom-packages-setup-stack ()
  (if (executable-find "stack")
      (let ((output (shell-command-to-string "stack path"))
	    (table (make-hash-table :test 'equal)))
	(advice-remove 'ghc-init #'custom-packages-setup-stack)
	(dolist (pair (split-string output "\n" t))
	  (let ((i (cl-search ": " pair)))
	    (if i
		(puthash (substring pair 0 i)
			 (substring pair (+ i 2))
			 table))))
	(add-executable-path (gethash "local-bin-path" table))
	(dolist (path (split-string (gethash "bin-path" table) ";" t))
	  (add-executable-path path))
	(setenv "PATH"
		(mapconcat 'identity exec-path ";")))))

;; libraries
(require 'package)
(require 'bind-key)
(require 'use-package)

(package-initialize)

(use-package leuven-theme
  :defer t
  :ensure t)

(use-package solarized-theme
  :init
  (setq solarized-use-more-italic t)
  :ensure t
  :defer t
  :config
  (load-theme 'solarized-dark t))

(use-package atom-dark-theme
  :defer t
  :ensure t)

(use-package monokai-theme
  :defer t
  :ensure t)

(use-package molokai-theme
  ;; :defer t
  :ensure t)

(use-package atom-one-dark-theme
  :defer t
  :ensure t)

(use-package magit
  :ensure t
  :config
  (bind-key "<f2>" 'magit-status))

(use-package clang-format
  :ensure t
  :config
  (bind-key "<f5>" 'clang-format-buffer))

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
  (advice-add 'ghc-init :before #'custom-packages-setup-stack)
  (add-hook 'haskell-mode-hook 'ghc-init))

(use-package markdown-mode
  :ensure t)

(provide 'custom-packages)
