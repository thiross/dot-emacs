;;; -*- lexical-binding: t; -*-
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(require 'package)

(require 'use-package)

(use-package emacs
  :init
  (let* ((font-name "Maple Mono")
	 (en (cond ((eq system-type 'darwin)
		    (font-spec :family font-name
			       :size 15
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
  :bind
  (("S-SPC" . set-mark-command)
   ("<home>" . move-beginning-of-line)
   ("<end>" . move-end-of-line)))

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
  :hook ((rustic-mode . rainbow-delimiters-mode)))

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

(use-package multiple-cursors
  :ensure t
  :diminish
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<". mc/mark-all-like-this))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

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

(use-package cc-mode
  :hook (((c++-mode c-mode) .
	  (lambda () (setq indent-tabs-mode nil c-basic-offset 4)))))

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

(use-package wgsl-mode
  :ensure t
  :hook
  ((wgsl-mode
    . (lambda ()
	(c-set-offset 'arglist-intro '+)
	(c-set-offset 'arglist-close 0)))))

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
	 (rust-mode . git-gutter-mode)
	 (sp3-mode . git-gutter-mode))
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

(use-package eglot
  :ensure t
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-autoshutdown t)
  :bind
  (:map eglot-mode-map
	("C-c a a" . eglot-code-actions)
	("C-c a o" . eglot-code-action-organize-imports)
	("C-c a r" . eglot-rename)
	("C-c h"   . eldoc))
  :hook
  (((rust-mode haskell-mode)
    . eglot-ensure)))

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
  :ensure t
  :hook
  ((prog-mode . yas-minor-mode)))

(use-package dabbrev
  :ensure t
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package rustic
  :ensure t
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-rustfmt-bin "rustfmt")
  (rustic-rustfmt-args "+nightly")
  :bind (("C-c C-f" . rustic-format-buffer))
  :hook (rust-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package dape
  :ensure t
  :commands dape

  :init
  (defvar my/dape--rust-example-cache (make-hash-table :test 'equal)
    "ROOT -> (TARGET-DIR . ((NAME . SRC-PATH) ...))")
  (defvar my/dape--rust-test-cache (make-hash-table :test 'equal)
    "ROOT::FEATURES -> ((NAME . EXE) ...)")

  (defcustom my/dape-rust-test-features ""
    "Fallback features for cargo test --no-run when eglot config is unset.
Comma separated."
    :type 'string
    :group 'my)

  (defun my/dape--lldb-dap-path ()
    "Return path to lldb-dap executable, or nil."
    (or (executable-find "lldb-dap")
        (and (executable-find "xcrun")
             (string-trim (shell-command-to-string "xcrun -f lldb-dap")))))

  (defun my/dape--cargo ()
    (or (executable-find "cargo")
        (user-error "cargo not found")))

  (defun my/dape--rust-root ()
    (or (locate-dominating-file default-directory "Cargo.toml")
        (user-error "Not in a Cargo project")))

  (defun my/dape--rust-eglot-features ()
    "Read cargo features from buffer-local `eglot-workspace-configuration'.
Returns a comma-separated string, or nil if unset."
    (when (bound-and-true-p eglot-workspace-configuration)
      (let* ((cfg eglot-workspace-configuration)
             (ra (or (plist-get cfg :rust-analyzer)
                     (cdr (assq 'rust-analyzer cfg))))
             (cargo (and ra (or (plist-get ra :cargo)
                                (cdr (assq 'cargo ra)))))
             (features (and cargo (or (plist-get cargo :features)
                                      (cdr (assq 'features cargo))))))
        (cond
         ((vectorp features)
          (mapconcat #'identity features ","))
         ((and (listp features) features)
          (mapconcat #'identity features ","))
         ((stringp features) features)
         (t nil)))))

  (defun my/dape--rust-examples (root)
    (or (gethash root my/dape--rust-example-cache)
        (puthash root
                 (let* ((default-directory root)
                        (json (shell-command-to-string
                               (concat (shell-quote-argument (my/dape--cargo))
                                       " metadata --no-deps --format-version 1")))
                        (data (json-parse-string json
                                                 :object-type 'alist
                                                 :array-type 'list
                                                 :null-object nil
                                                 :false-object nil))
                        (packages (alist-get 'packages data))
                        (target-dir (alist-get 'target_directory data))
                        (examples
                         (cl-loop for pkg in packages
                                  append (cl-loop for ex in (alist-get 'targets pkg)
                                                  when (member "example" (alist-get 'kind ex))
                                                  collect (cons (alist-get 'name ex)
                                                                (alist-get 'src_path ex))))))
                   (cons target-dir examples))
                 my/dape--rust-example-cache)))

  (defun my/dape--rust-tests (root &optional features)
    (let* ((features (or features my/dape-rust-test-features))
           (key (concat (file-truename root) "::" features)))
      (or (gethash key my/dape--rust-test-cache)
          (puthash key
                   (let* ((default-directory root)
                          (feat-arg (if (string-empty-p features)
                                        ""
                                      (concat " --features "
                                              (shell-quote-argument features))))
                          (json (shell-command-to-string
                                 (concat (shell-quote-argument (my/dape--cargo))
                                         " test --no-run --message-format=json"
                                         feat-arg
                                         " 2>/dev/null")))
                          (result nil))
                     (dolist (line (split-string json "\n" t))
                       (when (string-prefix-p "{" line)
                         (condition-case nil
                             (let* ((obj (json-parse-string line
                                                            :object-type 'alist
                                                            :array-type 'list
                                                            :null-object nil
                                                            :false-object nil))
                                    (exe (alist-get 'executable obj))
                                    (target (alist-get 'target obj))
                                    (kinds (alist-get 'kind target)))
                               (when (and exe
                                          (or (member "test" kinds)
                                              (alist-get 'test target)))
                                 (push (cons (alist-get 'name target) exe) result)))
                           (error nil))))
                     (nreverse result))
                   my/dape--rust-test-cache))))

  (defun my/dape--rust-test-names (exe)
    "Return list of test names in EXE."
    (let ((output (shell-command-to-string
                   (concat (shell-quote-argument exe)
                           " --list --format terse 2>/dev/null"))))
      (cl-loop for line in (split-string output "\n" t)
               when (string-match ": test$" line)
               collect (string-trim (substring line 0 (match-beginning 0))))))

  (defun my/dape-rust-clear-cache ()
    "Clear cached example/test targets.
Call after adding targets or switching features."
    (interactive)
    (clrhash my/dape--rust-example-cache)
    (clrhash my/dape--rust-test-cache)
    (message "dape rust cache cleared"))

  (defun my/dape-rust-example-fn (config)
    "Prompt for an example and set :program / :cwd / command-cwd."
    (let* ((root (file-truename (my/dape--rust-root)))
           (info (my/dape--rust-examples root))
           (target-dir (file-truename (car info)))
           (examples (cdr info))
           (chosen (completing-read "Example: " (mapcar #'car examples) nil t))
           (prog (expand-file-name (concat "debug/examples/" chosen) target-dir))
           (features (my/dape--rust-eglot-features)))
      (unless (file-exists-p prog)
        (user-error "Run: cargo build --example %s%s"
                    chosen
                    (if features (concat " --features " features) "")))
      (setq config (plist-put config :cwd root))
      (setq config (plist-put config 'command-cwd target-dir))
      (setq config (plist-put config :program prog))
      config))

  (defun my/dape-rust-test-fn (config)
    "Prompt for features, test binary, and test names."
    (let* ((root (file-truename (my/dape--rust-root)))
           (default-features (or (my/dape--rust-eglot-features)
                                 my/dape-rust-test-features))
           (input (read-string (format "Features (default: \"%s\"): " default-features)
                               nil nil default-features))
           (features (if (string-empty-p input) default-features input))
           (tests (my/dape--rust-tests root features)))
      (unless tests
        (user-error "No test binaries found with features: \"%s\"" features))
      (let* ((chosen (completing-read "Test binary: " (mapcar #'car tests) nil t))
             (exe (cdr (assoc chosen tests)))
             (names (my/dape--rust-test-names exe))
             (selected (completing-read-multiple
                        "Test names (empty = all, comma separated): "
                        names nil nil)))
        (setq config (plist-put config :cwd root))
        (setq config (plist-put config 'command-cwd root))
        (setq config (plist-put config :program exe))
        (if (null selected)
            config
          (plist-put config :args
                     (append selected
                             (if (= 1 (length selected)) '("--exact") nil)
                             '("--nocapture")))))))

  :config
  (add-to-list 'dape-configs
               `(lldb-dap-attach
                 modes (prog-mode)
                 command ,(my/dape--lldb-dap-path)
                 fn (lambda (cfg)
                      (plist-put cfg :pid (read-number "PID to attach: ")))
                 :type "lldb-dap"
                 :request "attach"))
  (add-to-list 'dape-configs
               `(rust-example
                 modes (rustic-mode)
                 command ,(my/dape--lldb-dap-path)
                 fn my/dape-rust-example-fn
                 :type "lldb-dap"
                 :request "launch"
                 :name "rust-example"
                 :cwd "."
                 :console "integratedTerminal"
                 :args []))
  (add-to-list 'dape-configs
               `(rust-test
                 modes (rustic-mode)
                 command ,(my/dape--lldb-dap-path)
                 fn my/dape-rust-test-fn
                 :type "lldb-dap"
                 :request "launch"
                 :name "rust-test"
                 :cwd "."
                 :console "integratedTerminal"
                 :args [])))

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
  (org-latex-compiler "xelatex")
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
