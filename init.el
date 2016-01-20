;; add subdirectories to load-path
(defun add-sub-under-user-directory (path)
  (add-to-list 'load-path
	       (expand-file-name path user-emacs-directory)))

(defun add-executable-path (path)
  (if (and path
	   (not (member path exec-path)))
      (add-to-list 'exec-path path)))

(add-sub-under-user-directory "lisp")
(add-sub-under-user-directory "lisp/use-package")

(require 'settings)
(settings-set-all)

(require 'mode-hooks)
(mode-hooks-install-hooks)

(require 'custom-packages)

(require 'projects)
(require 'hlsl-mode)


