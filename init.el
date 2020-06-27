;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; add subdirectories to load-path
(defun add-sub-under-user-directory (path)
  (add-to-list 'load-path
	       (expand-file-name path user-emacs-directory)))

(defun add-executable-path (path)
  (if (and path
	   (not (member path exec-path)))
      (add-to-list 'exec-path path)))

(defun export-exec-path-to-env ()
  (let ((sp (cond ((eq system-type 'ms-dos) ";")
		  ((eq system-type 'windows-nt) ";")
		  (t ":"))))
    (setenv "PATH"
	    (mapconcat 'identity exec-path sp))))

(if (eq system-type 'darwin)
    (progn (add-executable-path "~/.local/bin")
	   (add-executable-path "~/.cargo/bin")
	   (add-executable-path "/usr/local/bin")
	   (export-exec-path-to-env)))

(add-sub-under-user-directory "lisp")
(add-sub-under-user-directory "lisp/use-package")

(let ((user-config-file (expand-file-name "user.el"
					  user-emacs-directory)))
  (if (file-exists-p user-config-file)
      (load-file user-config-file)))

(require 'settings)
(settings-set-all)

(require 'mode-hooks)
(mode-hooks-install-hooks)

(require 'custom-packages)
