;; add subdirectories to load-path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun add-sub-under-user-directory (path)
  (add-to-list 'load-path
	       (expand-file-name path user-emacs-directory)))

(defun add-executable-path (path)
  (if (and path
	   (not (member path exec-path)))
      (add-to-list 'exec-path path)))

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

(require 'projects)
(require 'hlsl-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme molokai-theme moe-theme markdown-mode magit lua-mode leuven-theme helm-ag glsl-mode ghc cmake-mode clang-format auctex atom-one-dark-theme atom-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
