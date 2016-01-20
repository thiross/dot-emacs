(require 'bind-key)
(require 'gdb-mi)

;;; find root
(defun projects-find-root ()
  (let ((root (shell-command-to-string
	       "git rev-parse --show-toplevel")))
    (if (string-prefix-p "fatal:" root)
	;; return nil
	(error root)
      ;; remove the trailing line feed
      (substring root 0 (1- (length root))))))

;;; compile project
(defun projects-compile (n)
  (interactive "p")
  (if (= n 4)
      (call-interactively #'compile)
    (let (make-command build-dir)
      ;; `ninja' has higher priority than `gnu-make'
      (if (executable-find "ninja")
	  (setq make-command "ninja")
	(setq make-command "make -j 4"))
      (setq build-dir (file-name-as-directory (projects-find-root)))
      (if (file-exists-p (concat build-dir "src/main/jni"))
	  ;; android project
	  (progn
	    (setq make-command "ndk-build NDK_DEBUG=1")
	    (setq build-dir (concat build-dir "src/main/jni")))
	(if (file-exists-p (concat build-dir "build"))
	    (setq build-dir (concat build-dir "build"))))
      (setq make-command (concat make-command
				 " -C "
				 build-dir))
      (compile make-command))))

(defun projects-find-gud ()
  (let ((gud nil))
    (dolist (buf (buffer-list) gud)
      (if (null gud)
	  (if (string-prefix-p "*gud"
			       (buffer-name buf))
	      (setq gud buf))))))

(defun projects-reset-gud ()
  (interactive)
  (let ((gud (projects-find-gud)))
    (if (null gud)
	(call-interactively #'gdb))
    (gdb-restore-windows)))

(bind-key "<f3>" 'projects-reset-gud)
(bind-key "<f4>" 'projects-compile)

(provide 'projects)
