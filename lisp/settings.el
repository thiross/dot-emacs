(require 'bind-key)
;;; language&font config
(defun settings-lang&font ()
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'gb18030)
  (modify-coding-system-alist 'process "ghci" 'gb18030)
  (let ((en-font "Consolas-10"))
    (cond
     ((eq system-type 'darwin)
      (setq en-font "Fira Code-14")))
    (set-frame-font en-font))
  (let ((spec (font-spec :family "Microsoft Yahei" :size 12)))
    (set-fontset-font t 'han spec)
    (set-fontset-font t 'symbol spec)
    (set-fontset-font t 'cjk-misc spec)
    (set-fontset-font t 'bopomofo spec)))

(defun settings-gui ()
  ;; disable menu bar
  (menu-bar-mode -1)
  ;; disable tool bar
  (tool-bar-mode -1)
  ;; disable scroll bar
  (scroll-bar-mode -1)
  ;; show paren mode
  (show-paren-mode 1)
  ;; no transient mark mode
  (transient-mark-mode -1)
  ;; no visible bell
  (setq visible-bell nil)
  ;; no startup message
  (setq inhibit-startup-message t)
  ;; recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; frame title format
  (setq frame-title-format
        '(buffer-file-name "%f"
                           (dired-directory dired-directory "%b"))))

;;; file config
(defun settings-file ()
  ;; do not backup files
  (setq make-backup-files nil)
  ;; do not auto save files
  (setq auto-save-default nil))

;;; open line function
(defun settings-open-line (n)
  (interactive "p")
  (if (= n 4)
      (beginning-of-line)
    (end-of-line))
  (open-line 1)
  (if (/= n 4)
      (forward-line 1))
  (funcall indent-line-function))

;; bind keys globally
(defun settings-bind-global-keys ()
  (bind-key "<f1>" 'ff-find-other-file)
  (bind-key "M-/" 'hippie-expand)
  (bind-key "S-SPC" 'set-mark-command)
  (bind-key "C-o" 'settings-open-line))

(defun settings-set-all ()
  (settings-lang&font)
  (settings-gui)
  (settings-file)
  (settings-bind-global-keys))

(provide 'settings)
