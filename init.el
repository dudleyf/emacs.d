;; Set customization file path
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Set up PATH inside Emacs by reading from the shell's PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; Packages
(require 'package)
;; (add-to-list 'package-archives
;; 	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defvar df/packages '(better-defaults
		      clojure-mode
		      clojure-test-mode
		      cider
                      bubbleberry-theme
                      projectile))

(dolist (p df/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Font
(when (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Consolas")
    (set-face-attribute 'default nil :height 200))

;; No splash screen
(setq inhibit-splash-screen t)

;; Color theme
(load-theme 'bubbleberry)

;; Projectile
(projectile-global-mode)
