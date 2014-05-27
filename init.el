;; Utility functions
(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun list-buffer-names ()
  (filter 'identity (mapcar (lambda (el) (buffer-name el)) (buffer-list))))

(defun find-buffers (buffer)
	(filter (lambda (b) (string-match buffer b)) (list-buffer-names)))

(defun mark-buffer-umodified (buffer)
	(when (get-buffer buffer)
    (switch-to-buffer buffer)
    (set-buffer-modified-p nil)))

;; Set customization file path
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Packages
(require 'package)
;; (add-to-list 'package-archives
;;       '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defvar df/packages '(better-defaults
                      clojure-mode
                      clojure-test-mode
                      cider
                      bubbleberry-theme
                      projectile
                      dirtree
                      magit))

(dolist (p df/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Use a box cursor by default but a bar when we're selecting a region
(defun set-cursor-type-bar () (setq cursor-type 'bar))
(defun set-cursor-type-box () (setq cursor-type 'box))

(add-hook 'activate-mark-hook 'set-cursor-type-bar)
(add-hook 'deactivate-mark-hook 'set-cursor-type-box)

;; 90 character right margin
(setq-default fill-column 90)

;; 2 space tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; I can't type y-e-s all the time
(fset 'yes-or-no-p 'y-or-n-p)

;; Show line and column in the modeline
(linum-mode t)
(column-number-mode t)

;; Command up and down to go to the beginning and end of a buffer
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

;; Dirtree
(require 'dirtree)

;; Set up PATH inside Emacs by reading from the shell's PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))



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

;; Cider/clojure
(setq cider-repl-pop-to-buffer-on-connect nil) ;; don't send me to the repl on connect

;; (defun cider-force-quit ()
;;   (interactive)
;;   (dolist (connection nrepl-connection-list)
;;     (when connection
;;       (nrepl-close connection)))
;;   (message "All active nREPL connections were closed")
;;   (cider-close-ancillary-buffers))

;; (defun cider-switch-repl (project-root project-name server buffer)
;;   (cd project-root)
;;   (cider-jack-in)
;;   (switch-to-buffer server)
;;  (rename-buffer buffer)
;;  (clojure-mode)
;;   (make-directory (concat "~/tmp/emacs/" project-name) t)
;;   (let ((fname (concat "~/tmp/emacs/" project-name (format "/%s" buffer))))
;;     (when (file-exists-p fname)
;;       (delete-file fname))
;;     (write-file fname))
;;   (cd project-root)
;;   (bury-buffer))

;; (defun cider-switch-project (project-root)
;; ;  (interactive (list (ido-read-directory-name "Project Root: " (locate-dominating-file default-directory "project.clj"))))
;;  (let ((project-name (file-name-nondirectory (directory-file-name project-root))))
;;    (dolist (x (find-buffers "*nrepl-server"))
;;      (mark-buffer-umodified x))
;;    (cider-force-quit)
;;      (when (equal current-prefix-arg nil)
;;        (mapc 'kill-buffer (buffer-list)))
;;       (switch-repl project-root project-name (format "*nrepl-server %s*" project-name) (format "*nrepl-server %s*" project-name))))

;; (defun clojure-project-p (dir)
;;   (locate-dominating-file dir "project.clj"))

;; (add-hook 'projectile-switch-project-hook
;;           (lambda ()
;;             (when (clojure-project-p (projectile-project-root))
;;               (cider-switch-project (projectile-project-root)))))
