;;; lein-run-mode.el --- A minor mode for the Clojure 'lein run' command


;; Stolen from cljsbuild-mode.el


;; Copyright 2012 Kototama

;; Authors: Kototama <kototamo gmail com>
;; Version: 0.4.0
;; Package-version: 0.4.0
;; Keywords: clojure, clojurescript, leiningen, compilation
;; URL: http://github.com/kototama/cljsbuild-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; An Emacs minor mode for the ClojureScript 'lein cljsbuild' command
;; that will automatically watch the compilation buffer, pops it when the
;; compilation failed and (optionally) hides it when the compilation
;; succeed.

;; Installation:
;;
;; Packages are available in the Marmalade and MELPA repositories.
;; Install the mode with "M-x package-install RET cljsbuild-mode".
;;
;; Usage:
;;
;; M-x lein-run
;;
;;; Code:

(require 'ansi-color)
(require 'compile)

(defgroup lein-run-mode nil
  "A helper mode for running 'lein run' within Emacs."
  :prefix "lein-run-"
  :group 'applications)

;;;###autoload
(define-minor-mode lein-run-mode
  "Lein run mode"
  :init-value nil
  :lighter " Lein-Run"
  :group 'lein-run-mode
  :after-hook (lein-run-init-mode))

(defcustom lein-run-verbose t
  "When non-nil, provide progress feedback in the minibuffer."
  :type 'boolean
  :group 'lein-run-mode)

(defcustom lein-run-show-buffer-on-failure t
  "When non-nil, pop up the build buffer when failures are seen."
  :type 'boolean
  :group 'lein-run-mode)

(defcustom lein-run-hide-buffer-on-success nil
  "When non-nil, hide the build buffer when a build succeeds."
  :type 'boolean
  :group 'lein-run-mode)

(defcustom lein-run-show-buffer-on-warnings t
  "When non-nil, pop up the build buffer when warnings are seen."
  :type 'boolean
  :group 'lein-run-mode)

(defun lein-run-message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS through to `message' if `cljsbuild-verbose' is non-nil."
  (when lein-run-verbose
    (apply #'message format-string args)))

(defcustom lein-run-compile-command
  "lein run -dev"
  "Default build command to use for `lein-run-compile'."
  :type 'string
  :group 'lein-run-mode)

(defconst lein-run-compilation-error-regexp-alist
  '(("^Caused by: .+{:column \\([0-9]+\\), :line \\([0-9]+\\), :file \"\\(.+\\)\""
     3 2 1 nil 3)
    ("^ERROR: .+ error at \\(.+\\) line \\([0-9]+\\) : \\([0-9]+\\)"
     1 2 3 nil 3)
    ("^WARNING: .+ at line \\([0-9]+\\) \\(.+\\.cljs\\)"
     2 1 1 2 2)
    ("^WARNING: .+ at line \\([0-9]+\\) file:\\(.+\\)"
     2 1 1 1 2))
  "Regexps used for matching Clojure error messages.
See `compilation-error-regexp-alist' for semantics.")

(defvar lein-run-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map)
    (define-key map (kbd "g") 'lein-run-start)
    map)
  "Keymap for `lein-run-compilation-mode' buffers.")

(defun lein-run-on-buffer-change
  (beginning end &optional len)
  ;; (let ((inserted (buffer-substring-no-properties beginning end))
  ;;       (buffer-visible (get-buffer-window (buffer-name) 'visible)))
  ;;   (cond ((string-match "^Successfully compiled" inserted)
  ;;          (cljsbuild-message "Cljsbuild compilation success")
  ;;          (when cljsbuild-hide-buffer-on-success
  ;;            ;; hides the compilation buffer
  ;;            (delete-windows-on (buffer-name))))
  ;;         ((string-match "^Compiling.+failed.$" inserted)
  ;;          (cljsbuild-message "Cljsbuild compilation failure")
  ;;          (when (and (not buffer-visible) cljsbuild-show-buffer-on-failure)
  ;;            ;; if the compilation buffer is not visible, shows it
  ;;            (switch-to-buffer-other-window (buffer-name) t)))
  ;;         ((string-match "^WARNING:" inserted)
  ;;          (cljsbuild-message "Cljsbuild compilation warning")
  ;;          (when (and (not buffer-visible) cljsbuild-show-buffer-on-warnings)
  ;;           (switch-to-buffer-other-window (buffer-name) t)))))
)

(defun lein-run-init-mode ()
  "Initialize the minor mode and register a change hook on the
compilation buffer"
  ;(remove-hook 'after-change-functions 'lein-run-on-buffer-change)
  ;(add-hook 'after-change-functions 'lein-run-on-buffer-change nil t)
)

(defun lein-run-process-sentinel
  (process event)
  "Display a message when a change to the process occurs."
  (message "lein run: %s" event))

;; Functions using compile-mode as cljsbuild output:
(defun lein-run-compilation-filter-hook ()
  "Local `lein-run-filter-hook' for `lein-run-compilation-mode'."
  (ansi-color-apply-on-region compilation-filter-start (point-max))
  (lein-run-on-buffer-change compilation-filter-start (point-max)))

(define-compilation-mode lein-run-compilation-mode "lein-run"
  "Lein run `compilation-mode'."
  (set (make-local-variable 'compilation-error-regexp-alist)
       lein-run-compilation-error-regexp-alist)

  (add-hook 'compilation-filter-hook
            'lein-run-compilation-filter-hook nil t))

(defun lein-run-do-compile (command)
;  (save-some-buffers (not compilation-ask-about-save)
;                     compilation-save-buffers-predicate)
  (compilation-start command 'lein-run-compilation-mode))

;;;###autoload
(defun lein-run-start (command)
  "Runs lein run."
  (interactive
   (list (read-string "lein run command: "
		      lein-run-compile-command
		      nil
		      '("lein run"))))
  (let ((project-dir (locate-dominating-file default-directory "project.clj")))
    (if project-dir
	(cd project-dir)
      (error "Not inside a Leiningen project")))
  (lein-run-do-compile command))

(provide 'lein-run-mode)

;;; lein-run-mode.el ends here
