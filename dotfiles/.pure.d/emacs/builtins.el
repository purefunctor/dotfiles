(require 'constants)

(defun builtins/modes ()
  "Load global modes."
  (menu-bar-mode   -1)
  (tool-bar-mode   -1)
  (scroll-bar-mode -1)
  (tooltip-mode    -1)
  (fringe-mode      8)
  (column-number-mode)
  (electric-pair-mode)
  (global-auto-revert-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(defun builtins/binds ()
  "Load global binds."
  (setq
    inhibit-startup-message t
    require-final-newline t
    custom-file user-custom-file
    read-process-output-max (* 1024 32)
    dired-listing-switches "-alvF --group-directories-first"
    backup-directory-alist `((".*" . ,user-backup-directory))
    auto-save-file-name-transforms `((".*" ,user-backup-directory t))
    python-indent-guess-indent-offset nil
    shift-select-mode nil
    help-window-select t))

(defun builtins/fonts ()
  "Load font configuration."
  (set-face-attribute 'default nil :font "Fira Code" :height 98)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 98)
  (set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 98))

(defun builtins/frame ()
  "Load frame configuration."
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90))))

(defun builtins/entry ()
  "Entry point for `builtins.el`."
  (builtins/modes)
  (builtins/binds)
  (builtins/fonts)
  (builtins/frame))

(provide 'builtins)
