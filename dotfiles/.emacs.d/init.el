;;; init.el --- Pure's Personal Emacs Configuration
;;;
;;; Commentary:
;;;
;;; I honestly do not know what I'm doing, but here.
;;;
;;; Code:

(defun pure/base ()
  "Load global configurations."
  (setq inhibit-startup-message t)
  (setq require-final-newline t)
  (setq custom-file "~/.emacs.d/custom.el")

  (scroll-bar-mode -1)
  (tool-bar-mode   -1)
  (menu-bar-mode   -1)
  (tooltip-mode    -1)
  (fringe-mode      8)
  (set-window-fringes (minibuffer-window) 0 0 nil t)
  (column-number-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

  (set-face-attribute
     'default nil :font "Monoid Nerd Font:antialias=subpixel:size=12")

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))


(defun pure/packages ()
  "Load extra packages."

  (require 'package)

  (setq package-archives
	'(("melpa" . "https://melpa.org/packages/")
	  ("org" . "https://orgmode.org/elpa/")
	  ("elpa" . "https://elpa.gnu.org/packages/")))

  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package)
    (setq use-package-always-ensure t))

  (require 'diminish)
  (require 'bind-key)

  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)

  (pure/packages/themes)
  (pure/packages/tools)
  (pure/packages/utils)
  (pure/packages/languages))

(defun pure/packages/utils ()
  "Load utility packages."

  (use-package ivy
    :diminish
    :demand
    :bind
    ( :map ivy-minibuffer-map
      ("TAB" . ivy-alt-done)
      ("C-l" . ivy-alt-done)
      ("C-j" . ivy-next-line)
      ("C-k" . ivy-previous-line)
      :map ivy-switch-buffer-map
      ("C-k" . ivy-previous-line)
      ("C-l" . ivy-done)
      ("C-d" . ivy-switch-buffer-kill)
      :map ivy-reverse-i-search-map
      ("C-k" . ivy-previous-line)
      ("C-d" . ivy-previous-i-search-kill))
    :config (ivy-mode))

  (use-package evil
    :ensure t
    :init
    (setq evil-want-keybinding nil)
    (setq evil-undo-system 'undo-tree)
    :config
    (evil-mode)
    (evil-set-leader 'normal (kbd "SPC"))
    (evil-define-key 'normal 'global (kbd "<leader>:") 'eval-expression)
    (evil-define-key 'normal 'global (kbd "<leader>;") 'execute-extended-command)
    (evil-define-key 'insert 'global (kbd "M-i") 'evil-force-normal-state)
    (evil-define-key 'normal 'global (kbd "M-i") 'evil-insert))

  (use-package undo-tree
    :ensure t
    :diminish
    :init
    (global-undo-tree-mode))

  (use-package evil-collection
    :after evil
    :demand
    :config (evil-collection-init))

  (use-package evil-org
    :ensure t
    :after org evil
    :hook (org-mode . (lambda () evil-org-mode))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (use-package evil-snipe
    :diminish evil-snipe-local-mode
    :ensure t
    :after evil
    :init
    (setq evil-snipe-scope 'visible))
    :config
    (evil-snipe-mode 1)

  (use-package elcord
    :config (elcord-mode))

  (use-package which-key
    :demand
    :diminish
    :config
    (which-key-mode))

  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package direnv
    :config (direnv-mode))

  (use-package ws-butler
    :diminish
    :hook (prog-mode . ws-butler-mode))
  )

(defun pure/packages/tools ()
  "Load tool packages."

  (use-package projectile
    :diminish
    :demand
    :bind ("C-c p" . projectile-command-map)
    :config (projectile-mode +1))

  (use-package magit
    :bind ("C-c g" . magit)
    :config
    (setq git-commit-summary-max-length 50)
    (setq fill-column 72))

  (use-package org
    :bind ("C-c q" . org-agenda)
    :hook
    (org-mode . +pure/setup-org-mode)
    (org-mode . +pure/setup-org-fonts)
    :init
    (setq org-directory "/home/pure/PureFunctor/Org")
    (setq org-agenda-files (list org-directory))
    (setq org-ellipsis " ∀")
    (setq org-hide-emphasis-markers t))

  (use-package vterm
    :ensure t)

  (use-package flycheck
    :diminish
    :demand
    :ensure t
    :bind
    ("M-n" . 'flycheck-next-error)
    ("M-p" . 'flycheck-previous-error)
    ("M-l" . 'flycheck-list-errors)
    :init
    (global-flycheck-mode))

  (use-package company
    :diminish
    :ensure t
    :bind ("C-SPC" . 'company-complete)
    :init (global-company-mode))

  (use-package yasnippet
    :ensure t
    :init
    (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

  (use-package lsp-mode
    :init
    (setq lsp-keymap-prefix "C-c v")
    :hook
    (python-mode . lsp)
    (lsp-mode . lsp-enable-which-key-integration)
    :commands lsp)

  (use-package lsp-ui
    :after lsp-mode)

  (use-package lsp-pyright
    :after lsp-mode
    :hook (python-mode . (lambda ()
			   (require 'lsp-pyright)
			   (lsp))))
  )

(defun pure/packages/themes ()
  "Load theme packages."

  (use-package base16-theme
    :config (load-theme 'base16-horizon-dark t))

  (use-package emojify
    :hook (after-init . global-emojify-mode))

  (use-package org-superstar
    :ensure t
    :after org
    :hook (org-mode . (lambda () (org-superstar-mode 1)))
    :config (setq org-superstar-leading-bullet ?\s))

  (use-package visual-fill-column
    :hook (org-mode . +pure/setup-org-filled))

  (use-package ample-theme
    :init
    (load-theme 'ample t t)
    (load-theme 'ample-flat t t)
    (load-theme 'ample-light t t)
    :defer t
    :ensure t)
  )

(defun pure/packages/languages ()
  "Load language packages."

  (use-package haskell-mode
    :bind
    ("C-c f" . 'haskell-mode-stylish-buffer)
    :hook
    (haskell-mode . (lambda () (setq-local eldoc-documentation-function nil))))

  (use-package dante
    :ensure t
    :after haskell-mode
    :commands 'dante-mode
    :bind
    ("C-c r" . 'dante-restart)
    ("M-[" . 'xref-find-references)
    ("M-]" . 'xref-find-definitions)
    :hook
    (haskell-mode . dante-mode)
    :init
    (setq dante-tap-type-time 0.25)
    :config
    (flycheck-add-next-checker 'haskell-dante 'haskell-hlint))

  (use-package purescript-mode
    :hook
    (purescript-mode . turn-on-purescript-indentation)
    (purescript-indentation-mode . (lambda () (diminish 'purescript-indentation-mode)))
    :mode ("\\.purs$" . purescript-mode))

  (use-package psc-ide
    :ensure t
    :bind
    ("C-c /" . 'psc-ide-flycheck-insert-suggestion)
    ("M-]" . 'psc-ide-goto-definition)
    :after purescript-mode
    :hook
    (purescript-mode . psc-ide-mode))

  (use-package nix-mode
    :mode "\\.nix\\'")

  (use-package yaml-mode
    :mode "\\.yaml\\'")
  )


(defun +pure/setup-org-mode ()
  "Startup configuration for `org-mode`."
  (org-indent-mode)
  (setq evil-auto-indent nil))

(defun +pure/setup-org-fonts ()
  "Startup configuration for `org-mode` fonts."
  (dolist (face '((org-level-1 . 1.50)
		  (org-level-2 . 1.35)

		  (org-level-3 . 1.20)
		  (org-level-4 . 1.05)
		  (org-level-5 . 1.00)
		  (org-level-6 . 1.00)
		  (org-level-7 . 1.00)
		  (org-level-8 . 1.00)))
    (set-face-attribute (car face) nil :font "Monoid Nerd Font" :weight 'regular :height (cdr face))))

(defun +pure/setup-org-filled ()
  "Startup configuration for `org-mode` fill."
  (defvar visual-fill-column-width 100)
  (defvar visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))


(pure/base)
(pure/packages)
;;; init.el ends here
