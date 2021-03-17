;; Startup Options
(setq inhibit-startup-message t)

;; Visual Options
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(tooltip-mode    -1)
(set-fringe-mode -1)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; Font Options
(set-face-attribute 'default nil :font "Monoid Nerd Font:antialias=subpixel:size=12")

;; Bodge Options
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; General
(setq require-final-newline t)

(add-hook 'prog-mode-hook 'column-number-mode)

;; Packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Packages Proper
(use-package diminish)

(use-package ivy
  :diminish
  :demand
  :bind (:map ivy-minibuffer-map
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

(use-package company
  :config (global-company-mode))

(use-package evil
  :demand
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>:") 'eval-expression)
  (evil-define-key 'normal 'global (kbd "<leader>;") 'execute-extended-command)
  (evil-define-key 'insert 'global (kbd "M-i") 'evil-force-normal-state)
  (evil-define-key 'normal 'global (kbd "M-i") 'evil-insert))

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
  :diminish evil-snipe-mode
  :ensure t
  :after evil
  :init
  (setq evil-snipe-scope 'visible))
  :config
  (evil-snipe-mode 1)

(use-package projectile
  :diminish
  :demand
  :bind ("C-c p" . projectile-command-map)
  :config (projectile-mode +1))

(use-package elcord
  :config (elcord-mode))

(use-package base16-theme
  :config (load-theme 'base16-material-palenight t))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package magit
  :bind ("C-c g" . magit)
  :config
  (setq git-commit-summary-max-length 50)
  (setq fill-column 72))

(defun pure/org-mode-startup ()
  (org-indent-mode)
  (setq evil-auto-indent nil))

(defun pure/org-font-startup ()
  (dolist (face '((org-level-1 . 1.50)
		  (org-level-2 . 1.35)
		  (org-level-3 . 1.20)
		  (org-level-4 . 1.05)
		  (org-level-5 . 1.00)
		  (org-level-6 . 1.00)
		  (org-level-7 . 1.00)
		  (org-level-8 . 1.00)))
    (set-face-attribute (car face) nil :font "Monoid Nerd Font" :weight 'regular :height (cdr face))))

(defun pure/org-fill-center ()
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(use-package org
  :bind ("C-c q" . org-agenda)
  :hook (org-mode . pure/org-mode-startup)
  :config
  (setq org-directory "/home/pure/PureFunctor/Org")
  (setq org-agenda-files (list org-directory))
  (setq org-ellipsis " ∀")
  (setq org-hide-emphasis-markers t)
  (pure/org-font-startup))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :config (setq org-superstar-leading-bullet ?\s))

(use-package visual-fill-column
  :hook (org-mode . pure/org-fill-center))

(use-package which-key
  :demand
  :config
  (which-key-mode))

(use-package haskell-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c v")
  :hook ((haskell-mode . lsp)
	 (purescript-mode . lsp)
	 (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package purescript-mode
  :hook (purescript-mode . turn-on-purescript-indentation)
  :mode ("\\.purs$" . purescript-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package direnv
  :config (direnv-mode))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package flycheck
  :diminish
  :ensure t
  :init (global-flycheck-mode))

(use-package company
  :diminish
  :ensure t
  :bind ("C-SPC" . company-complete-common)
  :init (global-company-mode))

(use-package vterm
  :ensure t)

(use-package ample-theme
  :init
  (load-theme 'ample t t)
  (load-theme 'ample-flat t t)
  (load-theme 'ample-light t t)
  :defer t
  :ensure t)

;; Don't touch!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("59ba50f24540958f33699a5247255d10f34dd812f3975837e3eddccdc4caa32e" "68fb87a1805393d7a00ba5064d28b8277de5a75addf0d34094012408cfcf2eea" default))
 '(global-whitespace-cleanup-mode t)
 '(package-selected-packages
   '(lsp-pyright diminish ample-theme vterm yaml-mode purescript-mode flycheck ws-butler rainbow-delimiters visual-fill-column org-superstar evil-org company company-mode lsp-ui direnv lsp-haskell lsp-mode haskell-mode evil-collection which-key magit emojify base16-theme elcord use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
