(setq user-full-name "Mikołaj Lercher"
      user-mail-address "bandithedoge@protonmail.com")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
 (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package undo-fu)

(use-package evil
  :init (setq evil-want-keybinding 'nil
              evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-fu)
  (use-package evil-collection
    :after evil
    :config (evil-collection-init))
  (use-package evil-commentary
    :config (evil-commentary-mode)))

(use-package evil-god-state)

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'counsel-M-x'
    "f" 'find-file
    "b" 'counsel-switch-buffer
    "S-<SPC>" 'evil-execute-in-god-state
    "/" 'swiper

    "h" 'evil-window-left
    "j" 'evil-window-down
    "k" 'evil-window-up
    "l" 'evil-window-right

    "H" 'evil-window-move-far-left
    "J" 'evil-window-move-very-bottom
    "K" 'evil-window-move-very-top
    "L" 'evil-window-move-far-right

    "s" 'evil-window-split
    "S" 'evil-window-vsplit
    "W" 'evil-window-delete
    "w" 'kill-current-buffer
    "B" 'kill-buffer

    "y" 'yas-insert-snippet
    "g" 'magit
    "t" 'treemacs
    "v" 'evil-visual-block))

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode)
  (use-package yasnippet-snippets))

;; projectile
(use-package projectile)

(use-package magit)

(electric-pair-mode)

(use-package elcord
  :config (elcord-mode))

(use-package vterm)

(use-package pdf-tools)

(use-package flycheck
  :config
  (global-flycheck-mode)
  (use-package flycheck-inline
    :hook (flycheck-mode . flycheck-inline-mode)))

;; ivy autocompletion
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((t . ivy--regex)))
  (setq ivy-height 17
        ivy-wrap t)
  (use-package all-the-icons-ivy
    :config
    (all-the-icons-ivy-setup)
    (setq all-the-icons-ivy-file-commands
          '(counsel-find-file))))

;; ivy-based menus
(use-package counsel
  :config (counsel-mode 1))

(use-package swiper)

(use-package ace-popup-menu
  :config (ace-popup-menu-mode 1))

;; which-key
(use-package which-key
  :config (which-key-mode))

;; modeline
(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 35
        doom-modeline-bar-width 3
        doom-modeline-enable-word-count t
        doom-modeline-indent-info t))

;; company autocompletion
(use-package company
  :hook (after-init . global-company-mode)
  :config (use-package company-quickhelp))

(use-package treemacs
  :config
  (use-package treemacs-all-the-icons)
  (use-package treemacs-evil)
  (use-package treemacs-magit)
  (use-package treemacs-projectile))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode)
  :config (solaire-mode-in-minibuffer))

(add-to-list 'load-path "~/data/git/blueballs-theme/blueballs.el")
(add-to-list 'custom-theme-load-path "~/data/git/blueballs-theme/blueballs.el")
(load-theme 'blueballs-dark t)

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(set-face-attribute 'default nil :font "FiraCode Nerd Font-11")

(global-display-line-numbers-mode t)
(dolist (mode '(term-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-hl-line-mode)
(global-visual-line-mode)

(column-number-mode)
(size-indication-mode)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(use-package dashboard
  :config
  (use-package dashboard-project-status)
  (dashboard-setup-startup-hook))

(use-package highlight-indent-guides
  :config (setq highlight-indent-guides-method 'column)
  :hook (prog-mode . highlight-indent-guides-mode))

;; haskell
(use-package haskell-mode
  :config (use-package company-ghc))

(use-package web-mode
  :config (use-package company-web))

(use-package js2-mode
  :hook (js-mode)
  :config
  (setq js2-highlight-level 3)
  (use-package ac-js2
    :hook (js2-minor-mode)))

(use-package vue-mode)

(use-package elpy
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable))

(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-image-actual-width nil)
  (use-package evil-org
    :config (add-hook 'org-mode-hook 'evil-org-mode))
  (use-package toc-org
    :config (add-hook 'org-mode-hook 'toc-org-mode))
  (use-package org-bullets
    :config (add-hook 'org-mode-hook #'org-bullets-mode))
  (use-package org-link-beautify
    :config (org-link-beautify-mode 1))
  (use-package org-variable-pitch
    :config (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode))
  (use-package org-tree-slide)
  (use-package ox-pandoc)
  (use-package ox-hugo))

(use-package monkeytype)

(custom-set-variables
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (if
                 (y-or-n-p "Tangle?")
                 (org-babel-tangle)))
           nil t))))
