;; (defvar bootstrap-version)
 ;; (let ((bootstrap-file
 ;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
 ;;       (bootstrap-version 5))
 ;;   (unless (file-exists-p bootstrap-file)
 ;;     (with-current-buffer
 ;;         (url-retrieve-synchronously
 ;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
 ;;          'silent 'inhibit-cookies)
 ;;       (goto-char (point-max))
 ;;       (eval-print-last-sexp)))
 ;;   (load bootstrap-file nil 'nomessage))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org". "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
 (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package undo-fu)

;; evil-mode
(use-package evil
  :init
  (setq evil-want-keybinding 'nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-fu))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; evil-mode leader keybindings
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
    "v" 'evil-visual-block)

;; org mode
(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode))

;; org evil keybindings
(use-package evil-org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode))

;; org table of contents
(use-package toc-org
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

;; pretty org headings
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

;; pretty org fonts
(use-package org-variable-pitch
  :config
  (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode))

;; pretty org links
(use-package org-link-beautify
  :config
  (org-link-beautify-mode 1))

;; ivy autocompletion
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((t . ivy--regex)))
  (setq ivy-height 17
        ivy-wrap t))

;; ivy-based menus
(use-package counsel
  :config
  (counsel-mode 1))

;; ivy icons
(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-file-commands
          '(counsel-find-file)))

(use-package swiper)

;; which-key
(use-package which-key
  :config
  (which-key-mode))

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
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; treemacs file tree
(use-package treemacs)
(use-package treemacs-all-the-icons)
(use-package treemacs-evil)
(use-package treemacs-magit)
(use-package treemacs-projectile)

;; theme
(use-package solaire-mode
  :config
  (solaire-global-mode +1))
(use-package doom-themes)
(load-theme 'doom-dracula t)

;; line numbers
(global-display-line-numbers-mode)
;; highlight current line
(global-hl-line-mode)
;; hide ui elements
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; rainbow delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; font
(set-face-attribute 'default nil :font "FiraCode Nerd Font")

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode))

(use-package yasnippet-snippets)

;; projectile
(use-package projectile)

(use-package magit)
(use-package magit-todos)
(use-package magithub)

(use-package evil-magit
  :config
  (setq evil-magit-state 'normal))

(electric-pair-mode)

(use-package elcord
  :config
  (elcord-mode))

;; haskell
(use-package haskell-mode)
(use-package company-ghc)

;; html/css/js
(use-package web-mode)
(use-package company-web)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
	   (lambda nil
	     (if
		 (y-or-n-p "Tangle?")
		 (org-babel-tangle)))
	   nil t))))
