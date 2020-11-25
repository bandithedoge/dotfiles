;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; evil-mode
(straight-use-package 'evil)
(straight-use-package 'evil-collection)

(setq evil-want-keybinding 'nil)
(evil-mode 1)
(evil-collection-init)

(straight-use-package 'undo-fu)
(evil-set-undo-system 'undo-fu)

;; evil-mode leader keybindings
(straight-use-package 'evil-leader)
(straight-use-package 'evil-god-state)

(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>" 'counsel-M-x'
  "f" 'find-file
  "b" 'counsel-switch-buffer
  "S-<SPC>" 'evil-execute-in-god-state
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "s" 'evil-window-split
  "v" 'evil-window-vsplit
  "W" 'evil-window-delete
  "w" 'kill-current-buffer
  "B" 'kill-buffer
  "y" 'yas-insert-snippet
  "g" 'magit
  "t" 'treemacs
  "v" 'evil-visual-block)

;; org mode
(straight-use-package 'org)

;; org evil keybindings
(straight-use-package 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)

;; org table of contents
(straight-use-package 'toc-org)
(add-hook 'org-mode-hook 'toc-org-mode)

;; pretty org headings
(straight-use-package 'org-bullets)
(add-hook 'org-mode-hook #'org-bullets-mode)

;; pretty org fonts
(straight-use-package 'org-variable-pitch)
(add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)

;; pretty org links
(straight-use-package 'org-link-beautify)
(org-link-beautify-mode 1)

;; pretty org indents
(add-hook 'org-mode-hook 'org-indent-mode)

;; ivy autocompletion
(straight-use-package 'ivy)
(ivy-mode 1)

;; ivy-based menus
(straight-use-package 'counsel)
(counsel-mode 1)

;; ivy completion strategy
(setq ivy-re-builders-alist
      '((t . ivy--regex)))

;; ivy icons
(straight-use-package 'all-the-icons-ivy)
(all-the-icons-ivy-setup)
(setq all-the-icons-ivy-file-commands
      '(counsel-find-file))

(setq ivy-height 17
      ivy-wrap t)

;; which-key
(straight-use-package 'which-key)
(which-key-mode)

;; modeline
(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 35
      doom-modeline-bar-width 3
      doom-modeline-enable-word-count t
      doom-modeline-indent-info t)

;; company autocompletion
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; treemacs file tree
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-all-the-icons)
(straight-use-package 'treemacs-evil)
(straight-use-package 'treemacs-magit)
(straight-use-package 'treemacs-projectile)

;; theme
(straight-use-package 'solaire-mode)
(solaire-global-mode +1)
(straight-use-package 'doom-themes)
(load-theme 'doom-dracula t)

;; line numbers
(global-display-line-numbers-mode)
;; highlight current line
(global-hl-line-mode)
;; hide ui elements
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; rainbow delimiters
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(straight-use-package 'dashboard)
(dashboard-setup-startup-hook)

;; yasnippet
(straight-use-package 'yasnippet)
(yas-global-mode)

(straight-use-package 'yasnippet-snippets)

;; projectile
(straight-use-package 'projectile)

(straight-use-package 'magit)
(straight-use-package 'magit-todos)
(straight-use-package 'magithub)

(straight-use-package 'evil-magit)
(setq evil-magit-state 'normal)

(electric-pair-mode)

;; haskell
(straight-use-package 'haskell-mode)
(straight-use-package 'company-ghc)

;; html/css/js
(straight-use-package 'web-mode)
(straight-use-package 'company-web)
