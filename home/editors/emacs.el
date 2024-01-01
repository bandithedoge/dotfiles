(eval-when-compile
  (require 'use-package))

(require 'bind-key)

(use-package use-package
  :custom
  (use-package-always-defer t))

(use-package emacs
  :init
  (set-face-attribute 'default nil
                      :family mono-font
                      :height 110)
  (set-face-attribute 'variable-pitch nil
                      :family ui-font
                      :height 130))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package evil
  :init
  (unbind-key "SPC" override-global-map)
  (setq evil-want-keybinding nil)
  (use-package undo-fu)
  (use-package goto-chg)
  :bind*
  ("<leader>w" ("Kill buffer" . kill-this-buffer))
  ("<leader>W" ("Close window" . evil-window-delete))
  :custom
  (evil-echo-state nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-ex-search-vim-style-regexp t)
  (evil-ex-visual-char-range t)
  (evil-kbd-macro-suppress-motion-error t)
  (evil-mode-line-format 'nil)
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-fu)
  (evil-visual-state-cursor 'hollow)
  (evil-want-C-g-bindings t)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  (evil-set-leader '(motion) (kbd "SPC"))
  (evil-set-leader '(motion) (kbd "DEL") t)
  (use-package evil-collection
    :config
    (evil-collection-init))
  (use-package evil-nerd-commenter
    :commands (evilnc-comment-operator evilnc-inner-comment evilnc-outer-commenter)
    :bind (:map evil-motion-state-map
                ("gc" . evilnc-comment-operator)))
  (use-package evil-traces
    :after evil-ex
    :config
    (evil-traces-mode))
  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search evil-visualstar/begin-search-forward evil-visualstar/begin-search-backward)
    :bind (:map evil-visual-state-map
                ("*" . evil-visualstar/begin-search-forward)
                ("#" . evil-visualstar/begin-search-backward)))
  (use-package vimish-fold
    :config
    (use-package evil-vimish-fold
      :config
      (global-evil-vimish-fold-mode 1))))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font"))

(use-package better-defaults)

(use-package no-littering)

(use-package doom-themes
  :config
  (use-package all-the-icons)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-treemacs-config)
  (use-package solaire-mode
    :config
    (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
    (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)
    (solaire-global-mode +1)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-enable-word-count t)
  (doom-modeline-height 30)
  (doom-modeline-indent-info t)
  (doom-modeline-irc nil)
  (doom-modeline-time nil)
  :config
  (use-package anzu
    :config
    (global-anzu-mode +1)
    (use-package evil-anzu
      :config
      (global-anzu-mode +1))))

(use-package smartparens
  :hook (after-change-major-mode . smartparens-mode)
  :init
  (use-package evil-smartparens
    :hook smartparens-enabled-hook))

(use-package parinfer-rust-mode
  :hook ((emacs-lisp-mode lisp-mode fennel-mode) . parinfer-rust-mode)
  :custom
  (parinfer-rust-auto-download t)
  (parinfer-rust-troublesome-modes 'nil))

(use-package which-key
  :custom
  (which-key-idle-delay 0.3)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package orderless
 :custom
 (completion-styles '(orderless basic))
 (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-M-j" . vertico-next-group)
              ("C-k" . vertico-previous)
              ("C-M-j" . vertico-next-group))
  :init
  (use-package consult
    :bind* 
    ("<leader>SPC" ("Execute command" . execute-extended-command))
    ("<leader>b" ("Switch buffer" . consult-buffer)))
  (use-package marginalia
    :bind (:map minibuffer-local-map
                ("C-h" . marginalia-cycle))
    :init
    (marginalia-mode)
    :config
    (use-package nerd-icons-completion
      :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
      :config
      (nerd-icons-completion-mode)))
  (vertico-mode)
  (vertico-mouse-mode)
  :custom
  (vertico-count 17)
  (vertico-cycle t)
  (completion-in-region-function (lambda (&rest args)
                                   (apply (if vertico-mode
                                              #'consult-completion-in-region
                                            #'completion--in-region)
                                          args))))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :init
  (use-package flycheck-posframe
    :hook (flycheck-mode . flycheck-posframe-mode)
    :custom
    (flycheck-posframe-warning-prefix (concat (nerd-icons-faicon "nf-fa-warning") " "))
    (flycheck-posframe-error-prefix (concat (nerd-icons-faicon "nf-fa-exclamation_circle") " "))
    (flycheck-posframe-info-prefix (concat (nerd-icons-faicon "nf-fa-info_circle") " "))
    (flycheck-posframe-inhibit-functions '(evil-insert-state-p evil-replace-state-p)))
  (use-package consult-flycheck)
  :custom
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled idle-buffer-switch))
  (flycheck-display-errors-delay 0.25)
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package corfu
  :after evil
  :bind (:map corfu-popupinfo-map
              ("C-h" . corfu-popupinfo-scroll-up)
              ("C-l" . corfu-popupinfo-scroll-down))
  :bind (:map evil-insert-state-map
              ("C-SPC" . completion-at-point))
  :custom
  (corfu-auto t)
  (corfu-count 10)
  (corfu-cycle t)
  (corfu-left-margin-width 1)
  (corfu-margin-formatters '(nerd-icons-corfu-formatter))
  (corfu-on-exact-match nil)
  (corfu-preselect 'prompt)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary nil)
  (corfu-right-margin-width 1)
  (corfu-popupinfo-delay '(0.5 . 0.25))
  (corfu-popupinfo-max-height 20)
  :init
  (use-package cape
    :init
    (add-to-list 'completion-at-point-functions #'cape-file))
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode)
  (use-package nerd-icons-corfu))

(use-package tempel
  :init
  (add-to-list 'completion-at-point-functions #'tempel-complete)
  (use-package tempel-collection))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package projectile
  :custom
  (projectile-auto-discover nil)
  (projectile-globally-ignored-directories '("^\\.direnv$" "^\\result*$"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-ignored-projects '("~/"))
  :config
  (projectile-mode 1))

(use-package treemacs
  :bind* ("<leader>t" ("File tree" . treemacs))
  :bind (:map evil-treemacs-state-map
              ("o v" . treemacs-visit-node-horizontal-split)
              ("o s" . treemacs-visit-node-vertical-split))
  :custom
  (treemacs-follow-after-init t)
  (treemacs-eldoc-display 'detailed)
  (treemacs-fringe-indicator-mode nil)
  (treemacs-indent-guide-mode t)
  (treemacs-no-png-images t)
  :custom-face (treemacs-directory-face ((t :inherit (variable-pitch))))
  :config
  (treemacs-follow-mode -1)
  (treemacs-git-mode 'deferred)
  (use-package treemacs-nerd-icons
    :config
    (treemacs-load-theme "nerd-icons"))
  (use-package treemacs-evil)
  (use-package treemacs-projectile))
