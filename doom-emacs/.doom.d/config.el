;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; user name
(setq user-full-name "bandithedoge"
      user-mail-address "bandithedoge@protonmail.com")

;;; font & theme
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-theme 'doom-dracula)

;;; org directory
(setq org-directory "~/org/")

;;; line numbers
(setq display-line-numbers-type `relative)

(elcord-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
