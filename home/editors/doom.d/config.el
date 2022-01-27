(setq doom-modeline-height 30
      doom-modeline-major-mode-icon t
      doom-modeline-enable-word-count t
      doom-modeline-buffer-encoding nil
      doom-modeline-indent-info t
      doom-modeline-buffer-encoding t)

(+global-word-wrap-mode +1)

(use-package! mixed-pitch
  :hook (text-mode . mixed-pitch-mode)
  :config (setq mixed-pitch-fixed-pitch-faces ()))
