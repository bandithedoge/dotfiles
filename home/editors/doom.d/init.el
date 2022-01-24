(doom! :input

       :completion
       (company +childframe)
       (ivy +fuzzy +icons)

       :ui
       (emoji +unicode +github)
       (ligatures +extra)
       (popup +defaults)
       (treemacs +lsp)
       doom
       doom-dashboard
       doom-quit
       hl-todo
       indent-guides
       modeline
       nav-flash
       ophints
       tabs
       unicode
       vc-gutter
       window-select
       workspaces
       hydra

       :editor
       (dired +ranger +icons)
       (evil +everywhere)
       (format +onsave)
       electric
       file-templates
       fold
       parinfer
       snippets
       word-wrap

       :emacs
       (undo +tree)
       vc

       :term
       vterm

       :checkers
       (syntax +childframe)
       spell

       :tools
       (eval +overlay)
       (lookup +dictionary +docsets)
       (lsp +peek)
       debugger
       direnv
       editorconfig
       magit
       make
       pdf
       rgb

       :os
       (:if IS-MAC macos)

       :lang
       (cc +lsp)
       (go +lsp)
       (json +lsp)
       (org +pandoc +pretty)
       (python +lsp)
       (ruby +lsp)
       (rust +lsp)
       (sh +lsp +fish)
       (zig +lsp)
       emacs-lisp
       lua
       markdown
       nim
       nix
       web

       :config
       (default +bindings +smartparens))
 
