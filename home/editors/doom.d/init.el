(doom! :input

       :completion
       (company +childframe)
       (ivy +fuzzy +icons)

       :ui
       doom
       doom-dashboard
       doom-quit
       (emoji +unicode +github)
       hl-todo
       indent-guides
       (ligatures +extra)
       modeline
       nav-flash
       ophints
       (popup +defaults)
       tabs
       (treemacs +lsp)
       unicode
       vc-gutter
       window-select
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       parinfer
       snippets
       word-wrap
       (dired +ranger +icons)
       electric

       :emacs
       (undo +tree)
       vc

       :term
       vterm

       :checkers
       spell
       (syntax +childframe)

       :tools
       debugger
       direnv
       editorconfig
       (eval +overlay)
       (lookup +dictionary +docsets)
       (lsp +peek)
       magit
       pdf
       rgb

       :os
       (:if IS-MAC macos)

       :lang
       (cc +lsp)
       emacs-lisp
       (json +lsp)
       lua
       markdown
       nim
       nix
       (org +pandoc +pretty)
       (python +lsp)
       (ruby +lsp)
       (rust +lsp)
       (sh +lsp +fish)

       :config
       (default +bindings +smartparens))
 
