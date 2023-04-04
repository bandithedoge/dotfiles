(let [ts (require :nvim-treesitter.configs)
      ts-rainbow (require :ts-rainbow)]
  (ts.setup {:highlight {:enable true}
             :indent {:enable true}
             :rainbow {:enable true :strategy ts-rainbow.strategy.global}
             :playground {:enable true}
             :autotag {:enable true :filetypes [:html :xml]}
             :context_commentstring {:enable true}}))

(let [spellsitter (require :spellsitter)]
  (spellsitter.setup))
