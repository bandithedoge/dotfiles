(let [ts (require :nvim-treesitter.configs)]
  (ts.setup {:highlight {:enable true}
             :indent {:enable true}
             :rainbow {:enable true :extended_mode true}
             :playground {:enable true}
             :autotag {:enable true}
             :context_commentstring {:enable true}
             :yati {:enable true}}))

(let [spellsitter (require :spellsitter)]
  (spellsitter.setup))
