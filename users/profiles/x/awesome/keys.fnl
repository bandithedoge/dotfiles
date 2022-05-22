(local awful (require :awful))
(local gears (require :gears))

(local join gears.table.join)
(local key awful.key)

(local M {})

(let [mod :Mod1
      menu (awful.menu {:items [[:restart awesome.restart]
                                [:quit awesome.quit]]})]
  ;; global {{{
  (set M.globalkeys
       (join (key [mod :Control] :r awesome.restart)
             (key [mod] :Return #(awful.spawn _G.terminal))
             (key [mod] :space #(menu:show))
             (key [mod] :j #(awful.client.focus.byidx 1))
             (key [mod] :k #(awful.client.focus.byidx -1))
             (key [mod :Shift] :j #(awful.client.swap.byidx 1))
             (key [mod :Shift] :k #(awful.client.swap.byidx -1))
             (key [mod] :h #(awful.tag.incmwfact -0.02))
             (key [mod] :l #(awful.tag.incmwfact 0.02))
             (key [mod :Shift] :h #(awful.tag.incnmaster 1 nil true))
             (key [mod :Shift] :l #(awful.tag.incnmaster -1 nil true))
             (key [mod] :Tab #(awful.layout.inc 1))))
  ;; }}}
  ;; tags {{{
  (for [i 1 9]
    (set M.globalkeys
         (join M.globalkeys
               (key [mod] (.. "#" (+ i 9))
                    #(let [screen (awful.screen.focused)
                           tag (. screen.tags i)]
                       (when tag
                         (tag:view_only))))
               (key [mod :Shift] (.. "#" (+ i 9))
                    #(when client.focus
                       (let [tag (. client.focus.screen.tags i)
                             focus client.focus]
                         (focus:move_to_tag tag)))))))
  ;; }}}
  ;; client {{{
  (set M.clientkeys (join (key [mod] :w #($1:kill))
                          (key [mod] :f
                               #(do
                                  (set $1.fullscreen (not $1.fullscreen))
                                  ($1:raise))))))

;; }}}

M
