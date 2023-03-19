(local awful (require :awful))
(local gears (require :gears))

(local join gears.table.join)
(local mod :Mod4)
(local key awful.key)
(local button awful.button)

(local M
       {:globalkeys (join (key [mod :Control] :r awesome.restart)
                          (key [mod :Control] :q awesome.quit)
                          (key [mod] :Return #(awful.spawn _G.terminal))
                          (key [mod] :space #(awful.spawn _G.menu))
                          (key [mod] :j #(awful.client.focus.byidx 1))
                          (key [mod] :k #(awful.client.focus.byidx -1))
                          (key [mod :Shift] :j #(awful.client.swap.byidx 1))
                          (key [mod :Shift] :k #(awful.client.swap.byidx -1))
                          (key [mod] :h #(awful.tag.incmwfact -0.02))
                          (key [mod] :l #(awful.tag.incmwfact 0.02))
                          (key [mod :Shift] :h
                               #(awful.tag.incnmaster 1 nil true))
                          (key [mod :Shift] :l
                               #(awful.tag.incnmaster -1 nil true))
                          (key [mod] :Tab #(awful.layout.inc 1)))
        :clientkeys (join (key [mod] :w #($1:kill))
                          (key [mod] :f
                               #(do
                                  (set $1.fullscreen (not $1.fullscreen))
                                  ($1:raise)))
                          (key [mod] :t #(set $1.floating (not $1.floating)))
                          (key [mod] :m #(set $1.maximized (not $1.maximized))))
        :clientbuttons (join (button [] 1
                                     #($1:emit_signal "request::activate"
                                                      :mouse_click {:raise true}))
                             (button [mod] 1
                                     #(do
                                        ($1:emit_signal "request::activate"
                                                        :mouse_click
                                                        {:raise true})
                                        (awful.mouse.client.move $1)))
                             (button [mod] 3
                                     #(do
                                        ($1:emit_signal "request::activate"
                                                        :mouse_click
                                                        {:raise true})
                                        (awful.mouse.client.resize $1))))})

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

M
