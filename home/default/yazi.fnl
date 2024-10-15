(: (require :eza-preview) :setup)

(: (require :yatline) :setup
   {:section_separator {:open "" :close ""}
    :part_separator {:open "" :close ""}
    :inverse_separator {:open "" :close ""}
    :style_a {:fg _G.base00
              :bg_mode {:normal _G.base0F :select _G.base0A :un_set _G.base08}}
    :style_b {:fg _G.base05 :bg _G.base02}
    :style_c {:fg _G.base03 :bg _G.base02}
    :permissions_t_fg _G.base0F
    :permissions_r_fg _G.base0B
    :permissions_w_fg _G.base09
    :permissions_x_fg _G.base0C
    :permissions_s_fg _G.base03
    :show_background true
    :header_line {:left {:section_a [{:type :line
                                      :custom false
                                      :name :tabs
                                      :params [:left]}]
                         :section_b [{:type :string
                                      :custom false
                                      :name :tab_path}]
                         :section_c [{:type :coloreds
                                      :custom false
                                      :name :count}]}
                  :right {:section_a []
                          :section_b [{:type :coloreds
                                       :custom false
                                       :name :task_states}]
                          :section_c [{:type :string
                                       :custom false
                                       :name :tab_num_files}]}}
    :status_line {:left {:section_a [{:type :string
                                      :custom false
                                      :name :tab_mode}]
                         :section_b [{:type :string
                                      :custom false
                                      :name :hovered_path}]
                         :section_c [{:type :string
                                      :custom false
                                      :name :hovered_size}]}
                  :right {:section_a [{:type :string
                                       :custom false
                                       :name :cursor_position}
                                      {:type :string
                                       :custom false
                                       :name :cursor_percentage}]
                          :section_b [{:type :coloreds
                                       :custom false
                                       :name :permissions}]
                          :section_c [{:type :string
                                       :custom false
                                       :name :hovered_mime}]}}})

