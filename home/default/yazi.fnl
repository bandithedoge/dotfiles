(: (require :eza-preview) :setup)

(: (require :yatline) :setup
   {:section_separator {:open "" :close ""}
    :part_separator {:open "" :close ""}
    :inverse_separator {:open "" :close ""}
    :style_a {:fg :black
              :bg_mode {:normal :gray :select :yellow :un_set :red}}
    :header_line {:left {:section_a [{:type :line
                                      :custom false
                                      :name :tabs
                                      :params [:left]}]
                         :section_b []
                         :section_c []}
                  :right {:section_a [] :section_b [] :section_c []}}})

nil

