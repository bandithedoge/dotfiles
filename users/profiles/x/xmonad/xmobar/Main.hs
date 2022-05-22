import XMonad.Hooks.StatusBar.PP
import Xmobar

import Rice

config :: Rice -> Config
config rice =
  defaultConfig
    { font = "xft:" ++ uiFont rice ++ "-11"
    , additionalFonts =
        [ "xft:" ++ monoFont rice ++ "-9"
        , "xft:Material Design Icons-14"
        ]
    , position = TopH 24
    , textOffset = 17
    , textOffsets = [16, 19]
    , bgColor = base01 rice
    , fgColor = fg
    , commands =
        [ Run XMonadLog
        , Run $ Cpu ["-t", "<total>%"] 10
        , Run $
            Battery
              ( concat
                  [ ["-t", "<acstatus> <left>% (<timeleft>)"]
                  , ["--"]
                  , ["-O", icon "\xF06A5"]
                  , ["-i", icon "\xF06A5"]
                  , ["-o", ""]
                  ]
              )
              30
        , Run $
            CoreTemp
              ( ["-t", "<core0>°C"]
                  ++ ["-h", base08 rice]
              )
              10
        , Run $ Memory ["-t", "<used> MB"] 10
        , Run $ Date "%T" "date" 10
        ]
    , template =
        concat
          [ "%XMonadLog% }{ "
          , mod "battery" "\xF0079"
          , mod "coretemp" "\xF050F"
          , mod "memory" "\xF035B"
          , mod "cpu" "\xF061A"
          , mod "date" "\xF0954"
          ]
    }
  where
    bg = base02 rice
    fg = base04 rice
    icon = xmobarFont 2
    mod a i =
      pad $
        xmobarBorder "VBoth" bg 3 $
          xmobarColor fg bg $
            icon i
              ++ pad (wrap "%" "%" a)

main :: IO ()
main = do
  rice <- getRice
  xmobar $ config rice
