module Main where

import Data.Colour.SRGB
import Data.Default (def)
import qualified Data.Text as T

import System.Taffybar
import qualified System.Taffybar.Context as C
import System.Taffybar.Hooks
import qualified System.Taffybar.SimpleConfig as S

import System.Taffybar.Widget
import System.Taffybar.Widget.Battery
import System.Taffybar.Widget.Generic.PollingBar
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.Layout

import System.Taffybar.Information.CPU2
import System.Taffybar.Information.Memory

import Rice

type RGBA = (Double, Double, Double, Double)

colorWithAlpha :: Double -> String -> RGBA
colorWithAlpha alpha col = (channelRed rgb, channelGreen rgb, channelBlue rgb, alpha)
  where
    rgb = toSRGB $ sRGB24read col

color :: String -> RGBA
color = colorWithAlpha 1

myBarConfig :: Rice -> BarConfig
myBarConfig rice =
  BarConfig
    { barColor = const (1, 1, 1)
    , barBorderColor = (1, 1, 1)
    , barBackgroundColor = const (0, 0, 0)
    , barWidth = 10
    , barDirection = VERTICAL
    , barPadding = 10
    }

myGraphConfig :: Rice -> String -> GraphConfig
myGraphConfig rice label =
  def
    { graphDataColors = [color $ base0F rice]
    , graphBackgroundColor = color $ base02 rice
    , graphBorderWidth = 0
    , graphDirection = RIGHT_TO_LEFT
    , graphLabel =
        Just $
          T.pack
            ( "<span font_size='140%' font_family='" ++ monoFont rice ++ "'>"
                ++ label
                ++ "</span>"
            )
    }

memCallback :: IO ([Double], Maybe Text)
memCallback = do
  info <- parseMeminfo
  return
    ( [memoryUsedRatio info, memorySwapUsedRatio info]
    , Just $
        T.pack $
          "swap: " ++ show (memorySwapUsed info)
            ++ "\nmemory: "
            ++ show (memoryUsed info)
    )

tempCallback :: IO ([Double], Maybe Text)
tempCallback = do
  info <- getCPUTemperatures
  return
    ( [snd x / 100 | x <- info]
    , Just $ T.pack $ intercalate "\n" $ sort [fst x ++ ": " ++ show (snd x) ++ "°C" | x <- info]
    )

myConfig :: Rice -> S.SimpleTaffyConfig
myConfig rice =
  def
    { S.startWidgets =
        [ System.Taffybar.Widget.Layout.layoutNew defaultLayoutConfig
        , workspacesNew
            defaultWorkspacesConfig
              { showWorkspaceFn = hideEmpty
              , borderWidth = 0
              }
        , windowsNew defaultWindowsConfig
        ]
    , S.endWidgets =
        [ textClockNewWith
            def
              { clockFormatString = "%T"
              , clockUpdateStrategy = ConstantInterval 1
              }
        , textClockNewWith def {clockFormatString = "%A %d %B"}
        , pollingGraphNew (myGraphConfig rice "\xFB19") 1 $ getCPULoad "cpu"
        , pollingGraphNewWithTooltip (myGraphConfig rice "\xF85A") 1 memCallback
        , pollingGraphNewWithTooltip (myGraphConfig rice "\xFA0E") 1 tempCallback
        , textBatteryNew "$percentage$%"
        , batteryIconNew
        , sniTrayNew
        ]
    , S.barHeight = S.ExactSize 25
    , S.barPadding = 0
    , S.widgetSpacing = 10
    }

main = do
  rice <- getRice
  startTaffybar $ withBatteryRefresh $ S.toTaffyConfig $ myConfig rice
