-- vim: fdm=marker
import XMonad
import System.Exit (exitSuccess)
import Data.Tree
import Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.TreeSelect
import XMonad.Actions.WindowNavigation

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed

import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders

import XMonad.Prompt
import XMonad.Prompt.XMonad

-- variables {{{
myTerm :: String
myTerm = "kitty"

myModMask :: KeyMask
myModMask = mod4Mask

myFont :: String
myFont = "xft:Roboto Condensed-12"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#20222c"
myFocusedBorderColor = "#4b74ad"
-- }}}

-- theme {{{
myTheme = def {
    fontName = myFont,
    activeColor = accent0,
    activeTextColor = accent1,
    activeBorderColor = accent,
    activeBorderWidth = 0,
    inactiveColor = bg0,
    inactiveTextColor = fg,
    inactiveBorderColor = bg,
    inactiveBorderWidth = 2,
    urgentColor = red0,
    urgentTextColor = red1,
    urgentBorderColor = red,
    urgentBorderWidth = 0 
} where
    bg = "#20222c"
    bg0 = "#181a23"
    bg1 = "#262935"
    bg2 = "#2c2f3d"
    fg = "#d9dceb"
    fg0 = "#e7ebff"
    fg1 = "#cccfdf"
    fg2 = "#c7cadc"
    selection = "#7b7e8c"
    comment = "#747dab"
    accent = "#4b74ad"
    accent0 = "#40587a"
    accent1 = "#8eb9f5"
    red = "#eb585f"
    red0 = "#8b2f33"
    red1 = "#f28b90"
    purple = "#b96be1"
    purple0 = "#8f41b7"
    purple1 = "#d49af2"
    pink = "#e871e1"
    pink0 = "#9b4996"
    pink1 = "#edabe9"
    blue = "#4e5fc9"
    blue0 = "#283275"
    blue1 = "#96a3f2"
    cyan = "#52e6da"
    cyan0 = "#378680"
    cyan1 = "#8ffff4"
    green = "#69d26e"
    green0 = "#357a38"
    green1 = "#96f29b"
    yellow = "#e1c85c"
    yellow0 = "#9f8c39"
    yellow1 = "#f7e7a1"
    orange = "#d7953f"
    orange0 = "#9c6e31"
    orange1 = "#edc186"
-- }}}

-- layouts {{{
myLayoutHook =
    tall ||| bsp ||| tabs
    where
        gaps = spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True

        tall = renamed [Replace "TLL"] $ gaps $ Tall 1 (3/100) (1/2)
        bsp = renamed [Replace "BSP"] $ gaps $ emptyBSP
        tabs = renamed [Replace "TAB"] $ noBorders $ tabbedBottom shrinkText myTheme
-- }}}

-- bar {{{
myXmobarPP :: PP
myXmobarPP = def {
    ppSep = xmobarColor comment "" " ",
    ppCurrent = xmobarColor accent1 accent0full . xmobarBorder "Full" bg0 2 . pad . pad,
    ppHidden = xmobarColor fg bg0full . xmobarBorder "Full" bg0 2 . pad . pad,
    ppLayout = xmobarColor fg bg0full . xmobarFont 1 . xmobarBorder "Full" bg0 2 . pad,
    ppOrder = \[ws, l, t] -> [l, ws, t]
} where
    formatFocused = xmobarColor fg bg2 . wrap "<box width=2 color=#20222c> " " </box>"
    formatUnfocused = xmobarColor fg bg0 . wrap "<box width=2 color=#20222c> " " </box>"

    comment, accent, accent0, accent1 :: String
    comment = "#747dab"
    accent = "#4b74ad"
    accent0 = "#40587a"
    accent0full = "#40587a:0"
    accent1 = "#8eb9f5"
    bg = "#20222c"
    bg0 = "#181a23"
    bg0full = "#181a23:0"
    bg1 = "#262935"
    bg2 = "#2c2f3d"
    fg = "#d9dceb"
-- }}}

-- keybindings {{{
myKeys :: [(String, X())]
myKeys =
    [ ("M-C-r", spawn "xmonad --restart")
    , ("M-C-q", io exitSuccess)

    , ("M-w", kill1)
    , ("M-<Tab>", sendMessage $ NextLayout)

    , ("M-<Return>", spawn (myTerm))
    , ("M-<Space>", spawn "rofi -show drun")

    , ("M-<Backspace> b", spawn "qutebrowser")
    ]
-- }}}

-- main {{{
myConfig = def
    { borderWidth = myBorderWidth
    , terminal = myTerm
    , modMask = myModMask
    , layoutHook = myLayoutHook
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    } `additionalKeysP` myKeys

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig
    where
        toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
        toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)
-- }}}
