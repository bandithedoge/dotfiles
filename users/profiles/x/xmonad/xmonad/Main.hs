module Main where

import Data.Tree
import Data.Word
import Text.Read (read)

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.TreeSelect

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.TaffybarPagerHints

import XMonad.Layout.Dwindle
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)

import XMonad.Util.EntryHelper (withHelper)
import XMonad.Util.EntryHelper.Util (sendRestart)

import Rice
import XMonad.Util.SpawnOnce (spawnOnce)

-- layouts {{{
myLayout = avoidStruts (tall ||| Full)
  where
    tall = renamed [Replace "\xF0BCC"] $ gaps $ ResizableTall 1 (3 / 100) 0.5 []
    tabbed = renamed [Replace "\xF1888"] simpleTabbed

    gaps = spacingWithEdge 5

-- }}}

-- keybindings {{{
myKeys :: Rice -> [(String, X ())]
myKeys rice =
  [ ("M-C-r", restart "my-xmonad" True)
  , ("M-C-q", io exitSuccess)
  , ("M-<Return>", spawn "kitty")
  , ("M-w", kill)
  , -- layout manipulation
    ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-S-h", sendMessage $ IncMasterN 1)
  , ("M-s-l", sendMessage $ IncMasterN (-1))
  , ("M-<Tab>", sendMessage NextLayout)
  , ("M-t", withFocused $ windows . W.sink)
  ]
    -- switch to workspace
    ++ [ ("M-" ++ show x, windows $ W.greedyView (show x))
       | x <- [1 .. 9]
       ]
    -- move to workspace
    ++ [ ("M-S-" ++ show x, windows $ W.shift (show x))
       | x <- [1 .. 9]
       ]

-- }}}

-- startup hook {{{
myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawn "killall my-taffybar"
  spawn "my-taffybar"

-- }}}

oldMain :: IO ()
oldMain = do
  rice <- getRice
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    . pagerHints
    $ def
      { modMask = mod4Mask
      , layoutHook = myLayout
      , startupHook = myStartupHook
      , borderWidth = 2
      , normalBorderColor = base00 rice
      , focusedBorderColor = base0F rice
      , keys = (`mkKeymap` myKeys rice)
      }

main :: IO ()
main = withHelper oldMain
