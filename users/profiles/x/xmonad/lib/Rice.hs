{-# LANGUAGE DeriveGeneric #-}

-- {{{
module Rice (
  Rice,
  getRice,
  base00,
  base01,
  base02,
  base03,
  base04,
  base05,
  base06,
  base07,
  base08,
  base09,
  base0A,
  base0B,
  base0C,
  base0D,
  base0E,
  base0F,
  base10,
  base11,
  base12,
  base13,
  base14,
  base15,
  base16,
  base17,
  monoFont,
  uiFont,
  terminal,
  wm,
  menu,
) where -- }}}

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import GHC.Generics
import System.Environment.XDG.BaseDir

data Rice = Rice
  { base00, base01, base02, base03, base04, base05, base06, base07, base08, base09, base0A, base0B, base0C, base0D, base0E, base0F :: String
  , base10, base11, base12, base13, base14, base15, base16, base17 :: String
  , monoFont, uiFont :: String
  , terminal, wm, menu :: String
  }
  deriving (Generic, Show)

instance FromJSON Rice

getRice :: IO Rice
getRice = do
  file <- getUserConfigFile "" "rice.json"
  input <- B.readFile file
  let rice = decode input :: Maybe Rice

  case rice of
    Just rice -> return rice
