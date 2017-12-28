module TipToiYamlAux where

import Data.Aeson.Types
import Data.Char

gameYamlOptions = defaultOptions
    { fieldLabelModifier = map fix . map toLower . drop 2
    , allNullaryToStringTag = True
    }
       where fix '_' = '-'
             fix c   = c
