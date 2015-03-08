{-# LANGUAGE OverloadedStrings #-}
module Language  where

import Data.Aeson.Types
import Control.Applicative ((<$>))

newtype Language = Language String

defaultLanguage :: Language
defaultLanguage = Language "en"

ppLang :: Language -> String
ppLang (Language l) = l

instance ToJSON Language where
    toJSON = toJSON . ppLang

instance FromJSON Language where
    parseJSON s = Language <$> parseJSON s

