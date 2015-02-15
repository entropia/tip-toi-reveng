{-# LANGUAGE OverloadedStrings #-}
module Language where

import Data.Aeson.Types
import Control.Monad
import Data.Text


data Language = En | De | Fr

defaultLanguage :: Language
defaultLanguage = En

ppLang :: Language -> String
ppLang En = "en"
ppLang De = "de"
ppLang Fr = "fr"

instance ToJSON Language where
    toJSON = toJSON . ppLang

instance FromJSON Language where
    parseJSON (String "en") = return En
    parseJSON (String "de") = return De
    parseJSON (String "fr") = return Fr
    parseJSON (String s) = fail $ "Unknown language \"" ++ unpack s ++ "\"."
    parseJSON _ = mzero

