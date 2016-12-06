{-# LANGUAGE RecordWildCards, DeriveGeneric, CPP, TupleSections, TemplateHaskell #-}

module TipToiYaml
    ( tt2ttYaml, ttYaml2tt
    , readTipToiYaml, writeTipToiYaml,writeTipToiCodeYaml
    , ttyProduct_Id
    , debugGame
    )
where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import System.Exit
import System.FilePath
import Text.Printf
import Data.Char
import Data.Either
import Data.Functor
import Data.Maybe
import Control.Monad
import System.Directory
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Monad.Writer.Strict
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import Data.Time (getCurrentTime, formatTime)
import Data.Yaml hiding ((.=), Parser)
import Data.Aeson.Types hiding ((.=), Parser)
import Data.Aeson.TH
import Text.Parsec hiding (Line, lookAhead, spaces)
import Text.Parsec.String
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import GHC.Generics
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Traversable (for, traverse)
import Control.Arrow
import Control.Applicative (Applicative(..), (<*>), (<*))

import TextToSpeech
import Language
import Types
import Constants
import KnownCodes
import PrettyPrint
import OneLineParser
import Utils
import TipToiYamlAux

data TipToiYAML = TipToiYAML
    { ttyScripts     :: M.Map String [String]
    , ttyComment     :: Maybe String
    , ttyGME_Lang    :: Maybe String
    , ttyMedia_Path  :: Maybe String
    , ttyInit        :: Maybe String
    , ttyWelcome     :: Maybe String
    , ttyProduct_Id  :: Word32
    , ttyScriptCodes :: Maybe CodeMap
    , ttySpeak       :: Maybe SpeakSpecs
    , ttyLanguage    :: Maybe Language
    , ttyGames       :: Maybe [GameYaml]
    }
    deriving Generic

data TipToiCodesYAML = TipToiCodesYAML
    { ttcScriptCodes :: CodeMap
    }
    deriving Generic

data SpeakSpec = SpeakSpec
    { ssLanguage :: Maybe Language
    , ssSpeak    :: M.Map String String
    }

instance FromJSON SpeakSpec where
    parseJSON v = do
        m <- parseJSON v
        l <- T.traverse parseJSON  $ M.lookup "language" m
        m' <- T.traverse parseJSON $ M.delete "language" m
        return $ SpeakSpec l m'

instance ToJSON SpeakSpec where
    toJSON (SpeakSpec (Just l) m) = toJSON $ M.insert "language" (ppLang l) m
    toJSON (SpeakSpec Nothing m)  = toJSON $ m

toSpeakMap :: Language -> Maybe SpeakSpecs -> M.Map String (Language, String)
toSpeakMap l Nothing = M.empty
toSpeakMap l (Just (SpeakSpecs specs)) = M.unionsWith e $ map go specs
  where
    go (SpeakSpec ml m) = M.map ((l',)) m
      where l' = fromMaybe l ml
    e = error "Conflicting definitions in section \"speak\""


newtype SpeakSpecs = SpeakSpecs [SpeakSpec]

instance FromJSON SpeakSpecs where
    parseJSON (Array a) = SpeakSpecs <$> mapM parseJSON  (V.toList a)
    parseJSON v = SpeakSpecs . (:[]) <$> parseJSON v

instance ToJSON SpeakSpecs where
    toJSON (SpeakSpecs [x]) = toJSON x
    toJSON (SpeakSpecs l)   = Array $ V.fromList $ map toJSON $ l

type PlayListListYaml = String

type OIDListYaml = String

data GameYaml = CommonGameYaml
        { gyGameType                 :: Word16
        , gyRounds                   :: Word16
        , gyUnknownC                 :: Word16
        , gyEarlyRounds              :: Word16
        , gyRepeatLastMedia          :: Word16
        , gyUnknownX                 :: Word16
        , gyUnknownW                 :: Word16
        , gyUnknownV                 :: Word16
        , gyStartPlayList            :: PlayListListYaml
        , gyRoundEndPlayList         :: PlayListListYaml
        , gyFinishPlayList           :: PlayListListYaml
        , gyRoundStartPlayList       :: PlayListListYaml
        , gyLaterRoundStartPlayList  :: PlayListListYaml
        , gySubgames                 :: [SubGameYaml]
        , gyTargetScores             :: [Word16]
        , gyFinishPlayLists          :: [PlayListListYaml]
        }
    | Game6Yaml
        { gyRounds                   :: Word16
        , gyBonusSubgameCount        :: Word16
        , gyBonusRounds              :: Word16
        , gyBonusTarget              :: Word16
        , gyUnknownI                 :: Word16
        , gyEarlyRounds              :: Word16
        , gyUnknownQ                 :: Word16
        , gyRepeatLastMedia          :: Word16
        , gyUnknownX                 :: Word16
        , gyUnknownW                 :: Word16
        , gyUnknownV                 :: Word16
        , gyStartPlayList            :: PlayListListYaml
        , gyRoundEndPlayList         :: PlayListListYaml
        , gyFinishPlayList           :: PlayListListYaml
        , gyRoundStartPlayList       :: PlayListListYaml
        , gyLaterRoundStartPlayList  :: PlayListListYaml
        , gyRoundStartPlayList2      :: PlayListListYaml
        , gyLaterRoundStartPlayList2 :: PlayListListYaml
        , gySubgames                 :: [SubGameYaml]
        , gyTargetScores             :: [Word16]
        , gyBonusTargetScores        :: [Word16]
        , gyFinishPlayLists          :: [PlayListListYaml]
        , gyBonusFinishPlayLists     :: [PlayListListYaml]
        , gyBonusSubgameIds          :: [Word16]
        }
    | Game7Yaml
        { gyRounds                   :: Word16
        , gyUnknownC                 :: Word16
        , gyEarlyRounds              :: Word16
        , gyRepeatLastMedia          :: Word16
        , gyUnknownX                 :: Word16
        , gyUnknownW                 :: Word16
        , gyUnknownV                 :: Word16
        , gyStartPlayList            :: PlayListListYaml
        , gyRoundEndPlayList         :: PlayListListYaml
        , gyFinishPlayList           :: PlayListListYaml
        , gyRoundStartPlayList       :: PlayListListYaml
        , gyLaterRoundStartPlayList  :: PlayListListYaml
        , gySubgames                 :: [SubGameYaml]
        , gyTargetScores             :: [Word16]
        , gyFinishPlayLists          :: [PlayListListYaml]
        , gySubgameGroups            :: [[GameId]]
        }
    | Game8Yaml
        { gyRounds                   :: Word16
        , gyUnknownC                 :: Word16
        , gyEarlyRounds              :: Word16
        , gyRepeatLastMedia          :: Word16
        , gyUnknownX                 :: Word16
        , gyUnknownW                 :: Word16
        , gyUnknownV                 :: Word16
        , gyStartPlayList            :: PlayListListYaml
        , gyRoundEndPlayList         :: PlayListListYaml
        , gyFinishPlayList           :: PlayListListYaml
        , gyRoundStartPlayList       :: PlayListListYaml
        , gyLaterRoundStartPlayList  :: PlayListListYaml
        , gySubgames                 :: [SubGameYaml]
        , gyTargetScores             :: [Word16]
        , gyFinishPlayLists          :: [PlayListListYaml]
        , gyGameSelectOIDs           :: OIDListYaml
        , gyGameSelect               :: [Word16]
        , gyGameSelectErrors1        :: PlayListListYaml
        , gyGameSelectErrors2        :: PlayListListYaml
        }
    | Game9Yaml
        { gyRounds                   :: Word16
        , gyUnknownC                 :: Word16
        , gyEarlyRounds              :: Word16
        , gyRepeatLastMedia          :: Word16
        , gyUnknownX                 :: Word16
        , gyUnknownW                 :: Word16
        , gyUnknownV                 :: Word16
        , gyStartPlayList            :: PlayListListYaml
        , gyRoundEndPlayList         :: PlayListListYaml
        , gyFinishPlayList           :: PlayListListYaml
        , gyRoundStartPlayList       :: PlayListListYaml
        , gyLaterRoundStartPlayList  :: PlayListListYaml
        , gySubgames                 :: [SubGameYaml]
        , gyTargetScores             :: [Word16]
        , gyFinishPlayLists          :: [PlayListListYaml]
        , gyExtraPlayLists           :: [PlayListListYaml]
        }
    | Game10Yaml
        { gyRounds                   :: Word16
        , gyUnknownC                 :: Word16
        , gyEarlyRounds              :: Word16
        , gyRepeatLastMedia          :: Word16
        , gyUnknownX                 :: Word16
        , gyUnknownW                 :: Word16
        , gyUnknownV                 :: Word16
        , gyStartPlayList            :: PlayListListYaml
        , gyRoundEndPlayList         :: PlayListListYaml
        , gyFinishPlayList           :: PlayListListYaml
        , gyRoundStartPlayList       :: PlayListListYaml
        , gyLaterRoundStartPlayList  :: PlayListListYaml
        , gySubgames                 :: [SubGameYaml]
        , gyTargetScores             :: [Word16]
        , gyFinishPlayLists          :: [PlayListListYaml]
        , gyExtraPlayLists           :: [PlayListListYaml]
        }
    | Game16Yaml
        { gyRounds                   :: Word16
        , gyUnknownC                 :: Word16
        , gyEarlyRounds              :: Word16
        , gyRepeatLastMedia          :: Word16
        , gyUnknownX                 :: Word16
        , gyUnknownW                 :: Word16
        , gyUnknownV                 :: Word16
        , gyStartPlayList            :: PlayListListYaml
        , gyRoundEndPlayList         :: PlayListListYaml
        , gyFinishPlayList           :: PlayListListYaml
        , gyRoundStartPlayList       :: PlayListListYaml
        , gyLaterRoundStartPlayList  :: PlayListListYaml
        , gySubgames                 :: [SubGameYaml]
        , gyTargetScores             :: [Word16]
        , gyFinishPlayLists          :: [PlayListListYaml]
        , gyExtraOIDs                :: OIDListYaml
        , gyExtraPlayLists           :: [PlayListListYaml]
        }
    | Game253Yaml

data SubGameYaml = SubGameYaml
    { sgUnknown :: String
    , sgOids1 :: OIDListYaml
    , sgOids2 :: OIDListYaml
    , sgOids3 :: OIDListYaml
    , sgPlaylist :: [PlayListListYaml]
    }


$(deriveJSON gameYamlOptions ''GameYaml)
$(deriveJSON gameYamlOptions ''SubGameYaml)

tipToiYamlOptions = defaultOptions
    { fieldLabelModifier = map fix . map toLower . drop 3
    , omitNothingFields  = True
    }
       where fix '_' = '-'
             fix c   = c

instance FromJSON TipToiYAML where
    parseJSON = genericParseJSON tipToiYamlOptions
instance ToJSON TipToiYAML where
    toJSON = genericToJSON tipToiYamlOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding = genericToEncoding tipToiYamlOptions
#endif
instance FromJSON TipToiCodesYAML where
    parseJSON = genericParseJSON tipToiYamlOptions
instance ToJSON TipToiCodesYAML where
    toJSON = genericToJSON tipToiYamlOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding = genericToEncoding tipToiYamlOptions
#endif


tt2ttYaml :: String -> TipToiFile -> TipToiYAML
tt2ttYaml path (TipToiFile {..}) = TipToiYAML
    { ttyProduct_Id = ttProductId
    , ttyInit = Just $ spaces $ [ ppCommand True M.empty [] (ArithOp Set (RegPos r) (Const n))
                                | (r,n) <- zip [0..] ttInitialRegs , n /= 0]
    , ttyWelcome = Just $ playListList2Yaml ttWelcome
    , ttyComment = Just $ BC.unpack ttComment
    , ttyGME_Lang = if BC.null ttLang then Nothing else Just (BC.unpack ttLang)
    , ttyScripts = M.fromList
        [ (show oid, map exportLine ls) | (oid, Just ls) <- ttScripts]
    , ttyMedia_Path = Just path
    , ttyScriptCodes = Nothing
    , ttySpeak = Nothing
    , ttyLanguage = Nothing
    , ttyGames = list2Maybe $ map game2gameYaml ttGames
    }

list2Maybe [] = Nothing
list2Maybe xs = Just xs

playListList2Yaml :: PlayListList -> PlayListListYaml
playListList2Yaml = commas . map show . concat

oidList2Yaml :: [OID] -> OIDListYaml
oidList2Yaml = unwords . map show

subGame2Yaml :: SubGame -> SubGameYaml
subGame2Yaml (SubGame u o1 o2 o3 pl) = SubGameYaml
    { sgUnknown = prettyHex u
    , sgOids1 = oidList2Yaml o1
    , sgOids2 = oidList2Yaml o2
    , sgOids3 = oidList2Yaml o3
    , sgPlaylist = map playListList2Yaml pl
    }

game2gameYaml :: Game -> GameYaml
game2gameYaml CommonGame {..} = CommonGameYaml
        { gyGameType                 = gGameType
        , gyRounds                   = gRounds
        , gyUnknownC                 = gUnknownC
        , gyEarlyRounds              = gEarlyRounds
        , gyRepeatLastMedia          = gRepeatLastMedia
        , gyUnknownX                 = gUnknownX
        , gyUnknownW                 = gUnknownW
        , gyUnknownV                 = gUnknownV
        , gyStartPlayList            = playListList2Yaml gStartPlayList
        , gyRoundEndPlayList         = playListList2Yaml gRoundEndPlayList
        , gyFinishPlayList           = playListList2Yaml gFinishPlayList
        , gyRoundStartPlayList       = playListList2Yaml gRoundStartPlayList
        , gyLaterRoundStartPlayList  = playListList2Yaml gLaterRoundStartPlayList
        , gySubgames                 = map subGame2Yaml gSubgames
        , gyTargetScores             = gTargetScores
        , gyFinishPlayLists          = map playListList2Yaml gFinishPlayLists
        }
game2gameYaml Game6 {..} = Game6Yaml
        { gyRounds                   = gRounds
        , gyBonusSubgameCount        = gBonusSubgameCount
        , gyBonusRounds              = gBonusRounds
        , gyBonusTarget              = gBonusTarget
        , gyUnknownI                 = gUnknownI
        , gyEarlyRounds              = gEarlyRounds
        , gyUnknownQ                 = gUnknownQ
        , gyRepeatLastMedia          = gRepeatLastMedia
        , gyUnknownX                 = gUnknownX
        , gyUnknownW                 = gUnknownW
        , gyUnknownV                 = gUnknownV
        , gyStartPlayList            = playListList2Yaml gStartPlayList
        , gyRoundEndPlayList         = playListList2Yaml gRoundEndPlayList
        , gyFinishPlayList           = playListList2Yaml gFinishPlayList
        , gyRoundStartPlayList       = playListList2Yaml gRoundStartPlayList
        , gyLaterRoundStartPlayList  = playListList2Yaml gLaterRoundStartPlayList
        , gyRoundStartPlayList2      = playListList2Yaml gRoundStartPlayList2
        , gyLaterRoundStartPlayList2 = playListList2Yaml gLaterRoundStartPlayList2
        , gySubgames                 = map subGame2Yaml gSubgames
        , gyTargetScores             = gTargetScores
        , gyBonusTargetScores        = gBonusTargetScores
        , gyFinishPlayLists          = map playListList2Yaml gFinishPlayLists
        , gyBonusFinishPlayLists     = map playListList2Yaml gBonusFinishPlayLists
        , gyBonusSubgameIds          = gBonusSubgameIds
        }
game2gameYaml Game7 {..} = Game7Yaml
        { gyRounds                   = gRounds
        , gyUnknownC                 = gUnknownC
        , gyEarlyRounds              = gEarlyRounds
        , gyRepeatLastMedia          = gRepeatLastMedia
        , gyUnknownX                 = gUnknownX
        , gyUnknownW                 = gUnknownW
        , gyUnknownV                 = gUnknownV
        , gyStartPlayList            = playListList2Yaml gStartPlayList
        , gyRoundEndPlayList         = playListList2Yaml gRoundEndPlayList
        , gyFinishPlayList           = playListList2Yaml gFinishPlayList
        , gyRoundStartPlayList       = playListList2Yaml gRoundStartPlayList
        , gyLaterRoundStartPlayList  = playListList2Yaml gLaterRoundStartPlayList
        , gySubgames                 = map subGame2Yaml gSubgames
        , gyTargetScores             = gTargetScores
        , gyFinishPlayLists          = map playListList2Yaml gFinishPlayLists
        , gySubgameGroups            = gSubgameGroups
        }
game2gameYaml Game8 {..} = Game8Yaml
        { gyRounds                   = gRounds
        , gyUnknownC                 = gUnknownC
        , gyEarlyRounds              = gEarlyRounds
        , gyRepeatLastMedia          = gRepeatLastMedia
        , gyUnknownX                 = gUnknownX
        , gyUnknownW                 = gUnknownW
        , gyUnknownV                 = gUnknownV
        , gyStartPlayList            = playListList2Yaml gStartPlayList
        , gyRoundEndPlayList         = playListList2Yaml gRoundEndPlayList
        , gyFinishPlayList           = playListList2Yaml gFinishPlayList
        , gyRoundStartPlayList       = playListList2Yaml gRoundStartPlayList
        , gyLaterRoundStartPlayList  = playListList2Yaml gLaterRoundStartPlayList
        , gySubgames                 = map subGame2Yaml gSubgames
        , gyTargetScores             = gTargetScores
        , gyFinishPlayLists          = map playListList2Yaml gFinishPlayLists
        , gyGameSelectOIDs           = oidList2Yaml gGameSelectOIDs
        , gyGameSelect               = gGameSelect
        , gyGameSelectErrors1        = playListList2Yaml gGameSelectErrors1
        , gyGameSelectErrors2        = playListList2Yaml gGameSelectErrors2
        }
game2gameYaml Game9 {..} = Game9Yaml
        { gyRounds                   = gRounds
        , gyUnknownC                 = gUnknownC
        , gyEarlyRounds              = gEarlyRounds
        , gyRepeatLastMedia          = gRepeatLastMedia
        , gyUnknownX                 = gUnknownX
        , gyUnknownW                 = gUnknownW
        , gyUnknownV                 = gUnknownV
        , gyStartPlayList            = playListList2Yaml gStartPlayList
        , gyRoundEndPlayList         = playListList2Yaml gRoundEndPlayList
        , gyFinishPlayList           = playListList2Yaml gFinishPlayList
        , gyRoundStartPlayList       = playListList2Yaml gRoundStartPlayList
        , gyLaterRoundStartPlayList  = playListList2Yaml gLaterRoundStartPlayList
        , gySubgames                 = map subGame2Yaml gSubgames
        , gyTargetScores             = gTargetScores
        , gyFinishPlayLists          = map playListList2Yaml gFinishPlayLists
        , gyExtraPlayLists           = map playListList2Yaml gExtraPlayLists
        }
game2gameYaml Game10 {..} = Game10Yaml
        { gyRounds                   = gRounds
        , gyUnknownC                 = gUnknownC
        , gyEarlyRounds              = gEarlyRounds
        , gyRepeatLastMedia          = gRepeatLastMedia
        , gyUnknownX                 = gUnknownX
        , gyUnknownW                 = gUnknownW
        , gyUnknownV                 = gUnknownV
        , gyStartPlayList            = playListList2Yaml gStartPlayList
        , gyRoundEndPlayList         = playListList2Yaml gRoundEndPlayList
        , gyFinishPlayList           = playListList2Yaml gFinishPlayList
        , gyRoundStartPlayList       = playListList2Yaml gRoundStartPlayList
        , gyLaterRoundStartPlayList  = playListList2Yaml gLaterRoundStartPlayList
        , gySubgames                 = map subGame2Yaml gSubgames
        , gyTargetScores             = gTargetScores
        , gyFinishPlayLists          = map playListList2Yaml gFinishPlayLists
        , gyExtraPlayLists           = map playListList2Yaml gExtraPlayLists
        }
game2gameYaml Game16 {..} = Game16Yaml
        { gyRounds                   = gRounds
        , gyUnknownC                 = gUnknownC
        , gyEarlyRounds              = gEarlyRounds
        , gyRepeatLastMedia          = gRepeatLastMedia
        , gyUnknownX                 = gUnknownX
        , gyUnknownW                 = gUnknownW
        , gyUnknownV                 = gUnknownV
        , gyStartPlayList            = playListList2Yaml gStartPlayList
        , gyRoundEndPlayList         = playListList2Yaml gRoundEndPlayList
        , gyFinishPlayList           = playListList2Yaml gFinishPlayList
        , gyRoundStartPlayList       = playListList2Yaml gRoundStartPlayList
        , gyLaterRoundStartPlayList  = playListList2Yaml gLaterRoundStartPlayList
        , gySubgames                 = map subGame2Yaml gSubgames
        , gyTargetScores             = gTargetScores
        , gyFinishPlayLists          = map playListList2Yaml gFinishPlayLists
        , gyExtraOIDs                = oidList2Yaml gExtraOIDs
        , gyExtraPlayLists           = map playListList2Yaml gExtraPlayLists
        }
game2gameYaml Game253 = Game253Yaml

playListListFromYaml :: PlayListListYaml -> WithFileNames PlayListList
playListListFromYaml =
    fmap listify .
    traverse recordFilename .
    either error id .
    parseOneLinePure parsePlayList "playlist"
  where listify [] = []
        listify x  = [x]

oidListFromYaml :: OIDListYaml -> [OID]
oidListFromYaml = map read . words

subGameFromYaml :: SubGameYaml -> WithFileNames SubGame
subGameFromYaml (SubGameYaml u o1 o2 o3 pl) = (\x -> SubGame
    { sgUnknown = either error id $ parseOneLinePure parsePrettyHex "unknown" u
    , sgOids1 = oidListFromYaml o1
    , sgOids2 = oidListFromYaml o2
    , sgOids3 = oidListFromYaml o3
    , sgPlaylist = x
    }) <$> traverse playListListFromYaml pl


gameYaml2Game :: GameYaml -> WithFileNames Game
gameYaml2Game CommonGameYaml {..} = pure CommonGame
        <*> pure gyGameType
        <*> pure gyRounds
        <*> pure gyUnknownC
        <*> pure gyEarlyRounds
        <*> pure gyRepeatLastMedia
        <*> pure gyUnknownX
        <*> pure gyUnknownW
        <*> pure gyUnknownV
        <*> playListListFromYaml gyStartPlayList
        <*> playListListFromYaml gyRoundEndPlayList
        <*> playListListFromYaml gyFinishPlayList
        <*> playListListFromYaml gyRoundStartPlayList
        <*> playListListFromYaml gyLaterRoundStartPlayList
        <*> traverse subGameFromYaml gySubgames
        <*> pure gyTargetScores
        <*> traverse playListListFromYaml gyFinishPlayLists
gameYaml2Game Game6Yaml {..} = pure Game6
        <*> pure gyRounds
        <*> pure gyBonusSubgameCount
        <*> pure gyBonusRounds
        <*> pure gyBonusTarget
        <*> pure gyUnknownI
        <*> pure gyEarlyRounds
        <*> pure gyUnknownQ
        <*> pure gyRepeatLastMedia
        <*> pure gyUnknownX
        <*> pure gyUnknownW
        <*> pure gyUnknownV
        <*> playListListFromYaml gyStartPlayList
        <*> playListListFromYaml gyRoundEndPlayList
        <*> playListListFromYaml gyFinishPlayList
        <*> playListListFromYaml gyRoundStartPlayList
        <*> playListListFromYaml gyLaterRoundStartPlayList
        <*> playListListFromYaml gyRoundStartPlayList2
        <*> playListListFromYaml gyLaterRoundStartPlayList2
        <*> traverse subGameFromYaml gySubgames
        <*> pure gyTargetScores
        <*> pure gyBonusTargetScores
        <*> traverse playListListFromYaml gyFinishPlayLists
        <*> traverse playListListFromYaml gyBonusFinishPlayLists
        <*> pure gyBonusSubgameIds
gameYaml2Game Game7Yaml {..} = pure Game7
        <*> pure gyRounds
        <*> pure gyUnknownC
        <*> pure gyEarlyRounds
        <*> pure gyRepeatLastMedia
        <*> pure gyUnknownX
        <*> pure gyUnknownW
        <*> pure gyUnknownV
        <*> playListListFromYaml gyStartPlayList
        <*> playListListFromYaml gyRoundEndPlayList
        <*> playListListFromYaml gyFinishPlayList
        <*> playListListFromYaml gyRoundStartPlayList
        <*> playListListFromYaml gyLaterRoundStartPlayList
        <*> traverse subGameFromYaml gySubgames
        <*> pure gyTargetScores
        <*> traverse playListListFromYaml gyFinishPlayLists
        <*> pure gySubgameGroups
gameYaml2Game Game8Yaml {..} = pure Game8
        <*> pure gyRounds
        <*> pure gyUnknownC
        <*> pure gyEarlyRounds
        <*> pure gyRepeatLastMedia
        <*> pure gyUnknownX
        <*> pure gyUnknownW
        <*> pure gyUnknownV
        <*> playListListFromYaml gyStartPlayList
        <*> playListListFromYaml gyRoundEndPlayList
        <*> playListListFromYaml gyFinishPlayList
        <*> playListListFromYaml gyRoundStartPlayList
        <*> playListListFromYaml gyLaterRoundStartPlayList
        <*> traverse subGameFromYaml gySubgames
        <*> pure gyTargetScores
        <*> traverse playListListFromYaml gyFinishPlayLists
        <*> pure (oidListFromYaml gyGameSelectOIDs)
        <*> pure gyGameSelect
        <*> playListListFromYaml gyGameSelectErrors1
        <*> playListListFromYaml gyGameSelectErrors2
gameYaml2Game Game9Yaml {..} = pure Game9
        <*> pure gyRounds
        <*> pure gyUnknownC
        <*> pure gyEarlyRounds
        <*> pure gyRepeatLastMedia
        <*> pure gyUnknownX
        <*> pure gyUnknownW
        <*> pure gyUnknownV
        <*> playListListFromYaml gyStartPlayList
        <*> playListListFromYaml gyRoundEndPlayList
        <*> playListListFromYaml gyFinishPlayList
        <*> playListListFromYaml gyRoundStartPlayList
        <*> playListListFromYaml gyLaterRoundStartPlayList
        <*> traverse subGameFromYaml gySubgames
        <*> pure gyTargetScores
        <*> traverse playListListFromYaml gyFinishPlayLists
        <*> traverse playListListFromYaml gyExtraPlayLists
gameYaml2Game Game10Yaml {..} = pure Game10
        <*> pure gyRounds
        <*> pure gyUnknownC
        <*> pure gyEarlyRounds
        <*> pure gyRepeatLastMedia
        <*> pure gyUnknownX
        <*> pure gyUnknownW
        <*> pure gyUnknownV
        <*> playListListFromYaml gyStartPlayList
        <*> playListListFromYaml gyRoundEndPlayList
        <*> playListListFromYaml gyFinishPlayList
        <*> playListListFromYaml gyRoundStartPlayList
        <*> playListListFromYaml gyLaterRoundStartPlayList
        <*> traverse subGameFromYaml gySubgames
        <*> pure gyTargetScores
        <*> traverse playListListFromYaml gyFinishPlayLists
        <*> traverse playListListFromYaml gyExtraPlayLists
gameYaml2Game Game16Yaml {..} = pure Game16
        <*> pure gyRounds
        <*> pure gyUnknownC
        <*> pure gyEarlyRounds
        <*> pure gyRepeatLastMedia
        <*> pure gyUnknownX
        <*> pure gyUnknownW
        <*> pure gyUnknownV
        <*> playListListFromYaml gyStartPlayList
        <*> playListListFromYaml gyRoundEndPlayList
        <*> playListListFromYaml gyFinishPlayList
        <*> playListListFromYaml gyRoundStartPlayList
        <*> playListListFromYaml gyLaterRoundStartPlayList
        <*> traverse subGameFromYaml gySubgames
        <*> pure gyTargetScores
        <*> traverse playListListFromYaml gyFinishPlayLists
        <*> pure (oidListFromYaml gyExtraOIDs)
        <*> traverse playListListFromYaml gyExtraPlayLists
gameYaml2Game Game253Yaml = pure Game253


mergeOnlyEqual :: String -> Word16 -> Word16 -> Word16
mergeOnlyEqual _ c1 c2 | c1 == c2 = c1
mergeOnlyEqual s c1 c2 = error $
    printf "The .yaml file specifies code %d for script \"%s\",\
           \but the .codes.yamls file specifies %d. Please fix this!" c2 s c1

toWord16 :: Word32 -> Word16
toWord16 x = fromIntegral x

toWord32 :: Word16 -> Word32
toWord32 x = fromIntegral x

scriptCodes :: [String] -> CodeMap -> Word32 -> Either String (String -> Word16, CodeMap)
scriptCodes [] codeMap productId = Right (error "scriptCodes []", codeMap)
scriptCodes codes codeMap productId
    | null strs || null nums = Right (lookupCode, totalMap)
    | otherwise = Left "Cannot mix numbers and names in scripts."
  where
    (strs, nums) = partitionEithers $ map f codes
    newStrs = filter (`M.notMember` codeMap) strs
    usedCodes = S.fromList $ M.elems codeMap

    f s = case readMaybe s of
            Nothing -> Left s
            Just n -> Right (n::Word16)

-- The following logic (for objectCodes) tries to use different object codes
-- for different projects, as far as possible. This makes the detection of not
-- having activated a book/product more robust.

-- We could theoretically set:
--    objectCodeOffsetMax = lastObjectCode - firstObjectCode.
-- This would assign perfectly usable object codes, and would minimize the
-- probability of object code collisions between products, but sometimes
-- object codes would wrap around from 14999 to 1000 even for small projects
-- which may be undesirable. We arbitrarily do not use the last 999 possible
-- offsets to avoid a wrap around in object codes for projects with <= 1000
-- object codes. This does not impose any limit on the number of object codes
-- per project. Every project can always use all 14000 object codes.
    objectCodeOffsetMax = lastObjectCode - firstObjectCode - 999

-- Distribute the used object codes for different projects across the whole
-- range of usable object codes. We do this by multiplying the productId with
-- the golden ratio to achive a maximum distance between different projects,
-- independent of the total number of different projects.
-- 8035 = (14999-1000-999+1)*((sqrt(5)-1)/2)
    objectCodeOffset = toWord16(rem (productId * 8035) (toWord32(objectCodeOffsetMax) + 1))

-- objectCodes always contains _all_ possible object codes [firstObjectCode..lastObjectCode],
-- starting at firstObjectCode+objectCodeOffset and then wrapping around.
    objectCodes = [firstObjectCode + objectCodeOffset .. lastObjectCode] ++ [firstObjectCode .. firstObjectCode + objectCodeOffset - 1]

    newAssignments =
        M.fromList $
        zip newStrs $
        filter (`S.notMember` usedCodes) $
        objectCodes

    totalMap = M.fromList
        [ (str, fromJust $
                readMaybe str `mplus`
                M.lookup str codeMap `mplus`
                M.lookup str newAssignments)
        | str <- codes
        ]

    readCode s = case readMaybe s of
        Nothing -> error $ printf "Cannot jump to named script \"%s\" in a yaml with numbered scripts." s
        Just c -> c
    lookupCode s = case M.lookup s totalMap of
        Nothing -> error $ printf "Cannot jump to unknown script \"%s\"." s
        Just c -> c

resolveRegs ::
    (M.Map Register Word16, [(a, Maybe [Line Register])]) ->
    (M.Map ResReg Word16, [(a, Maybe [Line ResReg])])
resolveRegs x = everywhere x
  where
    -- Could use generics somehow
    regs = S.fromList $
        M.keys (fst x) ++ concatMap (maybe [] (concatMap F.toList) . snd) (snd x)
    -- Could use generics somehow
    everywhere = M.mapKeys resolve *** map (second (fmap (map (fmap resolve))))


    regNums = S.fromList [ n | RegPos n <- S.toList regs ]
    regNames = [ n | RegName n <- S.toList regs ]
    mapping = M.fromList $ zip regNames [n | n <- [0..], n `S.notMember` regNums]
    resolve (RegPos n) = n
    resolve (RegName n) = fromMaybe (error "resolveRegs broken") (M.lookup n mapping)

resolveJumps :: (String -> Word16) -> [(a, Maybe [Line b])] -> [(a, Maybe [Line b])]
resolveJumps m = everywhere
  where
    everywhere = map (second (fmap (map resolveLine)))
    resolveLine (Line o cond cmds acts) = (Line o cond (map resolve cmds) acts)
    resolve (NamedJump n) = Jump (Const (m n))
    resolve c = c

newtype WithFileNames a = WithFileNames
    { runWithFileNames :: ((String -> Word16) -> a, [String] -> [String])
    }

instance Functor WithFileNames where
    fmap f (WithFileNames (r,fns)) = WithFileNames (f . r, fns)

instance Applicative WithFileNames where
    pure x = WithFileNames (const x, id)
    WithFileNames (r1,fns1) <*> WithFileNames (r2,fns2)
        = WithFileNames (\m -> r1 m (r2 m),fns1 . fns2)

recordFilename :: String -> WithFileNames Word16
recordFilename fn = WithFileNames (($ fn), (fn :))

resolveFileNames :: WithFileNames a -> (a, [String])
resolveFileNames (WithFileNames (r,fns)) = (r filename_lookup, filenames)
  where
    filenames = S.toList $ S.fromList $ fns []
    filename_lookup = (M.fromList (zip filenames [0..]) M.!)


ttYaml2tt :: FilePath -> TipToiYAML -> CodeMap -> IO (TipToiFile, CodeMap)
ttYaml2tt dir (TipToiYAML {..}) extCodeMap = do
    now <- getCurrentTime
    let date = formatTime defaultTimeLocale "%Y%m%d" now

    let codeMap = M.unionWithKey mergeOnlyEqual
                                 extCodeMap
                                 (fromMaybe M.empty ttyScriptCodes)

    (scriptMap, totalMap) <- either fail return $ scriptCodes (M.keys ttyScripts) codeMap ttyProduct_Id

    let m = M.mapKeys scriptMap ttyScripts
        first = fst (M.findMin m)
        last = fst (M.findMax m)

    welcome_names <- parseOneLine parsePlayList "welcome" (fromMaybe "" ttyWelcome)


    let ((prescripts, welcome, games), filenames) = resolveFileNames $
            (,,) <$>
            for [first ..last] (\oid ->
                (oid ,) <$>
                for (M.lookup oid m) (\raw_lines ->
                    forAn raw_lines (\i raw_line ->
                        let d = printf "Line %d of OID %d" i oid
                            (l,s) = either error id $ parseOneLinePure parseLine d raw_line
                        in l <$> traverse recordFilename s
                    )
                )
            ) <*>
            traverse recordFilename welcome_names <*>
            traverse gameYaml2Game (fromMaybe [] ttyGames)

    preInitRegs <- M.fromList <$> parseOneLine parseInitRegs "init" (fromMaybe "" ttyInit)

    -- resolve registers
    let (initRegs, scripts) = resolveRegs (preInitRegs, prescripts)

    -- resolve named jumps
    let scripts' = resolveJumps scriptMap scripts

    let maxReg = maximum $ 0:
            [ r
            | (_, Just ls) <- scripts'
            , Line _ cs as _ <- ls
            , r <- concatMap F.toList cs ++ concatMap F.toList as ]

    let ttySpeakMap = toSpeakMap (fromMaybe defaultLanguage ttyLanguage) ttySpeak

    -- Generate text-to-spech files
    forM (M.elems ttySpeakMap) $ \(lang, txt) ->
        textToSpeech lang txt

    -- Check which files do not exist


    -- Not very nice, better to use something like Control.Applicative.Error if
    -- it were in base, and not fixed to String.
    files_with_errors <- forM filenames $ \fn -> case M.lookup fn ttySpeakMap of
        Just (lang, txt) -> do
            Right <$> readFile' (ttsFileName lang txt)
        Nothing -> do
            let paths = [ combine dir relpath
                    | ext <- map snd fileMagics
                    , let pat = fromMaybe "%s" ttyMedia_Path
                    , let relpath = printf pat fn <.> ext
                    ]
            ex <- filterM doesFileExist paths
            case ex of
                [] -> do
                    return $ Left $ unlines $
                      "Could not find any of these files:" :
                      paths
                [f] -> Right <$> readFile' f
                _  -> do
                    return $ Left $ unlines $
                      "Multiple matching files found:" :
                      paths

    files <- case partitionEithers files_with_errors of
        ([],files)  -> return files
        (errors, _) -> putStr (unlines errors) >> exitFailure

    comment <- case ttyComment of
        Nothing -> return $ BC.pack $ "created with tttool version " ++ tttoolVersion
        Just c | length c > maxCommentLength -> do
                    printf "Comment is %d characters too long; the maximum is %d."
                           (length c - maxCommentLength) maxCommentLength
                    exitFailure
               | otherwise -> return $ BC.pack c

    return $ (TipToiFile
        { ttProductId = ttyProduct_Id
        , ttRawXor = knownRawXOR
        , ttComment = comment
        , ttDate = BC.pack date
        , ttLang = maybe BC.empty BC.pack ttyGME_Lang
        , ttWelcome = [welcome]
        , ttInitialRegs = [fromMaybe 0 (M.lookup r initRegs) | r <- [0..maxReg]]
        , ttScripts = scripts'
        , ttGames = games
        , ttAudioFiles = files
        , ttAudioXor = knownXOR
        , ttAudioFilesDoubles = False
        , ttChecksum = 0x00
        , ttChecksumCalc = 0x00
        , ttBinaries1 = []
        , ttBinaries2 = []
        , ttBinaries3 = []
        , ttBinaries4 = []
        , ttSpecialOIDs = Nothing
        }, totalMap)


lexer       = P.makeTokenParser $
    emptyDef
        { P.reservedOpNames = words ":= == /= < >="
        , P.opLetter       = oneOf ":!#%&*+./<=>?@\\^|-~" -- Removed $, used for registers
        }

parseLine :: Parser ([Word16] -> Line Register, [String])
parseLine = do
    conds <- many (P.try parseCond)
    (acts, filenames) <- parseCommands 0
    eof
    return (Line 0 conds acts, filenames)

descP d p = p <?> d

parseCond :: Parser (Conditional Register)
parseCond = descP "Conditional" $ do
    v1 <- parseTVal
    op <- parseCondOp
    v2 <- parseTVal
    P.lexeme lexer (char '?')
    return (Cond v1 op v2)

parseWord16 :: Parser Word16
parseWord16 = fromIntegral <$> P.natural lexer

parseReg :: Parser Register
parseReg = P.lexeme lexer $ char '$' >> (RegPos <$> parseWord16 <|> RegName <$> many1 (alphaNum <|> char '_'))

parseTVal :: Parser (TVal Register)
parseTVal = (Reg <$> parseReg <|> Const <$> parseWord16) <?> "Value"

parseCondOp :: Parser CondOp
parseCondOp = choice
    [ P.reservedOp lexer "==" >> return Eq
    , P.reservedOp lexer "<"  >> return Lt
    , P.reservedOp lexer ">"  >> return Gt
    , P.reservedOp lexer ">=" >> return GEq
    , P.reservedOp lexer "<=" >> return LEq
    , P.reservedOp lexer "/=" >> return NEq
    , P.reservedOp lexer "!=" >> return NEq
    ]

parseInitRegs :: Parser [(Register, Word16)]
parseInitRegs = many $ do
    r <- parseReg
    P.reservedOp lexer ":="
    v <- parseWord16
    return (r,v)

parsePlayList :: Parser [String]
parsePlayList = P.commaSep lexer $ parseAudioRef

parseAudioRef :: Parser String
parseAudioRef = P.lexeme lexer $ many1 (alphaNum <|> char '_')

parseScriptRef :: Parser String
parseScriptRef = P.lexeme lexer $ many1 (alphaNum <|> char '_')

parsePrettyHex :: Parser B.ByteString
parsePrettyHex = B.pack <$> many1 (P.lexeme lexer nibble)
  where
    nibble = fromIntegral <$> number 16 hexDigit
    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }


parseCommands :: Int -> Parser ([Command Register], [String])
parseCommands i =
    choice
    [ eof >> return ([],[])
    , descP "Register action" $
      do r <- parseReg
         op <- choice [ P.reservedOp lexer ":=" >> return Set
                      , P.reservedOp lexer "+=" >> return Inc
                      , P.reservedOp lexer "-=" >> return Dec
                      , P.reservedOp lexer "%=" >> return Mod
                      , P.reservedOp lexer "/=" >> return Div
                      , P.reservedOp lexer "*=" >> return Mult
                      , P.reservedOp lexer "&=" >> return And
                      , P.reservedOp lexer "|=" >> return Or
                      , P.reservedOp lexer "^=" >> return XOr
                      ]
         v <- parseTVal
         (cmds, filenames) <- parseCommands i
         return (ArithOp op r v : cmds, filenames)

    , descP "Negation" $
      do P.lexeme lexer $ string "Neg"
         r <- P.parens lexer $ parseReg
         (cmds, filenames) <- parseCommands i
         return (Neg r : cmds, filenames)

    , descP "Unknown action" $
      do P.lexeme lexer $ char '?'
         (r,v) <- P.parens lexer $
            (,) <$> parseReg <* P.comma lexer <*> parseTVal
         h <- P.parens lexer parsePrettyHex
         (cmds, filenames) <- parseCommands i
         return (Unknown h r v : cmds, filenames)

    , descP "Play action" $
      do (withA, withStar) <- P.lexeme lexer $ do
            char 'P'
            withA <- optionBool (char 'A')
            withStar <- optionBool (char '*')
            return (withA, withStar)
         fns <- P.parens lexer $ P.commaSep1 lexer parseAudioRef
         playAllUnknownArgument <- option (Const 0) $ P.parens lexer $ parseTVal
         let n = length fns
         let c = case (withA, withStar, fns) of
                (False, False, [fn]) -> Play (fromIntegral i)
                (False, False, _)    -> Random (fromIntegral (i + n - 1)) (fromIntegral i)
                (True,  False, _)    -> PlayAll (fromIntegral (i + n - 1)) (fromIntegral i)
                (False, True,  _)    -> RandomVariant playAllUnknownArgument
                (True,  True,  _)    -> PlayAllVariant playAllUnknownArgument
         (cmds, filenames) <- parseCommands (i+n)
         return (c : cmds, fns ++ filenames)
    , descP "Cancel" $
      do P.lexeme lexer $ char 'C'
         (cmds, filenames) <- parseCommands i
         return (Cancel : cmds, filenames)
    , descP "Jump action" $
      do P.lexeme lexer $ char 'J'
         cmd <- P.parens lexer $ choice
            [ Jump <$> parseTVal
            , NamedJump <$> parseScriptRef
            ]
         (cmds, filenames) <- parseCommands i
         return (cmd : cmds, filenames)
    , descP "Timer action" $
      do P.lexeme lexer $ char 'T'
         (r,v) <- P.parens lexer $
            (,) <$> parseReg <* P.comma lexer <*> parseTVal
         (cmds, filenames) <- parseCommands i
         return (Timer r v : cmds, filenames)
    , descP "Start Game" $
      do P.lexeme lexer $ char 'G'
         n <- P.parens lexer $ parseWord16
         (cmds, filenames) <- parseCommands i
         return (Game n : cmds, filenames)
    ]


optionBool :: Stream s m t => ParsecT s u m a -> ParsecT s u m Bool
optionBool p = option False (const True <$> p)


encodeFileCommented :: ToJSON a => FilePath -> String -> a -> IO ()
encodeFileCommented fn c v = do
    SBC.writeFile fn $ SBC.pack c <> encode v

readFile' :: String -> IO B.ByteString
readFile' filename =
    B.fromStrict <$> SB.readFile filename

readTipToiYaml :: FilePath -> IO (TipToiYAML, CodeMap)
readTipToiYaml inf = do
    content <- SBC.readFile inf
    let etty = decodeEither' content
    tty <- case etty of
        Left e -> print e >> exitFailure
        Right tty -> return tty

    ex <- doesFileExist infCodes
    codeMap <-
        if ex
        then do
            ettcy <- decodeFileEither infCodes
            case ettcy of
                Left e -> print e >> exitFailure
                Right ttcy -> return (ttcScriptCodes ttcy)
        else return M.empty
    return (tty, codeMap)
  where
    infCodes = codeFileName inf

writeTipToiYaml :: FilePath -> TipToiYAML -> IO ()
writeTipToiYaml out tty = encodeFile out tty

writeTipToiCodeYaml :: FilePath -> TipToiYAML -> CodeMap -> CodeMap -> IO ()
writeTipToiCodeYaml inf tty oldMap totalMap = do
    let newCodeMap =
            M.filterWithKey (\s v -> readMaybe s /= Just v) totalMap
            M.\\ fromMaybe M.empty (ttyScriptCodes tty)
    if M.null newCodeMap
    then do
        ex <- doesFileExist infCodes
        when ex $ removeFile infCodes
    else when (newCodeMap /= oldMap) $ encodeFileCommented infCodes codesComment (TipToiCodesYAML { ttcScriptCodes = newCodeMap })
  where
    infCodes = codeFileName inf

codesComment :: String
codesComment = unlines $ map ("# " ++)
    [ "This file contains a mapping from script names to oid codes."
    , "This way the existing scripts are always assigned to the the"
    , "same codes, even if you add further scripts."
    , ""
    , "You can copy the contents of this file into the main .yaml file,"
    , "if you want to have both together."
    , ""
    , "If you delete this file, the next run of \"ttool assemble\" might"
    , "use different codes for your scripts, and you might have to re-"
    , "create the images for your product."
    ]

codeFileName :: FilePath -> FilePath
codeFileName fn = base <.> "codes" <.> ext
  where
    base = dropExtension fn
    ext  = takeExtension fn

-- | Unused
debugGame :: ProductID -> IO TipToiYAML
debugGame productID = do
    return $ TipToiYAML
        { ttyProduct_Id = productID
        , ttyMedia_Path = Just "Audio/digits/%s"
        , ttyInit = Nothing
        , ttyScriptCodes = Nothing
        , ttySpeak = Nothing
        , ttyComment = Nothing
        , ttyGME_Lang = Nothing
        , ttyWelcome = Just $ "blob"
        , ttyScripts = M.fromList [
            (show oid, [line])
            | oid <- [1..15000]
            , let chars = [oid `div` 10^p `mod` 10| p <-[4,3,2,1,0]]
            , let line = ppLine t $ Line 0 [] [Play n | n <- [0..5]] ([10] ++ chars)
            ]
        , ttyLanguage = Nothing
        , ttyGames = Nothing
        }
  where
    t= M.fromList $
        [ (n, "english_" ++ show n) | n <- [0..9]] ++ [(10, "blob")]
