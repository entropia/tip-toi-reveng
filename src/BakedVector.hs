{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables, CPP #-}

-- © 2015 Patrick Chilton

module BakedVector where

import qualified Data.ByteString.Lazy as BSL
#if __GLASGOW_HASKELL__ <= 704
import Data.ByteString.Lazy.Char8 (unpack)
#else
import Data.ByteString.Lazy (unpack)
#endif
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Builder.Prim
import Foreign.Storable
import qualified Data.Vector.Storable as VS
import Foreign.ForeignPtr
import GHC.Ptr
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Data.Word
import Utils

-- Compiled-in Storable Vectors.
-- Example: [intVector| 1, 2, 4, 6, 7 |]
--          [intString|\xEF\xBE\xAD\xDE\NUL\NUL\NUL\NUL]
--          $(bakeVector [ 1, 2, 3 ])

class (Read a, Storable a) => Bakable a where
  bakeType :: proxy a -> Q Type
  bakePrim :: proxy a -> FixedPrim a

bakeString :: forall proxy a. Bakable a => proxy a -> BSL.ByteString -> Q Exp
bakeString p s =
  case fromIntegral (BSL.length s) `quotRem` sizeOf (undefined :: a) of
    ( len, 0 ) ->
      [| let ptr = unsafeDupablePerformIO $ newForeignPtr_ $ Ptr $(litE)
         in VS.unsafeFromForeignPtr ptr 0 len :: VS.Vector $(bakeType p)
      |] where litE = return $ LitE $ StringPrimL $ unpack s
    _ -> fail "Invalid string length."

bakeVector :: Bakable a => [ a ] -> Q Exp
bakeVector xs
  = bakeString xs $ toLazyByteString $ primMapListFixed (bakePrim xs) xs

quoter :: (String -> Q Exp) -> QuasiQuoter
quoter qExp = QuasiQuoter qExp unsupported unsupported unsupported
  where unsupported = fail "Baked vectors must be used as expressions."

bakedString :: Bakable a => proxy a -> QuasiQuoter
bakedString p = quoter $ \s ->
  case readMaybe ("\"" ++ s ++ "\"") of
    Nothing -> fail "Failed to parse baked vector string."
    Just s' -> bakeString p s'

bakedVector :: forall proxy a. Bakable a => proxy a -> QuasiQuoter
bakedVector _ = quoter $ \s ->
  case readMaybe ("[" ++ s ++ "]") of
    Nothing -> fail "Failed to parse baked vector literal."
    Just xs -> bakeVector (xs :: [ a ])

instance Bakable Int where
  bakeType _ = [t|Int|]
  bakePrim _ = intHost

intVector :: QuasiQuoter
intVector = bakedVector intHost

intString :: QuasiQuoter
intString = bakedString intHost

instance Bakable Float where
  bakeType _ = [t|Float|]
  bakePrim _ = floatHost

floatVector :: QuasiQuoter
floatVector = bakedVector floatHost

floatString :: QuasiQuoter
floatString = bakedString floatHost

instance Bakable Double where
  bakeType _ = [t|Double|]
  bakePrim _ = doubleHost

doubleVector :: QuasiQuoter
doubleVector = bakedVector doubleHost

doubleString :: QuasiQuoter
doubleString = bakedString doubleHost

instance Bakable Word16 where
  bakeType _ = [t|Word16|]
  bakePrim _ = word16Host

word16Vector :: QuasiQuoter
word16Vector = bakedVector word16Host

word16String :: QuasiQuoter
word16String = bakedString word16Host
