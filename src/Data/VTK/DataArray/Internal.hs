{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables #-}

module Data.VTK.DataArray.Internal where

import qualified Codec.Compression.Zlib      as Z
import           Control.Arrow               (second)
import           Data.Bits                   (Bits, unsafeShiftL, unsafeShiftR,
                                              (.&.))
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Base64 as BL
import           Data.Text.Lazy              (Text)
import qualified Data.Vector.Storable        as Vec
import           GHC.Int
import           GHC.Word


-- * Compression
------------------------------------------------------------------------------
-- | Break the given @ByteString@ into chunks, compress these, and then pre-
--   pend a header.
--
--   TODO:
--    + header @n@ seems to be wrong
--
--   NOTE:
--    + see 'sc_vtk_write_compressed' (from 'sc_io.c') for the source-code
--      that was used to determine the header format;
--
compressPlusHeader :: BL.ByteString -> Text
compressPlusHeader bs =
  let s  = 1 `unsafeShiftL` 15           -- block-size
      l  = BL.length bs .&. pred s       -- 'lastsize'
      n  = (BL.length bs + s - 1) `unsafeShiftR` 15 -- num blocks
      o  = if l > 0 || BL.null bs then l else s

      -- Break into 32 KiB blocks, and then Z-compress
      cs = Z.compress <$> bchunks s bs

      -- Compute the header structure
      os = Vec.fromList . map w32 $ [n, s, o] ++ BL.length `map` cs
      hx = BL.pack . Vec.toList $ Vec.unsafeCast os

      w32 :: Integral i => i -> Word32
      w32  = fromIntegral
      {-# INLINE w32 #-}

      -- Base64 encode header plus each block seperately, before concatenating
  in  BL.encodeBase64 hx <> BL.encodeBase64 (BL.concat cs)

decompress :: BL.ByteString -> BL.ByteString
decompress bx = BL.concat cs
  where
--     (n:s:o:ls, bs) = splitAtHeader bx
    (_:_:_:ls, bs) = splitAtHeader bx
    cs = Z.decompress <$> ls `cchunks` BL.decodeBase64Lenient bs


-- * Helpers
------------------------------------------------------------------------------
-- | Break the given @ByteString@ into @n@-sized chunks.
bchunks :: Int64 -> BL.ByteString -> [BL.ByteString]
bchunks n = go where
  go :: BL.ByteString -> [BL.ByteString]
  go !bs | BL.null bs = []
         | otherwise  = uncurry (:) . second go $ BL.splitAt n bs

-- | Break the given compressed @ByteString@ into the size indicated.
cchunks :: [Int64] -> BL.ByteString -> [BL.ByteString]
cchunks (l:ls) bs = BL.take l bs:cchunks ls (BL.drop l bs)
cchunks     _   _ = []

------------------------------------------------------------------------------
-- | @ByteString@ to an @Integral@ type.
btoi :: forall i. (Bits i, Integral i) => Int64 -> Int64 -> BL.ByteString -> i
btoi n i bs = go 0 n (i+n-1)
-- btoi n i bs
--   | BL.length bs < n+i = error "given ByteString is too short"
--   | otherwise          = go 0 n (i+n-1)
  where
    go :: i -> Int64 -> Int64 -> i
    go !x 0 _ = x
    go !x c j = go (unsafeShiftL x 8 + fromIntegral (BL.index bs j)) (c-1) (j-1)
{-# INLINE btoi #-}

-- | @ByteString@ to an @Integral@ type, and using Big Endian byte-ordering.
btoiBE :: forall i. (Bits i, Integral i) => Int64 -> Int64 -> BL.ByteString -> i
btoiBE n i bs = go 0 n i where
  go :: i -> Int64 -> Int64 -> i
  go !x 0 _ = x
  go !x c j = go (unsafeShiftL x 8 + fromIntegral (BL.index bs j)) (c-1) (j+1)
{-# INLINE btoiBE #-}


-- ** Compression helpers
------------------------------------------------------------------------------
splitAtHeader :: BL.ByteString -> ([Int64], BL.ByteString)
splitAtHeader bs = second (BL.drop `flip` bs) $ headerValues bs

------------------------------------------------------------------------------
-- | Compute the number of bytes within the compression header.
headerLength :: BL.ByteString -> Int64
headerLength  = (*4) . (+3) . btoi 4 0 . BL.decodeBase64Lenient . BL.take 6

-- | Generate a list of the values of the compression header, and the total
--   number of bytes used by the (base64-encoded) header.
headerValues :: BL.ByteString -> ([Int64], Int64)
headerValues bs = (go <$> takeWhile (<lh) [0,4..], lb)
  where
    hs = BL.decodeBase64Lenient $ BL.take lb bs
    lh = headerLength bs
    lb = div (lh+2) 3 `unsafeShiftL` 2
--     lb = (unsafeShiftL lh 2 + 3) `div` 3
    go = btoi 4 `flip` BL.take lh hs
