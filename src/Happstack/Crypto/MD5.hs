-- TODO: deprecated module!

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
--
-- Module      : Happstack.Crypto.MD5
-- License     : BSD3
-- Maintainer  : lemmih@vo.com
-- Author      : Thomas.DuBuisson@mail.google.com
-- Stability   : experimental
-- Portability : portable, requires bang patterns and ByteString
-- Tested with : GHC-6.8.1
--

module Happstack.Crypto.MD5
	(md5
  ,md5InitialContext
	,md5Update
	,md5Finalize
	,MD5Context
	,md5File
        ,stringMD5
	,applyMD5Rounds
        ,test
	) where

import Prelude hiding ((!!))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal
import Data.Bits
import Data.List hiding ((!!))
import Data.Int(Int64)
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Numeric
import System.Environment()
import System.IO

blockSize :: Int
blockSize = 512		-- Block size in bits

blockSizeBytes :: Int
blockSizeBytes = blockSize `div` 8

blockSizeBytesW64 :: Int64
blockSizeBytesW64 = fromIntegral blockSizeBytes

blockSizeBits :: Word64
blockSizeBits = fromIntegral blockSize

data MD5Partial = MD5Par !Word32 !Word32 !Word32 !Word32
data MD5Context = MD5Ctx { mdPartial  :: MD5Partial,
			   mdLeftOver :: ByteString,
			   mdTotalLen :: Word64
			}

md5InitialContext :: MD5Context
md5InitialContext = MD5Ctx (MD5Par h0 h1 h2 h3) B.empty 0
h0, h1, h2, h3 :: Word32
h0 = 0x67452301
h1 = 0xEFCDAB89
h2 = 0x98BADCFE
h3 = 0x10325476

-- | Will read the lazy ByteString and return the md5 digest.
--   Some application might want to wrap this function for type safty.
md5 :: L.ByteString -> L.ByteString
md5 bs = md5Finalize $ md5Update md5InitialContext bs

md5Finalize :: MD5Context -> L.ByteString
md5Finalize !ctx@(MD5Ctx (MD5Par _ _ _ _) r !totLen) =
	let totLen' = (totLen + 8*fromIntegral l) :: Word64
	    padBS = B.pack $ 0x80 : replicate lenZeroPad 0 ++ size_split 8 totLen'

	    (MD5Ctx (MD5Par a' b' c' d') _ _) = md5Update ctx (L.fromChunks [r,padBS])
	in L.pack $ concatMap (size_split 4) [a',b',c',d']
	where
	l = B.length r
	lenZeroPad = if (l+1) <= blockSizeBytes - 8
			then (blockSizeBytes - 8) - (l+1)
			else (2*blockSizeBytes - 8) - (l+1)

size_split :: (Integral t, Num a) => Int -> t -> [a]
size_split 0 _ = []
size_split p n = (fromIntegral d):size_split (p-1) n'
    where (n', d) = divMod n 256

md5Update :: MD5Context -> L.ByteString -> MD5Context
md5Update ctx bsLazy =
	let _ = L.toChunks bsLazy
	    blks = block bsLazy
	in foldl' performMD5Update ctx blks

block :: L.ByteString -> [ByteString]
block bs = case L.toChunks bs of
             []		-> []
             _ 	        -> (B.concat . L.toChunks) top : block rest
    where
      (top,rest) = L.splitAt blockSizeBytesW64 bs
{-# INLINE block #-}

-- Assumes ByteString length == blockSizeBytes, will fold the 
-- context across calls to applyMD5Rounds.
performMD5Update :: MD5Context -> ByteString -> MD5Context
performMD5Update !ctx@(MD5Ctx !par@(MD5Par !a !b !c !d) _ !len) bs =
	let MD5Par a' b' c' d' = applyMD5Rounds par bs
	in if B.length bs == blockSizeBytes
		then MD5Ctx {
			mdPartial = MD5Par (a' + a) (b' + b) (c' + c) (d' + d),
			mdLeftOver = B.empty,
			mdTotalLen = len + blockSizeBits
			}
		else ctx { mdLeftOver = bs } 

applyMD5Rounds :: MD5Partial -> ByteString -> MD5Partial
applyMD5Rounds (MD5Par a b c d) w =
	let -- Round 1
	    !r0 = ff   a  b  c  d   (w!!0)  7  3614090360
	    !r1 = ff   d r0  b  c   (w!!1)  12 3905402710
	    !r2 = ff   c r1 r0  b   (w!!2)  17 606105819
	    !r3 = ff   b r2 r1 r0   (w!!3)  22 3250441966
	    !r4 = ff  r0 r3 r2 r1   (w!!4)  7  4118548399
	    !r5 = ff  r1 r4 r3 r2   (w!!5)  12 1200080426
	    !r6 = ff  r2 r5 r4 r3   (w!!6)  17 2821735955
	    !r7 = ff  r3 r6 r5 r4   (w!!7)  22 4249261313
	    !r8 = ff  r4 r7 r6 r5   (w!!8)  7  1770035416
	    !r9 = ff  r5 r8 r7 r6   (w!!9)  12 2336552879
	    !r10 = ff r6 r9 r8 r7  (w!!10) 17 4294925233
	    !r11 = ff r7 r10 r9 r8 (w!!11) 22 2304563134
	    !r12 = ff r8 r11 r10 r9 (w!!12) 7  1804603682
	    !r13 = ff r9 r12 r11 r10 (w!!13) 12 4254626195
	    !r14 = ff r10 r13 r12 r11 (w!!14) 17 2792965006
	    !r15 = ff r11 r14 r13 r12 (w!!15) 22 1236535329
	    -- Round 2
	    !r16 = gg r12 r15 r14 r13 (w!!1)  5  4129170786
	    !r17 = gg r13 r16 r15 r14 (w!!6)  9  3225465664
	    !r18 = gg r14 r17 r16 r15 (w!!11) 14 643717713
	    !r19 = gg r15 r18 r17 r16 (w!!0)  20 3921069994
	    !r20 = gg r16 r19 r18 r17 (w!!5)  5  3593408605
	    !r21 = gg r17 r20 r19 r18 (w!!10) 9  38016083
	    !r22 = gg r18 r21 r20 r19 (w!!15) 14 3634488961
	    !r23 = gg r19 r22 r21 r20 (w!!4)  20 3889429448
	    !r24 = gg r20 r23 r22 r21 (w!!9)  5  568446438
	    !r25 = gg r21 r24 r23 r22 (w!!14) 9  3275163606
	    !r26 = gg r22 r25 r24 r23 (w!!3)  14 4107603335
	    !r27 = gg r23 r26 r25 r24 (w!!8)  20 1163531501
	    !r28 = gg r24 r27 r26 r25 (w!!13) 5  2850285829
	    !r29 = gg r25 r28 r27 r26 (w!!2)  9  4243563512
	    !r30 = gg r26 r29 r28 r27 (w!!7)  14 1735328473
	    !r31 = gg r27 r30 r29 r28 (w!!12) 20 2368359562
	    -- Round 3
	    !r32 = hh r28 r31 r30 r29 (w!!5)  4  4294588738
	    !r33 = hh r29 r32 r31 r30 (w!!8)  11 2272392833
	    !r34 = hh r30 r33 r32 r31 (w!!11) 16 1839030562
	    !r35 = hh r31 r34 r33 r32 (w!!14) 23 4259657740
	    !r36 = hh r32 r35 r34 r33 (w!!1)  4  2763975236
	    !r37 = hh r33 r36 r35 r34 (w!!4)  11 1272893353
	    !r38 = hh r34 r37 r36 r35 (w!!7)  16 4139469664
	    !r39 = hh r35 r38 r37 r36 (w!!10) 23 3200236656
	    !r40 = hh r36 r39 r38 r37 (w!!13) 4  681279174
	    !r41 = hh r37 r40 r39 r38 (w!!0)  11 3936430074
	    !r42 = hh r38 r41 r40 r39 (w!!3)  16 3572445317
	    !r43 = hh r39 r42 r41 r40 (w!!6)  23 76029189
	    !r44 = hh r40 r43 r42 r41 (w!!9)  4  3654602809
	    !r45 = hh r41 r44 r43 r42 (w!!12) 11 3873151461
	    !r46 = hh r42 r45 r44 r43 (w!!15) 16 530742520
	    !r47 = hh r43 r46 r45 r44 (w!!2)  23 3299628645
	    -- Round 4
	    !r48 = ii r44 r47 r46 r45 (w!!0)  6  4096336452
	    !r49 = ii r45 r48 r47 r46 (w!!7)  10 1126891415
	    !r50 = ii r46 r49 r48 r47 (w!!14) 15 2878612391
	    !r51 = ii r47 r50 r49 r48 (w!!5)  21 4237533241
	    !r52 = ii r48 r51 r50 r49 (w!!12) 6  1700485571
	    !r53 = ii r49 r52 r51 r50 (w!!3)  10 2399980690
	    !r54 = ii r50 r53 r52 r51 (w!!10) 15 4293915773
	    !r55 = ii r51 r54 r53 r52 (w!!1)  21 2240044497
	    !r56 = ii r52 r55 r54 r53 (w!!8)  6  1873313359
	    !r57 = ii r53 r56 r55 r54 (w!!15) 10 4264355552
	    !r58 = ii r54 r57 r56 r55 (w!!6)  15 2734768916
	    !r59 = ii r55 r58 r57 r56 (w!!13) 21 1309151649
	    !r60 = ii r56 r59 r58 r57 (w!!4)  6  4149444226
	    !r61 = ii r57 r60 r59 r58 (w!!11) 10 3174756917
	    !r62 = ii r58 r61 r60 r59 (w!!2)  15 718787259
	    !r63 = ii r59 r62 r61 r60 (w!!9)  21 3951481745
	in MD5Par r60 r63 r62 r61
	where
	f !x !y !z = (x .&. y) .|. ((complement x) .&. z)
	{-# INLINE f #-}
	g !x !y !z = (x .&. z) .|. (y .&. (complement z))
	{-# INLINE g #-}
	h !x !y !z = (x `xor` y `xor` z)
	{-# INLINE h #-}
	i !x !y !z = y `xor` (x .|. (complement z))
	{-# INLINE i #-}
	ff a_ b_ c_ d_ x s ac = {-# SCC "ff" #-}
		let !a' = f b_ c_ d_ + x + ac + a_
		    !a'' = rotateL a' s
		in a'' + b_
	{-# INLINE ff #-}
	gg a_ b_ c_ d_ x s ac = {-# SCC "gg" #-}
		let !a' = g b_ c_ d_ + x + ac + a_
		    !a'' = rotateL a' s
		in a'' + b_
	{-# INLINE gg #-}
	hh a_ b_ c_ d_ x s ac = {-# SCC "hh" #-}
		let !a' = h b_ c_ d_ + x + ac + a_
		    !a'' = rotateL a' s
		    in a'' + b_
	{-# INLINE hh #-}
	ii a_ b_ c_ d_  x s ac = {-# SCC "ii" #-}
		let !a' = i b_ c_ d_ + x + ac + a_
		    !a'' = rotateL a' s
		in a'' + b_
	{-# INLINE ii #-}
	(!!) word32s pos = getNthWord pos word32s
	{-# INLINE (!!) #-}

	getNthWord n (PS ptr off _) =
		inlinePerformIO $ withForeignPtr ptr $ \ptr' -> do
		let p = castPtr $ plusPtr ptr' off
		peekElemOff p n
	{-# INLINE getNthWord #-}
{-# INLINE applyMD5Rounds #-}

stringMD5 :: L.ByteString -> String
stringMD5 lazy = 
	let x = L.toChunks lazy
	    w = B.unpack (B.concat x)
	    s = map (\v -> showHex v "") w
	    s' = map (\v -> if length v == 1 then '0':v else v) s
	in concat s'

test :: IO ()
test = do
	let h = md5 $ L.pack []
	putStrLn $ "Hash is:   " ++ (stringMD5 h)
	putStrLn $ "Should Be: d41d8cd98f00b204e9800998ecf8427e" 

md5File :: String -> IO ()
md5File f = do
	h <- openFile f ReadMode
	s <- L.hGetContents h
	let hash = md5 s
	putStrLn (stringMD5 hash)
	return ()
