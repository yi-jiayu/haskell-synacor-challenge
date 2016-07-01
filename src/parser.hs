module Parser (toIntsr) where
import qualified Data.Array.Unboxed as Array
import           Data.Bits       (shift)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import           Data.Word
import           Types

parse :: B.ByteString -> Array.Array Int Instruction
parse bin = let instrs = fst (parse' ([], bin))
            in Array.listArray (1, length instrs) instrs

parse' :: ([Instruction], B.ByteString) -> ([Instruction], B.ByteString)
parse' x@(prog, bin) = if B.length bin == 0 then (prog, B.empty)
                      else let next = case B.head bin of 0  -> parse1 x
                                                         1  -> parse3 x
                                                         2  -> parse2 x
                                                         3  -> parse2 x
                                                         4  -> parse4 x
                                                         5  -> parse4 x
                                                         6  -> parse2 x
                                                         7  -> parse3 x
                                                         8  -> parse3 x
                                                         9  -> parse4 x
                                                         10 -> parse4 x
                                                         11 -> parse4 x
                                                         12 -> parse4 x
                                                         13 -> parse4 x
                                                         14 -> parse3 x
                                                         15 -> parse3 x
                                                         16 -> parse3 x
                                                         17 -> parse2 x
                                                         18 -> parse1 x
                                                         19 -> parse2 x
                                                         20 -> parse2 x
                                                         21 -> parse1 x
                                                         _  -> drop1 x
                           in parse' next

int :: Word8 -> Word8 -> Int
int low' high' = let low = fromIntegral low' :: Int
                     high = fromIntegral high' :: Int
                 in low + high `shift` 8

toInts :: B.ByteString -> [Int]
toInts bin = fst (toInts' ([], bin))

-- mmap :: Array.UArray Int Int -> B.ByteString -> Array.UArray Int Int
-- mmap mem bin =

toIntsr :: B.ByteString -> [Int]
toIntsr bin = fst (B.foldr' toIntsr' ([], True) bin)

toIntsr' :: Word8 -> ([Int], Bool) -> ([Int], Bool)
toIntsr' next (accum, highBit) = if highBit
                                 then (fromIntegral next `shift` 8:accum, False)
                                 else ((fromIntegral next + head accum):tail accum, True)

toInts' :: ([Int], B.ByteString) -> ([Int], B.ByteString)
toInts' x@(accum, bin) = if B.null bin then x
                         else let (valL: valH: _) = B.unpack $ B.take 2 bin
                                  val = int valL valH
                              in toInts' (accum ++ [val], B.drop 2 bin)

parse1 :: ([Instruction], B.ByteString) -> ([Instruction], B.ByteString)
parse1 (prog, bin) = let (opl:oph:_) = B.unpack $ B.take 2 bin
                         op = opFor (int opl oph)
                     in (prog ++ [Instruction op 0 0 0], B.drop 2 bin)

parse2 :: ([Instruction], B.ByteString) -> ([Instruction], B.ByteString)
parse2 (prog, bin) = let (opl:oph:al:ah:_) = B.unpack $ B.take 4 bin
                         op = opFor (int opl oph)
                         a = int al ah
                     in (prog ++ [Instruction op a 0 0], B.drop 4 bin)

unpack3 :: B.ByteString -> (Op, Int, Int)
unpack3 bin = let (op, a, b, _) = unpack4 bin
              in (op, a, b)

parse3 :: ([Instruction], B.ByteString) -> ([Instruction], B.ByteString)
parse3 (prog, bin) = let (op, a, b) = unpack3 bin
                     in (prog ++ [Instruction op a b 0], B.drop 6 bin)

unpack4 :: B.ByteString -> (Op, Int, Int, Int)
unpack4 bin = let (opl:oph:al:ah:bl:bh:cl:ch:_) = B.unpack $ B.take 8 bin
                  op = opFor (int opl oph)
                  a = int al ah
                  b = int bl bh
                  c = int cl ch
              in (op, a, b, c)

parse4 :: ([Instruction], B.ByteString) -> ([Instruction], B.ByteString)
parse4 (prog, bin) = let (op, a, b, c) = unpack4 bin
                     in (prog ++ [Instruction op a b c], B.drop 8 bin)

drop1 :: ([Instruction], B.ByteString) -> ([Instruction], B.ByteString)
drop1 (prog, bin) = (prog, B.drop 2 bin)
