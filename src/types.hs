{-# LANGUAGE TemplateHaskell #-}

module Types where
import           Control.Lens
import qualified Data.Array.Unboxed as Array
import qualified Data.Map.Strict as Map

data Vm = Vm { _registers :: Regs
             , _memory    :: Array.UArray Int Int
             , _stack     :: [Int] }

data Regs = Regs { _r0      :: Int
                 , _r1      :: Int
                 , _r2      :: Int
                 , _r3      :: Int
                 , _r4      :: Int
                 , _r5      :: Int
                 , _r6      :: Int
                 , _r7      :: Int
                 , _progCtr :: Int } deriving (Show)

data Instruction = Instruction { _op :: Op
                               ,  _a :: Int
                               ,  _b :: Int
                               ,  _c :: Int } deriving (Show)

data Op = HALT | SET | PUSH | POP | IEQ | IGT | JMP | JT | JF | ADD | MULT | MOD | AND | OR | NOT | RMEM | WMEM | CALL | RET | OUT | IN | NOOP deriving (Eq, Show)

initMem :: Array.UArray Int Int
initMem = Array.listArray (0, 32767) (repeat 0)

initRegs :: Regs
initRegs = Regs 0 0 0 0 0 0 0 0 0

initVm :: Vm
initVm = Vm initRegs initMem []

makeLenses ''Vm
makeLenses ''Regs
makeLenses ''Instruction

opcodesMap :: Map.Map Int Op
opcodesMap = Map.fromList [ (0, HALT), (1, SET), (2, PUSH), (3, POP), (4, IEQ), (5, IGT)
                          , (6, JMP), (7, JT), (8, JF), (9, ADD), (10, MULT), (11, MOD)
                          , (12, AND), (13, OR), (14, NOT), (15, RMEM), (16, WMEM)
                          , (17, CALL), (18, RET), (19, OUT), (20, IN), (21, NOOP) ]

opFor :: Int -> Op
opFor op = opcodesMap Map.! op

registersMap :: (Functor f0) => Map.Map Int ((Int -> f0 Int) -> Regs -> f0 Regs)
registersMap = Map.fromList [ (32768, r0), (32769, r1), (32770, r2), (32771, r3)
                            , (32772, r4), (32773, r5), (32774, r6), (32775, r7) ]

regFor :: Functor f0 => Int -> (Int -> f0 Int) -> Regs -> f0 Regs
regFor addr = registersMap Map.! addr
