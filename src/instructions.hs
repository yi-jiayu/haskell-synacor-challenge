module Instructions where
import           Control.Lens
import qualified Data.Array.Unboxed as Array
import           Data.Bits
import qualified Data.BitVector     as BV
import           Data.Char
import           Data.Maybe
import           Prelude            hiding (lookup)
import           Types

instrAt :: Vm -> Int -> Instruction
instrAt vm pc = let mem = view memory vm
                    (opcode:a:b:c:_) = map ((Array.!) mem) [pc, pc+1, pc+2, pc+3]
                 in Instruction (opFor opcode) a b c

lookup :: Vm -> Int -> Int
lookup vm addr = if addr < 32768
                 then (Array.!) (view memory vm) addr
                 else let addr' = case addr of 32768 -> view (registers . r0) vm
                                               32769 -> view (registers . r1) vm
                                               32770 -> view (registers . r2) vm
                                               32771 -> view (registers . r3) vm
                                               32772 -> view (registers . r4) vm
                                               32773 -> view (registers . r5) vm
                                               32774 -> view (registers . r6) vm
                                               32775 -> view (registers . r7) vm
                                               _ -> error "ERRINVALIDADDR"
                      in view memory vm Array.! addr'

store :: Vm -> Int -> Int -> Vm
store vm addr val = if addr < 32768
                    then let mem' = (Array.//) (view memory vm) [(addr, val)]
                         in set memory mem' vm
                    else case addr of 32768 -> set (registers . r0) val vm
                                      32769 -> set (registers . r1) val vm
                                      32770 -> set (registers . r2) val vm
                                      32771 -> set (registers . r3) val vm
                                      32772 -> set (registers . r4) val vm
                                      32773 -> set (registers . r5) val vm
                                      32774 -> set (registers . r6) val vm
                                      32775 -> set (registers . r7) val vm
                                      _ -> error "ERRINVALIDADDR"

value :: Vm -> Int -> Int
value vm val
  | val < 32768 = val
  | val > 32775 = error "ERRINVALIDVAL"
  | otherwise = case val of 32768 -> view (registers . r0) vm
                            32769 -> view (registers . r1) vm
                            32770 -> view (registers . r2) vm
                            32771 -> view (registers . r3) vm
                            32772 -> view (registers . r4) vm
                            32773 -> view (registers . r5) vm
                            32774 -> view (registers . r6) vm
                            32775 -> view (registers . r7) vm
                            _ -> error "ERRIMPOSSIBLE"

literal :: Int -> Int
literal = id

incPC :: Vm -> Int -> Vm
incPC vm inc = over (registers . progCtr) (+ inc) vm

setPC :: Vm -> Int -> Vm
setPC vm new = set (registers . progCtr) new vm

ihalt :: Vm -> Instruction -> IO Vm
ihalt vm _ = return (setPC vm (-1))

iset :: Vm -> Instruction -> IO Vm
iset vm instr = let a' = view a instr
                    b' = value vm (view b instr)
                    reg = regFor a'
                    vm' = set (registers . reg) b' vm -- update memory
                    vm'' = incPC vm' 3                -- increment pc
                in return vm''

ipush :: Vm -> Instruction -> IO Vm
ipush vm instr = let a' = value vm (view a instr)
                     vm' = over stack (++ [a']) vm -- update stack
                     vm'' = incPC vm' 2            -- increment pc
                 in return vm''

ipop :: Vm -> Instruction -> IO Vm
ipop vm instr = if null (view stack vm) then error "ERRPOPEMPTYSTACK"
                  else let addr = view a instr
                           val = last (view stack vm)
                           stack' = init (view stack vm)
                           vm' = store vm addr val     -- write last val of stack into <a>
                           vm'' = set stack stack' vm' -- remove last val of stack from vm
                           vm'3 = incPC vm'' 2         -- increment pc
                       in return vm'3

icomp :: (Int -> Int -> Bool) -> Vm -> Instruction -> IO Vm
icomp func vm instr = let addr = view a instr
                          b' = value vm (view b instr)
                          c' = value vm (view c instr)
                          result = if func b' c' then 1 else 0 -- compare
                          vm' = store vm addr result           -- store result
                          vm'' = incPC vm' 4                   -- increment pc
                      in return vm''

ieq :: Vm -> Instruction -> IO Vm
ieq = icomp (==)

igt :: Vm -> Instruction -> IO Vm
igt = icomp (>)

ijmp :: Vm -> Instruction -> IO Vm
ijmp vm instr = let addr = value vm (view a instr)
                    vm' = setPC vm addr -- jump
                in return vm'

ijt :: Vm -> Instruction -> IO Vm
ijt vm instr = let cond = value vm (view a instr)
                   addr = value vm (view b instr)
               in if cond /= 0
                  then return (setPC vm addr) -- jump
                  else return (incPC vm 3)    -- no jump

ijf :: Vm -> Instruction -> IO Vm
ijf vm instr = let cond = value vm (view a instr)
                   addr = value vm (view b instr)
               in if cond == 0
                  then return (setPC vm addr) -- jump
                  else return (incPC vm 3)    -- no jump

iarith :: (Int -> Int -> Int) -> Vm -> Instruction -> IO Vm
iarith func vm instr = let addr = view a instr
                           b' = value vm (view b instr)
                           c' = value vm (view c instr)
                           result = func b' c'          -- apply func
                           result' = mod result 32768   -- modulo
                           vm' = store vm addr result'  -- update memory
                           vm'' = incPC vm' 4           -- increment pc
                       in return vm''

iadd :: Vm -> Instruction -> IO Vm
iadd = iarith (+)

imult :: Vm -> Instruction -> IO Vm
imult = iarith (*)

imod :: Vm -> Instruction -> IO Vm
imod vm instr = let addr = view a instr
                    b' = value vm (view b instr)
                    c' = value vm (view c instr)
                    result = mod b' c'           -- modulo
                    vm' = store vm addr result   -- update memory
                    vm'' = incPC vm' 4           -- increment pc
                in return vm''

iand :: Vm -> Instruction -> IO Vm
iand vm instr = let addr = view a instr
                    b' = value vm (view b instr)
                    c' = value vm (view c instr)
                    result = b' .&. c'           -- bitwise and
                    vm' = store vm addr result   -- update memory
                    vm'' = incPC vm' 4           -- increment pc
                in return vm''

ior :: Vm -> Instruction -> IO Vm
ior vm instr = let addr = view a instr
                   b' = value vm (view b instr)
                   c' = value vm (view c instr)
                   result = b' .|. c'           -- bitwise or
                   vm' = store vm addr result   -- update memory
                   vm'' = incPC vm' 4           -- increment pc
               in return vm''

inot :: Vm -> Instruction -> IO Vm
inot vm instr = let addr = view a instr
                    b' = value vm (view b instr)
                    bv = BV.bitVec 15 b'
                    result = (fromInteger . BV.uint . BV.complement) bv -- complement (not sure if it's a 15-bit complement though)
                    vm' = store vm addr result   -- update memory
                    vm'' = incPC vm' 3           -- increment pc
                in return vm''

irmem :: Vm -> Instruction -> IO Vm
irmem vm instr = let val = lookup vm (view b instr) -- read memory at address b
                     dest = view a instr            -- destination addr
                     vm' = store vm dest val        -- write to destination addr
                     vm'' = incPC vm' 3             -- increment pc
                 in return vm''

iwmem :: Vm -> Instruction -> IO Vm
iwmem vm instr = let addr = value vm (view a instr) -- get destination address
                     val = value vm (view b instr)  -- get value to be written
                     vm' = store vm addr val        -- write value to destination
                     vm'' = incPC vm' 3             -- increment pc
                 in return vm''

icall :: Vm -> Instruction -> IO Vm
icall vm instr = let pc = view (registers . progCtr) vm -- get current pc
                     stack' = view stack vm ++ [pc + 2] -- push addr of next instruction to stack
                     vm' = set stack stack' vm          -- update stack
                     dest = value vm (view a instr)     -- get destination addr
                     vm'' = setPC vm' dest              -- jump
                 in return vm''

iret :: Vm -> Instruction -> IO Vm
iret vm _ = if null (view stack vm)
                then return (setPC vm (-1))                     -- halt if is empty
                else let addr = last (view stack vm)   -- get jump destination
                         stack' = init (view stack vm) -- remove last element from stack
                         vm' = set stack stack' vm     -- update stack
                         vm'' = setPC vm' addr         -- jump
                     in return vm''

iout :: Vm -> Instruction -> IO Vm
iout vm instr = do putChar $ chr (value vm (view a instr)) -- output char
                   let vm' = incPC vm 2         -- increment pc
                   return vm'

iin :: Vm -> Instruction -> IO Vm
iin vm instr =  if null (view inpBuf vm)
                then do line <- getLine
                        let cmd = if null (words line) then " " else head (words line)
                        vm' <- case cmd of "regs" -> cmdShowRegs vm
                                           "set" -> cmdSetReg vm line
                                           "memdump" -> cmdMemDump vm
                                           _ -> return (set inpBuf (line ++ "\n") vm)
                        iin vm' instr
                else do let addr = view a instr
                        let char = head (view inpBuf vm)
                        let vm' = over inpBuf tail vm
                        let vm'' = store vm' addr (ord char) -- store ascii code of char to mem
                        let vm'2 = incPC vm'' 2              -- increment pc
                        return vm'2

cmdShowRegs :: Vm -> IO Vm
cmdShowRegs vm = do print (view registers vm)
                    return (set inpBuf "look\n" vm)

cmdSetReg :: Vm -> String -> IO Vm
cmdSetReg vm cmd = let (_:reg:newVal':_) = words cmd
                       newVal = read newVal' :: Int
                       vm' = case reg of "r0" -> Just (set (registers . r0) newVal vm)
                                         "r1" -> Just (set (registers . r1) newVal vm)
                                         "r2" -> Just (set (registers . r2) newVal vm)
                                         "r3" -> Just (set (registers . r3) newVal vm)
                                         "r4" -> Just (set (registers . r4) newVal vm)
                                         "r5" -> Just (set (registers . r5) newVal vm)
                                         "r6" -> Just (set (registers . r6) newVal vm)
                                         "r7" -> Just (set (registers . r7) newVal vm)
                                         _ -> Nothing
                   in maybe
                      (do putStrLn "Invalid register specified!"
                          line <- getLine
                          cmdSetReg vm line)
                      (return . set inpBuf "look\n")
                      vm'

cmdMemDump :: Vm -> IO Vm
cmdMemDump vm = let mem = view memory vm
                in do writeFile "memdump" (show (Array.elems mem))
                      putStrLn "Memory dumped to ./memdump"
                      return (set inpBuf "look\n" vm)

inoop :: Vm -> Instruction -> IO Vm
inoop vm _ = return (incPC vm 1) -- increment pc
