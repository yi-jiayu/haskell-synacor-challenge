module Main (main) where
import System.Environment
import           Control.Lens hiding (iset, op)
import qualified Data.Array.Unboxed      as Array
import qualified Data.ByteString as B
import           Instructions
import           Parser
import           Types

load :: Vm -> B.ByteString -> IO Vm
load vm bin = let mem = Array.listArray (0, 32767) (toIntsr bin ++ repeat 0)
                  vm' = set memory mem vm
              in do putStr "loading program..."
                    putChar $ seq (view memory vm' Array.! 0) ' '-- put this here to force the array to be evaluated
                    putStrLn "done"
                    return vm'

step :: Vm -> IO Vm
step vm = let pc = view (registers . progCtr) vm
              instr = instrAt vm pc
              op' = case view op instr of HALT -> ihalt
                                          SET -> iset
                                          PUSH -> ipush
                                          POP -> ipop
                                          IEQ -> ieq
                                          IGT -> igt
                                          JMP -> ijmp
                                          JT -> ijt
                                          JF -> ijf
                                          ADD -> iadd
                                          MULT -> imult
                                          MOD -> imod
                                          AND -> iand
                                          OR -> ior
                                          NOT -> inot
                                          RMEM -> irmem
                                          WMEM -> iwmem
                                          CALL -> icall
                                          RET -> iret
                                          OUT -> iout
                                          IN -> iin
                                          NOOP -> inoop
              in op' vm instr

run :: Vm -> IO Vm
run vm = let pc = view (registers . progCtr) vm
         in if pc < 0 || pc > 32775
           then do print $ view registers vm
                   return vm
           else do vm' <- step vm
                   run vm'

main = do args <- getArgs
          if null args
            then putStrLn "Usage: vm.exe path/to/binary"
            else do bin <- B.readFile (head args)
                    prog <- load initVm bin
                    _ <- run prog
                    return ()
