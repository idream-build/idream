module Main

import Remotelib2
import Testlib2
import System

main : IO ()
main = do
  putStr "Starting test... "
  if length val2 == 0 || length rem2 == 0
    then putStrLn "FAILED." >> exitFailure
    else putStrLn "Succeeded."
