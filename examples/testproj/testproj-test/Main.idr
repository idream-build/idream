module Main

import Testlib2
import System (exit)

main : IO ()
main = if length val2 == 0 then exit 1 else pure ()
