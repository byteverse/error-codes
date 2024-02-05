import Foreign.C.Error

import qualified Foreign.C.Error.Describe as D

main :: IO ()
main = do
  stringMatch "A" eOK "EOK"
  stringMatch "B" eADDRNOTAVAIL "EADDRNOTAVAIL"
  stringMatch "C" eOPNOTSUPP "EOPNOTSUPP"
  stringMatch "D" (Errno (-5264)) "UNKNOWN"
  stringMatch "E" (Errno (-1)) "UNKNOWN"
  stringMatch "F" (Errno 257) "UNKNOWN"
  putStrLn "All tests passed"

stringMatch :: String -> Errno -> String -> IO ()
stringMatch ident e expected = do
  let actual = D.string e
  if actual == expected
    then pure ()
    else fail $ ident ++ ": expected " ++ expected ++ " but got " ++ actual
