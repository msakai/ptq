-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

module Main where
import Report
import Version
import System.IO

main :: IO ()
main = do
  putStrLn banner
  hSetBuffering stdout NoBuffering
  loop

banner :: String
banner = unlines
  [ "PTQ version " ++ versionStr
  , ""
  , "Type :quit to quit"
  ]

loop :: IO ()
loop = do
  putStr "PTQ> "
  s <- getLine
  case s of
    ":quit" -> return ()
    _ -> do
      putStr (report s)
      loop
