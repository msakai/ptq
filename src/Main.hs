-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

{-# LANGUAGE CPP #-}
module Main where
import Report
import Version
#ifdef USE_UTF8
import Prelude hiding (putStr, putStrLn, getLine)
import System.IO (stdout, hSetBuffering, BufferMode (..))
import System.IO.UTF8
#else
import System.IO
#endif

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
