-----------------------------------------------------------------------------
-- |
-- Module      :  CGIMain
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  portable

module Main where

import ReportHTML
import CGI
import Control.Concurrent

-- 1秒以内に処理が終わらなかったら、そのままプロセスを終了
main :: IO ()
main =
    do th <- myThreadId
       forkIO $ threadDelay (1*1000*1000) >> killThread th
       runCGI appMain

appMain :: HTTPRequest -> IO HTTPResponse
appMain req = return $ 
    case lookupVar "s" req of
    Nothing ->
	HTTPResponse (textContentType "text/plain" "us-ascii") "(no input)\n"
    Just s  ->
	--HTTPResponse (textContentType "text/plain" "utf-8") (report s)
        HTTPResponse (textContentType "text/html" "utf-8") (report s)
