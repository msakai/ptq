-----------------------------------------------------------------------------
-- |
-- Module      :  CGI
-- Copyright   :  Copyright (c) 2005,2006 Minero Aoki
-- License     :  LGPL (see COPYING)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

{-# LANGUAGE CPP #-}

--
-- $Id: CGI.hs,v 1.2 2006/05/14 17:29:22 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module CGI
  (runCGI,
   HTTPRequest, varExist, lookupVar, lookupVars,
   HTTPResponse(..), textContentType) where

import URLEncoding
import Data.Maybe
import Control.Monad
import System.IO
import System.IO.Error
import System.Environment

runCGI :: (HTTPRequest -> IO HTTPResponse) -> IO ()
runCGI f = do hSetBinaryMode stdin True
              -- hSetBinaryMode stdout True
              hSetEncoding stdout utf8
              hSetNewlineMode stdout noNewlineTranslation
              input <- getContents
              env <- cgiEnvs
              res@(HTTPResponse ctype body) <- f (parseCGIRequest env input)
              putStr $ "Content-Type: " ++ ctype ++ "\r\n"
              putStr "\r\n"
              putStr body

cgiEnvs = return . catMaybes =<< mapM mGetEnvPair names
  where
    mGetEnvPair :: String -> IO (Maybe (String, String))
    mGetEnvPair name =
      catchIOError (return . Just . (,) name =<< getEnv name)
                   (const $ return Nothing)

    names = [ "SERVER_NAME", "SERVER_PORT",
              "SERVER_SOFTWARE", "SERVER_PROTOCOL",
              "GATEWAY_INTERFACE", "SCRIPT_NAME", "REQUEST_METHOD",
              "PATH_INFO", "PATH_TRANSLATED",
              "CONTENT_TYPE", "CONTENT_LENGTH", "QUERY_STRING",
              "HTTP_COOKIE", "HTTP_ACCEPT",
              "REMOTE_HOST", "REMOTE_ADDR", "REMOTE_USER",
              "AUTH_TYPE", "HTTPS" ]

data HTTPRequest = HTTPRequest { params :: [(String, String)] }

parseCGIRequest env input =
    case method of
      "GET"  -> parseGET env
      "POST" -> parsePOST env input
      _      -> parseUnknown
  where
    method = getenv "REQUEST_METHOD" env

    getenv key env = fromMaybe "" $ lookup key env

    parseGET env = HTTPRequest (parseQueryString $ getenv "QUERY_STRING" env)

    parsePOST env input = HTTPRequest (parseQueryString $ input)

    -- FIXME
    parseUnknown = HTTPRequest []

    parseQueryString = map splitKV . splitQueryString

    splitQueryString = splitBy (\c -> c == ';' || c == '&')

    splitKV kv = case break (== '=') kv of
                   (k, ('=':v)) -> (decodeWord k, decodeWord v)
                   (k, "")      -> (decodeWord k, "")

    decodeWord = urldecode . decodePlus

    decodePlus = map (\c -> if c == '+' then ' ' else c)

splitBy :: (Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy f str = word : splitBy f cont
  where
    (word, cont') = break f str
    cont = case cont' of
             []     -> ""
             (c:cs) -> cs

varExist :: String -> HTTPRequest -> Bool
varExist key = isJust . lookupVar key

lookupVar :: String -> HTTPRequest -> Maybe String
lookupVar key = lookup key . params

lookupVars :: String -> HTTPRequest -> [String]
lookupVars key = lookupAll key . params

lookupAll :: Eq a => a -> [(a,b)] -> [b]
lookupAll key = map snd . filter ((== key) . fst)

data HTTPResponse = HTTPResponse {
    resContentType :: String,
    resBody :: String
}

textContentType typ encoding = concat [typ, "; charset=\"", encoding, "\""]
