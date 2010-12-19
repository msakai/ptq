-----------------------------------------------------------------------------
-- |
-- Module      :  URLEncoding
-- Copyright   :  Copyright (c) 2005,2006 Minero Aoki
-- License     :  LGPL (see COPYING)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

{-# LANGUAGE CPP #-}
--
-- $Id: URLEncoding.hs,v 1.2 2006/04/05 17:55:14 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module URLEncoding (urlencode, urldecode) where

#if __GLASGOW_HASKELL__ > 602
import Network.URI (escapeURIString, unEscapeString)
#else
import Network.URI (escapeString, unEscapeString)
#endif
import Data.Char (isAlphaNum)

#if __GLASGOW_HASKELL__ > 602
urlencode = escapeURIString (isAlphaNum)
#else
urlencode str = escapeString str (isAlphaNum)
#endif
urldecode = unEscapeString
