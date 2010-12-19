-----------------------------------------------------------------------------
-- |
-- Module      :  ReportHTML
-- Copyright   :  (c) Masahiro Sakai 2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  portable

module ReportHTML (report) where
import IL
import P
import Parser
import MP
import Translation
import Data.List (intersperse)
import Text.XML.Light

report :: String -> String
report s = showTopElement $ html [hd (title ("Result of \"" ++ s ++ "\"")), body b]
  where
    b = case parseAny s of
          [] -> [p "(parse error)"]
          ps -> intersperse sep (map f ps)

f :: PAny -> Element
f (PAny p) = dl
    [ dt "Parsed"
    , dd (show p ++ " : " ++ show c)
    , dt "Translation"
    , dd (show e ++ " : " ++ show t)
    , dt "Translation (simplified)"
    , dd (show e' ++ " : " ++ show t)
    , dt "Translation (MP applied)"
    , dd (show e'' ++ " : " ++ show t)
    ]
    where
      e   = translate p
      e'  = normalize e
      e'' = normalize $ applyMP $ e'
      c = catOf p
      t = catToType c

html :: Node t => t -> Element
html c = add_attr (Attr xmlns ns) $ node QName{ qName = "html", qURI = Just ns, qPrefix = Nothing } c

xmlns :: QName
xmlns = QName{ qName = "xmlns", qURI = Nothing, qPrefix = Nothing}

ns :: String
ns = "http://www.w3.org/1999/xhtml"

hd :: Node t => t -> Element
hd = node QName{ qName = "head", qURI = Just ns, qPrefix = Nothing }

title :: Node t => t -> Element
title = node QName{ qName = "title", qURI = Just ns, qPrefix = Nothing }

body :: Node t => t -> Element
body = node QName{ qName = "body", qURI = Just ns, qPrefix = Nothing }

p :: Node t => t -> Element
p = node QName{ qName = "p", qURI = Just ns, qPrefix = Nothing }

dl :: Node t => t -> Element
dl = node QName{ qName = "dl", qURI = Just ns, qPrefix = Nothing }

dt :: Node t => t -> Element
dt = node QName{ qName = "dt", qURI = Just ns, qPrefix = Nothing }

dd :: Node t => t -> Element
dd = node QName{ qName = "dd", qURI = Just ns, qPrefix = Nothing }

sep :: Element
sep = node QName{ qName = "hr", qURI = Just ns, qPrefix = Nothing } ()
