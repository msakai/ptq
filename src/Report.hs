-----------------------------------------------------------------------------
-- |
-- Module      :  Report
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  portable

module Report (report) where
import IL
import P
import Parser
import MP
import Translation
import Data.List (intersperse)

report :: String -> String
report s = unlines $
  case parseAny s of
    [] -> ["(parse error)"]
    ps -> concat $ intersperse ["",sep,""] (map f ps)

f :: PAny -> [String]
f (PAny p) = 
    [ "Parsed:"
    , "  " ++ show p
    , "  : " ++ show c
    , ""
    , "Translation:"
    , "  " ++ show e
    , "  : " ++ show t
    , ""
    , "Translation (simplified):"
    , "  " ++ show e'
    , "  : " ++ show t
    , ""
    , "Translation (MP applied):"
    , "  " ++ show e''
    , "  : " ++ show t
    ]
    where
      e   = translate p
      e'  = normalize e
      e'' = normalize $ applyMP $ e'
      c = catOf p
      t = catToType c

sep :: String
sep = "------------------------------------------------------"
