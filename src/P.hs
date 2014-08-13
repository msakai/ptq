-----------------------------------------------------------------------------
-- |
-- Module      :  P
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

{-# LANGUAGE TypeOperators, GADTs, EmptyDataDecls, MultiParamTypeClasses
  , TypeSynonymInstances, FlexibleInstances #-}

module P
  ( Sen
  , IV
  , CN
  , Adj
  , (:/)
  , (://)
  , Cat (..)
  , CatType (..) 
  , T
  , TV
  , IAV
  , Det
  , DTV
  , TTV
  , PP
  , PronounNo
  , P (..)
  , PAny (..)
  , F5
  , F6
  , F8
  , F9
  , F10
  , catOf
  ) where

-----------------------------------------------------------------------------

infixl 1 :/
infixl 1 ://

-- 型レベルでの範疇の表現
data Sen -- 文。本来はtと書かれるけど小文字が使えないので
data IV  -- 動詞句, 自動詞: walk
data CN  -- 普通名詞(句): man
data Adj -- 形容詞? (定義が載っていなかった)
data (:/) a b
data (://) a b

-- 範疇を型レベルだけでなく値レベルでも表現
data Cat a where
    Sen   :: Cat Sen
    IV    :: Cat IV
    CN    :: Cat CN
    Adj   :: Cat Adj
    (:/)  :: Cat a -> Cat b -> Cat (a :/ b)
    (://) :: Cat a -> Cat b -> Cat (a :// b)

class CatType a where
  cat :: Cat a
instance CatType Sen where
  cat = Sen
instance CatType IV where
  cat = IV
instance CatType CN where
  cat = CN
instance CatType Adj where
  cat = Adj
instance (CatType a, CatType b) => CatType (a :/ b) where
  cat = cat :/ cat
instance (CatType a, CatType b) => CatType (a :// b) where
  cat = cat :// cat

instance Show (Cat a) where
  showsPrec _ Sen = showString "t"
  showsPrec _ IV = showString "IV"
  showsPrec _ CN = showString "CN"
  showsPrec _ Adj = showString "Adj"
  showsPrec d (a :/ b) = showParen (d > 0) $ showsPrec 1 a . showString " / " . showsPrec 1 b
  showsPrec d (a :// b) = showParen (d > 0) $ showsPrec 1 a . showString " // " . showsPrec 1 b

type T   = Sen :/ IV -- 名詞句, 固有名詞: John, he
type TV  = IV :/ T   -- 他動詞(句): find
type IAV = IV :/ IV  -- 動詞句修飾の副詞(句): slowly

-- FIXME: 定義が載っていなかった範疇
type Det = T  :/ CN -- 冠詞
type DTV = TV :/ T  -- ditransitive verb : 名詞句を２つ取る他動詞
type TTV = TV :/ T  -- to transitive verb : DTVと名詞句の順序が逆?
type PP  = IV :/ TV -- prepositional phrase?: by John

-----------------------------------------------------------------------------

type PronounNo = Int

-- 範疇がcである表現
data P c where
    -- 基本表現
    B  :: Cat c -> String -> P c
    He :: PronounNo -> P T

    -- 統語規則
    -- F1はないのね
    F2  :: P Det -> P CN -> P T
    F3  :: PronounNo -> P CN -> P Sen -> P CN
    F4  :: P T -> P IV -> P Sen
    F5  :: F5 c => P (c :/ T) -> P T -> P c
    F6  :: F6 c a => P (c :/ a) -> P a -> P c
    F7  :: P IAV -> P IV -> P IV
    F8  :: F8 c => P c -> P c -> P c -- and
    F9  :: F9 c => P c -> P c -> P c -- or
    F10 :: F10 c => PronounNo -> P T -> P c -> P c -- 文の中への量化
    F11 :: P T -> P IV -> P Sen
    F12 :: P T -> P IV -> P Sen
    F13 :: P T -> P IV -> P Sen
    F14 :: P T -> P IV -> P Sen
    F15 :: P T -> P IV -> P Sen
    F16 :: P (IV :/ Sen) -> P Sen -> P IV
    F17 :: P (IV :// IV) -> P IV -> P IV
    -- F18はないのね
    F19 :: P TV -> P IV
    F20 :: P DTV -> P T -> P TV
    F21 :: P DTV -> P T -> P TV
    F22 :: P DTV -> P TTV
    F23 :: P PP -> P TV -> P IV
    F24 :: P (PP :/ T) -> P T -> P PP
    F25 :: P TV -> P Adj

data PAny where
    PAny :: CatType c => P c -> PAny

class CatType c => F5 c 
instance F5 IV  -- T5
instance F5 IAV -- T6

class (CatType c, CatType a) => F6 c a
instance F6 Sen Sen -- T9
instance F6 IV Adj  -- T18

class CatType c => F8 c
instance F8 Sen
instance F8 IV

class CatType c => F9 c
instance F9 Sen
instance F9 IV
instance F9 T

class CatType c => F10 c
instance F10 Sen
instance F10 CN
instance F10 IV

catOf :: CatType c => P c -> Cat c
catOf _ = cat

-----------------------------------------------------------------------------

-- Ughhh!!
instance Show (P c) where
    showsPrec _ (B _ s)     = showString s
    showsPrec d (He n)      = c1 d "He" n
    showsPrec d (F2 x y)    = c2 d "F2" x y
    showsPrec d (F3 n x y)  = c3 d "F3" n x y
    showsPrec d (F4 x y)    = c2 d "F4" x y
    showsPrec d (F5 x y)    = c2 d "F5" x y
    showsPrec d (F6 x y)    = c2 d "F6" x y
    showsPrec d (F7 x y)    = c2 d "F7" x y
    showsPrec d (F8 x y)    = c2 d "F8" x y
    showsPrec d (F9 x y)    = c2 d "F9" x y
    showsPrec d (F10 n x y) = c3 d "F10" n x y
    showsPrec d (F11 x y)   = c2 d "F11" x y
    showsPrec d (F12 x y)   = c2 d "F12" x y
    showsPrec d (F13 x y)   = c2 d "F13" x y
    showsPrec d (F14 x y)   = c2 d "F14" x y
    showsPrec d (F15 x y)   = c2 d "F15" x y
    showsPrec d (F16 x y)   = c2 d "F16" x y
    showsPrec d (F17 x y)   = c2 d "F17" x y
    showsPrec d (F19 x)     = c1 d "F19" x
    showsPrec d (F20 x y)   = c2 d "F20" x y
    showsPrec d (F21 x y)   = c2 d "F21" x y
    showsPrec d (F22 x)     = c1 d "F22" x
    showsPrec d (F23 x y)   = c2 d "F23" x y
    showsPrec d (F24 x y)   = c2 d "F24" x y
    showsPrec d (F25 x)     = c1 d "F25" x

instance Show PAny where
  showsPrec d (PAny p) = c1 d "PAny" p

c1 :: (Show x) => Int -> String -> x -> ShowS
c1 d con x =
    showParen (d > app_prec) $
    showString con
    . showChar ' ' . showsPrec (app_prec+1) x
c2 :: (Show x, Show y) => Int -> String -> x -> y -> ShowS
c2 d con x y =
    showParen (d > app_prec) $
    showString con
    . showChar ' ' . showsPrec (app_prec+1) x
    . showChar ' ' . showsPrec (app_prec+1) y
c3 :: (Show x, Show y, Show z) => Int -> String -> x -> y -> z -> ShowS
c3 d con x y z =
    showParen (d > app_prec) $
    showString con
    . showChar ' ' . showsPrec (app_prec+1) x
    . showChar ' ' . showsPrec (app_prec+1) y
    . showChar ' ' . showsPrec (app_prec+1) z
app_prec :: Int
app_prec = 10
