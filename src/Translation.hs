-----------------------------------------------------------------------------
-- |
-- Module      :  Translation
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

{-# LANGUAGE TypeOperators, GADTs, TypeSynonymInstances, ScopedTypeVariables #-}

module Translation (translate, catToType) where

import IL
import P

-----------------------------------------------------------------------------

{-
-- 範疇から型への対応
type family Translate x
type instance Translate Sen = Prop
type instance Translate IV = E -> Prop
type instance Translate CN = E -> Prop
type instance Translate Adj = E -> Prop
type instance Translate (a :/ b) = ((S -> Translate b) -> Translate a)
type instance Translate (a :// b) = ((S -> Translate b) -> Translate a)
-}

catToType :: Cat c ->  Type
catToType Sen = Prop
catToType IV = E :-> Prop
catToType CN = E :-> Prop
catToType Adj = E :-> Prop -- これで本当にあっている?
catToType (a :/ b) = (S (catToType b)) :-> catToType a
catToType (a :// b) = (S (catToType b)) :-> catToType a

-----------------------------------------------------------------------------

translate :: forall c. P c -> Expr
--- 1. αがgの定義域にあれば，α は，g(α) に翻訳される．
-- 最後に
--- 2. be → λp.λx. p{f^λy.[x = y]}.
---    ここで，変数pのタイプは<s, <<s, <e, t>>, t>>.
translate (B (IV :/ (Sen :/ IV)){- TV -} "be") =
  lambda p $ lambda x $ 
    FVar p <@> int (lambda y $ Op2 Id (FVar x) (FVar y))
  where
    p = ("p", S (S (E :-> Prop) :-> Prop))
    x = ("x", E)
    y = ("y", E)
--- 3. necessarily → λp[□ext p]. ここで，p のタイプは<s, t>とする．
translate (B (Sen :/ Sen) "necessarily") = lambda p $ Op1 Box (ext (FVar p))
  where
    p = ("p", S Prop)
--- 4. j, m, b はタイプがe の定数記号，変数P のタイプは<s, <e, t>>とする．
translate (B (Sen :/ IV){- T -} x) = lambda p $ FVar p <@> Const (x, E)
  where
    p = ("p", S (catToType IV))
--- 5. he_n → λP. P {x_n}．x_ はタイプe の変数．
translate (He n) = lambda p $ FVar p <@> FVar (xn n)
  where
    p = ("p", S (E :-> Prop))
translate (F2 delta zeta) = trApp delta zeta -- T2
translate (F3 n zeta phi) = -- T3
    lambda (xn n) $ Op2 And (translate zeta :@ FVar (xn n)) (translate phi)
translate (F4 alpha delta) = trApp alpha delta -- T4
translate (F5 delta beta)  = trApp delta beta  -- T5
-- T6 (T5と同じなので省略)
translate (F16 delta phi)  = trApp delta phi   -- T7
translate (F17 delta beta) = trApp' delta beta -- T8
translate (F6 delta beta)  = trApp delta beta  -- T9
translate (F7 delta beta)  = trApp delta beta  -- T10
translate (F8 phi psi) =
  case cat :: Cat c of
    Sen -> Op2 And (translate phi) (translate psi) -- T11a
    IV -> lambda x $ Op2 And (translate phi :@ FVar x) (translate psi :@ FVar x) -- T12a
  where
    x = ("x", E)
translate (F9 phi psi) =
  case cat :: Cat c of
    Sen -> Op2 Or (translate phi) (translate psi) -- T11b
    IV -> lambda x $ Op2 Or (translate phi :@ FVar x) (translate psi :@ FVar x) -- T12b
      where x = ("x", E)
    Sen :/ IV -> lambda p $ Op2 Or (translate phi :@ FVar p) (translate psi :@ FVar p) -- T13
      where p = ("P", S (E :-> Prop))
-- T14 (講義資料はx_nになるべきところがxになっている)
translate (F10 n alpha phi) =
  case cat :: Cat c of
    Sen -> translate alpha :@ (int $ lambda (xn n) (translate phi)) -- T14
    CN -> lambda y $ translate alpha :@ int (lambda (xn n) (translate phi :@ FVar y)) -- T15
    IV -> lambda y $ translate alpha :@ int (lambda (xn n) (translate phi :@ FVar y)) -- T16
  where
    y = ("y", E)
-- T17
translate (F11 alpha delta) = Op1 Not         $ trApp alpha delta
translate (F12 alpha delta) =           Op1 F $ trApp alpha delta
translate (F13 alpha delta) = Op1 Not $ Op1 F $ trApp alpha delta
translate (F14 alpha delta) =           Op1 H $ trApp alpha delta
translate (F15 alpha delta) = Op1 Not $ Op1 H $ trApp alpha delta
-- T18 (beの扱い以外はT9と同じ)
translate (B (IV :/ Adj) "be") = lambda p $ lambda x $ FVar p <@> FVar x
  where
    p = ("P", S (E :-> Prop))
    x = ("x", E)
-- T19
translate (F19 delta) =
  lambda x $ exists y $
    translate delta :@ int (lambda p (FVar p <@> FVar y)) :@ FVar x
  where
    x = ("x", E)
    p = ("P", S (E :-> Prop))
    y = ("y", E)
translate (F20 delta beta) = trApp delta beta -- T20
translate (F21 delta beta) = trApp delta beta -- T21 (講義資料ではF20を誤って使っている)
-- T22
translate (F22 delta) =
  lambda p $ lambda q $ lambda x $
    translate delta :@ FVar q :@ FVar p :@ FVar x
  where
    p = ("P", S (catToType (cat :: Cat T)))
    q = ("Q", S (catToType (cat :: Cat T)))
    x = ("x", E)
translate (F23 alpha delta) = trApp alpha delta -- T23
translate (F24 alpha beta)  = trApp alpha beta  -- T24
-- 講義資料のByの解釈は誤り? (型が一致しない)
translate (B (IV :/ (IV :/ (Sen :/ IV)) :/ (Sen :/ IV)){- PP/T -} "by") = 
  lambda p $ lambda r $ lambda x $
    FVar p <@>
      (int $ lambda y $ FVar r <@> int (lambda q $ FVar q <@> FVar x) :@ FVar y)
  where
    p = ("P", S (catToType (Sen :/ IV)))
    r = ("R", S (catToType (IV :/ (Sen :/ IV))))
    x = ("x", E)
    y = ("y", E)
    q = ("Q", S (catToType IV))
-- T25
translate (F25 delta) =
  lambda x $ exists y $ Op1 H $
    translate delta :@
      int (lambda p $ FVar p <@> FVar x) :@
      (FVar y)
  where
    x = ("x", E)
    y = ("y", E)
    p = ("P", S (catToType IV))

-- Det
translate (B (Sen :/ IV :/ CN) s) =
  case s of
    "a"   ->
      lambda p $ lambda q $ exists x $
        Op2 And (FVar p <@> FVar x) (FVar q <@> FVar x)
    "the" ->
      lambda p $ lambda q $ exists y $ 
        Op2 And
          (forall x $ Op2 Equiv (FVar p <@> FVar x) (Op2 Id (FVar x) (FVar y)))
          (FVar q <@> FVar y)
    "every" ->
      lambda p $ lambda q $ forall x $
        Op2 Imply (FVar p <@> FVar x) (FVar q <@> FVar x)
    "no" ->
      lambda p $ lambda q $ forall x $
        Op1 Not (Op2 And (FVar p <@> FVar x) (FVar q <@> FVar x))
    _ -> Const (s, catToType (cat :: Cat Det))
  where
    p = ("p", S (E :-> Prop))
    q = ("q", S (E :-> Prop))
    x = ("x", E)
    y = ("y", E)

-- それ以外
translate (B c x) = Const (x, catToType c)

-- ユーティリティ
trApp :: P (b :/ a) -> P a -> Expr
trApp  f a = translate f :@ (int (translate a))
trApp' :: P (b :// a) -> P a -> Expr
trApp' f a = translate f :@ (int (translate a))

xn :: Int -> Name
xn n = ("he_"++show n, E)

