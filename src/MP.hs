-----------------------------------------------------------------------------
-- |
-- Module      :  MP
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  portable

module MP
    ( applyMP
    )
    where
import P
import IL
import Translation (catToType)

applyMP :: Expr -> Expr
-- MP1
-- MP4
applyMP (Const tv) | fst tv `elem` ["find","lose","eat","love"] =
    lambda p $ lambda x $
    FVar p <@> (int $ lambda y $ int (Const tv') <@> FVar y :@ FVar x)
  where
    tv' = (fst tv ++ "*", E :-> E :-> Prop)
    p = ("p", S ((S (E :-> Prop)) :-> Prop))
    x = ("x", E)
    y = ("y", E)
-- MP8
applyMP (Const ("in",_)) =
    lambda p $ lambda pp $ lambda x $
    FVar p <@> (int $ lambda y $ int (Const in') <@> FVar y :@ FVar pp :@ FVar x)
  where
    in' = ("in*", E :-> (S (E :-> Prop)) :-> E :-> Prop)
    p = ("p", S ((S (E :-> Prop)) :-> Prop))
    pp = ("P", S (E :-> Prop))
    x = ("x", E)
    y = ("y", E)
-- MP9
applyMP (Const ("seek",_)) = lambda p $ Const try' :@ int (find' :@ FVar p)
  where
    try' = ("try", catToType (IV :// IV))
    find' = applyMP (Const ("find", catToType (cat :: Cat TV)))
    p = ("P", S ((S (E :-> Prop)) :-> Prop))
applyMP (Bind q t (Sc body)) = Bind q t (Sc (applyMP body))
applyMP (fun :@ arg) = applyMP fun :@ applyMP arg
applyMP (Op1 op a)   = Op1 op (applyMP a)
applyMP (Op2 op a b) = Op2 op (applyMP a) (applyMP b)
applyMP x = x
