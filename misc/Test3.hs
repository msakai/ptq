{-# OPTIONS_GHC -fallow-undecidable-instances #-}
-- coverage condition を満たしていないインスタンス宣言があるため

-----------------------------------------------------------------------------

data Prop
data S
data E

infixl 9 :@

data Expr t where
    BVar :: Int -> Expr t
    FVar :: String -> Expr t
    (:@) :: Expr (a->b) -> Expr a -> Expr b
    Lam  :: Expr b -> Expr (a->b)
    Int  :: Expr a -> Expr (S->a)
    Ext  :: Expr (S->a) -> Expr a

-----------------------------------------------------------------------------

infixl 1 :/

data Sen
data IV
data CN
data (:/) a b

type T   = Sen :/ IV
type TV  = IV :/ T
type IAV = IV :/ IV

data P c where
    F :: CatToType b u => P (a :/ b) -> P b -> P a

class CatToType c t | c -> t
instance CatToType Sen Prop
instance CatToType IV (E->Prop)
instance CatToType CN (E->Prop)
instance (CatToType a t, CatToType b u) => CatToType (a :/ b) ((S->u)->t)

translate :: CatToType c t => P c -> Expr t
translate (F fun arg) = translate fun :@ Int (translate arg)

-----------------------------------------------------------------------------

{-
class Assoc a b | a -> b
instance Assoc Int Int

f :: Assoc Int b => Int -> b
f x = x
-}
