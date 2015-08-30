{-# OPTIONS_GHC -fglasgow-exts #-}

{-# OPTIONS_GHC -fallow-undecidable-instances #-}
-- coverage condition を満たしていないインスタンス宣言があるため

-----------------------------------------------------------------------------
-- Expr.hs 

-- Benetteの意味タイプ
data E       -- 個体
data Prop    -- 本来はtと書かれるけど、小文字が使えないので
data S a
-- 関数型はHaskellの -> をそのまま使う

infixl 9 :@

-- 意味タイプがtである式
data Expr t where
    FVar :: Name -> Expr t
    BVar :: !Int -> Expr t
    Lam  :: Name -> Scope b -> Expr (a -> b)
    Forall :: Name -> Scope Prop -> Expr Prop
    Exists :: Name -> Scope Prop -> Expr Prop
    (:@) :: Expr (a -> b) -> Expr a -> Expr b
    C :: C t -> Expr t

type Name = String
newtype Scope t = Sc (Expr t)

data C t where
    John' :: C E
    Mary' :: C E
    Bill' :: C E
    Int   :: C (a -> S a)

abstract :: Name -> Expr t -> Scope t
abstract name expr = Sc (h 0 expr)
  where
    h outer (FVar name') | name==name' = BVar 0
                         | otherwise   = FVar name'
    h outer (BVar index) = BVar index

-- instantiate :: Expr u -> Scope t -> Expr t

-----------------------------------------------------------------------------

-- import Expr.hs


-----------------------------------------------------------------------------
