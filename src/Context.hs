-----------------------------------------------------------------------------
-- |
-- Module      :  Context
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

class Context a

data Empty
instance Context Empty

infixl 9 :*
infixl 9 :@

data (:*) c a 
instance Context c => Context (c :* a)

class Append c d e | c d -> e
instance Append c Empty c
instance Append c d e => Append c (d :* a) (e :* a)

data BVar ctx a where
    B0 :: BVar (c :* a) a
    BS :: BVar c a -> BVar (c :* b) a

data Expr c t where
    B    :: BVar c t -> Expr c t
    (:@) :: Expr c (a->b) -> Expr c a -> Expr c b
    Lam  :: Expr (c :* a) b -> Expr c (a->b)

type Name = String

{-
inst :: Expr (c :* a) b -> Expr c a -> Expr c b
inst (B v) x = case v of
               B0    -> x
               BS v  -> B v
inst (a :@ b) x = inst a x :@ inst b x
inst (Lam a) x  =
    case x of
    B B0     -> 
    B (BS v) -> 
-}

{-
f :: forall c d a. Expr c a -> (forall a. BVar c a -> Expr d a) -> Expr d a
f x g =
    case x of
    a :@ b -> f a g :@ f b g
    B x -> g x
    Lam y -> Lam (f y g')
        where g' :: forall b z. BVar (c :* z) b -> Expr (d :* z) b
              g' B0     = B B0
              g' (BS x) = let foo :: Expr d b
                              foo = g x
                              bar :: Expr (d :* z) b
                              bar = shift foo
                          in bar
-}

shift' :: forall c d e e' u t.
          (Append c d e, Append (c :* u) d e') =>
          (forall t. BVar e t -> BVar e' t) ->
          Expr e t -> Expr e' t
shift' v (a :@ b) = shift' v a :@ shift' v b
shift' v (B x)    = B (v x)
shift' v (Lam x)  = Lam (shift' v' x)
    where v' B0     = B0
          v' (BS x) = BS (v x)

shift0 :: Expr c t -> Expr (c :* u) t
shift0 (a :@ b) = shift0 a :@ shift0 b
shift0 (B x)    = B (v0 x)
shift0 (Lam x)  = Lam (shift1 x)

v0 :: BVar c t -> BVar (c :* u) t
v0 = BS

shift1 :: Expr (c :* a) t -> Expr (c :* u :* a) t
shift1 (a :@ b) = shift1 a :@ shift1 b
shift1 (B x)    = B (v1 x)
shift1 (Lam x)  = Lam (shift2 x)

v1 :: BVar (c :* a) t -> BVar (c :* u :* a) t
v1 B0     = B0
v1 (BS x) = BS (v0 x)

shift2 :: Expr (c :* a :* b) t -> Expr (c :* u :* a :* b) t
shift2 (a :@ b) = shift2 a :@ shift2 b
shift2 (B x)    = B (v2 x)
shift2 (Lam x)  = undefined

v2 :: BVar (c :* a :* b) t -> BVar (c :* u :* a :* b) t
v2 B0     = B0
v2 (BS x) = BS (v1 x)
