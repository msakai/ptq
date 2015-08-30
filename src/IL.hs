-----------------------------------------------------------------------------
-- |
-- Module      :  IL
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

{-# LANGUAGE CPP #-}
module IL
    ( Type (..)
    , renderType
    , Op1 (..)
    , Op2 (..)
    , Binder (..)
    , Expr (..)
    , Scope (..)
    , Name
    , lambda
    , forall
    , exists
    , int
    , ext
    , (<@>)
    , abstract
    , instantiate
    , normalize
    , renderExpr
    , typeCheck
    ) where

import Control.Monad.RWS
import Data.List
import Data.Monoid
import Data.Function

-- --------------------------------------------------------------------------

infixr 0 :->

data Type
  = Prop
  | E
  | S Type
  | (:->) Type Type
  deriving (Eq, Ord)

instance Show Type where
#ifdef USE_UTF8
  showsPrec = renderType True
#else
  showsPrec = renderType False
#endif

renderType :: Bool -> Int -> Type -> ShowS
renderType unicode = f
  where
    f _ Prop = showString "t"
    f _ E = showString "e"
    f d (S t) = showParen (d > 0) $ showString "s" . arr . f 0 t
    f d (t1 :-> t2) = showParen (d > 0) $ f 1 t1 . arr . f 0 t2
    arr = if unicode then showString "→" else showString "->"

-- --------------------------------------------------------------------------

infixl 9 :@

data Expr
    = FVar Name          -- 自由変数
    | BVar !Int          -- 束縛変数
    | Expr :@ Expr       -- 関数適用
    | Const Name         -- 定数 (≒自由変数)
    | Op1 !Op1 Expr      -- 前置演算子
    | Op2 !Op2 Expr Expr -- 中置演算子
    | Bind !Binder Type Scope -- 変数束縛

data Op1 = Not | Box | F | H | Int | Ext deriving (Eq,Ord,Show)
data Op2 = And | Or | Imply | Equiv | Id deriving (Eq,Ord,Show)
data Binder = Lambda | Forall | Exists deriving (Eq,Ord,Show)

newtype Scope = Sc Expr deriving Show
type Name = (String, Type)

instance Show Expr where
#ifdef USE_UTF8
    showsPrec = renderExpr True False
#else
    showsPrec = renderExpr False False
#endif

lambda :: Name -> Expr -> Expr
lambda name expr = Bind Lambda (snd name) (abstract name expr)

forall :: Name -> Expr -> Expr
forall name expr = Bind Forall (snd name) (abstract name expr)

exists :: Name -> Expr -> Expr
exists name expr = Bind Exists (snd name) (abstract name expr)

int :: Expr -> Expr
int = Op1 Int

ext :: Expr -> Expr
ext = Op1 Ext

-- 「a{b}」を「a <@> b」と表記
infixl 9 <@>
(<@>) :: Expr -> Expr -> Expr
fun <@> arg = ext fun :@ arg

varChange :: (Int -> Name -> Expr) -> (Int -> Int -> Expr) -> Expr -> Expr
varChange f g = h 0
  where
    h :: Int -> Expr -> Expr
    h outer (FVar name)  = f outer name
    h outer (BVar index) = g outer index
    h outer (Bind q t (Sc body)) = Bind q t (Sc (h (outer+1) body))
    h outer (fun :@ arg) = h outer fun :@ h outer arg
    h _     (Const s)    = Const s
    h outer (Op1 op a)   = Op1 op (h outer a)
    h outer (Op2 op a b) = Op2 op (h outer a) (h outer b)

abstract :: Name -> Expr -> Scope
abstract name expr = Sc (varChange f g expr)
  where
    f outer name' | name==name'  = BVar outer
                  | otherwise    = FVar name'
    g outer index | index>=outer = BVar (index+1)
                  | otherwise    = BVar index

instantiate :: Expr -> Scope -> Expr
instantiate image (Sc body) = varChange f g body
  where
    f _ name = FVar name
    g outer index | index==outer = varShift outer image
                  | index>outer  = BVar (index-1)
                  | otherwise    = BVar index

-- 外を指している変数のインデックスをずらす
varShift :: Int -> Expr -> Expr
varShift 0 = id
varShift n = varChange f g 
  where
    f _ name  = FVar name
    g outer index | index>=outer = BVar (index+n)
                  | otherwise    = BVar index

normalize :: Expr -> Expr
normalize (Bind Lambda t (Sc body)) =
  case normalize body of
    f :@ BVar 0 | not (0 `elem` bvs f) -> varShift (-1) f -- η-conversion
    body' -> Bind Lambda t (Sc body')
normalize (Bind q t (Sc body)) = Bind q t (Sc (normalize body))
normalize (fun :@ arg) =
  case normalize fun of
    Bind Lambda t body -> normalize (instantiate arg' body) -- β-reduction
    fun' -> fun' :@ arg'
  where arg' = normalize arg
normalize (Op1 Ext a)  =
  case normalize a of
    Op1 Int b -> b
    a' -> Op1 Ext a'
normalize (Op1 op a)   = Op1 op (normalize a)
normalize (Op2 op a b) = Op2 op (normalize a) (normalize b)
normalize x = x

bvs :: Expr -> [Int]
bvs (FVar _) = []
bvs (BVar n) = [n]
bvs (f :@ x) = bvs f ++ bvs x
bvs (Const _) = []
bvs (Op1 _ e) = bvs e
bvs (Op2 _ e1 e2) = bvs e1 ++ bvs e2
bvs (Bind _ _ (Sc e)) = [n - 1 | n <- bvs e, n /= 0]

-- --------------------------------------------------------------------------

type RenderM = RWS [Name] () Int

renderExpr :: Bool -> Bool -> Int -> Expr -> ShowS
renderExpr unicode uncurrying d e =
  case runRWS (h d e) [] 0 of
    (a, _, _) -> a
  where
    h d e = case e of
      FVar name  -> return $ showString (fst name)
      BVar index -> do
        vs <- ask
        return $ showString $ fst (vs !! index)
      Bind q _ _ -> f d q e
      a :@ b | uncurrying -> f a [b]
        where
          f (e1 :@ e2) xs = f e1 (e2:xs)
          f e xs = uncurriedApp e xs
      Op1 Ext a :@ b -> do
        a' <- h (app_prec+1) a
        b' <- h 0 b
        return $ showParen (d > app_prec)
               $ a' . showString " {" . b' . showChar '}'
      a :@ b  -> do 
        a' <- h app_prec a
        b' <- h (app_prec+1) b
        return $ showParen (d > app_prec) $ a' . showChar ' ' . b'
      Const s -> return $ showString (fst s)
      Op1 op a -> do
        t <- h (prec+1) a
        return $ showParen (d > prec)
               $ showString s . t
        where
          s = case op of
                Not -> if unicode then "¬" else "not " -- ¬ (U+00AC)
                Box -> if unicode then "◻" else "[]"  -- ◻ (U+25FB) が正しそうだが □ (U+25A1) を使うのが無難か?
                F   -> "F "
                H   -> "H "
                Int -> if unicode then "˄" else "Int " -- ˄ (U+02C4)
                Ext -> if unicode then "˅" else "Ext " -- ˅ (U+02C5)
          prec = case op of
                   Int | unicode -> app_prec + 1
                   Ext | unicode -> app_prec + 1
                   _ -> app_prec
      Op2 op a b -> do
        a' <- h (l prec) a
        b' <- h (r prec) b
        return $ showParen (d > prec)
               $ a' . showChar ' ' . showString s . showChar ' ' . b'
        where
          (s,prec,l,r) =
            case op of
              And   -> (if unicode then "∧" else "&&",  4, id, id)     -- ∧ (U+2227)
              Or    -> (if unicode then "∨" else "||",  3, id, id)     -- ∨ (U+2228)
              Imply -> (if unicode then "→" else "->",  1, (+1), id)   -- → (U+2192)
              Equiv -> (if unicode then "↔" else "<->", 1, (+1), (+1)) -- ↔ (U+2194)
              Id    -> ("=", 5, (+1), (+1))

    f :: Int -> Binder -> Expr -> RenderM ShowS
    f d b e = do
      (xs, s) <- go e
      let b' = case b of
            Lambda -> if unicode then "λ" else "\\" -- λ (U+03BB)
            Forall -> if unicode then "∀" else "forall " -- ∀ (U+2200)
            Exists -> if unicode then "∃" else "exists " -- ∃ (U+2203)
          ys :: [(Type, [String])]
          ys = [(snd (head ys), map fst ys) | ys <- groupBy ((==) `on` snd) xs]
          ws :: [String]
          ws = [intercalate ", " zs ++ " : "++ renderType unicode 0 t "" | (t, zs) <- ys]
      return $ showParen (d > 0) $
        showString b' . showString (intercalate ", " ws) . showString ". " . s
      where
        go :: Expr -> RenderM ([Name], ShowS)
        go (Bind b' t (Sc body)) | b==b' = do
          x <- gensym t
          (xs, s) <- local (x:) $ go body
          return (x:xs, s)
        go e = do
          s <- h 0 e
          return ([], s)

    uncurriedApp :: Expr -> [Expr] -> RenderM ShowS
    uncurriedApp e xs = do
      bs <- mapM (liftM Endo . h 0) $ reverse xs
      let cs = appEndo $ mconcat $ intersperse (Endo (showString ", ")) bs
      case e of
        Op1 Ext e2 -> do
          a <- h (app_prec+1) e2
          return $ showParen (d > app_prec)
                 $ a . showString "{" . cs . showChar '}'
        _ -> do
          a <- h (app_prec+1) e
          return $ showParen (d > app_prec)
                 $ a . showString "(" . cs . showChar ')'

app_prec :: Int
app_prec = 10

gensym :: Type -> RenderM Name
gensym t = do 
  i <- get
  put (i+1)
  return ("x"++show i, t)

-- ---------------------------------------------------------------------------

typeCheck ::[Type] -> Expr -> Maybe Type
typeCheck = f
  where
    f _ (FVar (_,t)) = return t
    f env (BVar n) = return (env !! n)
    f env (e1 :@ e2) = do
      (t1 :-> t2) <- f env e1
      t3 <- f env e2
      guard $ t1 == t3
      return t2
    f env (Const (_,t)) = return t
    f env (Op1 Ext e) = do
      S t <- f env e
      return t
    f env (Op1 Int e) = do
      t <- f env e
      return (S t)
    f env (Op1 op e) = do
      Prop <- f env e
      return Prop
    f env (Op2 Id e1 e2) = do
      t1 <- f env e1
      t2 <- f env e2
      guard $ t1 == t2
      return Prop
    f env (Op2 op e1 e2) = do
      Prop <- f env e1
      Prop <- f env e2
      return Prop
    f env (Bind Lambda t (Sc e)) = do
      t2 <- f (t:env) e
      return (t :-> t2)
    f env (Bind b t (Sc e)) = do
      Prop <- f (t:env) e
      return Prop
