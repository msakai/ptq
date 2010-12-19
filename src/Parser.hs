----------------------------------------------------------------------------
-- |
-- Module      :  Parser
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

{-# LANGUAGE TypeOperators #-}
module Parser (parse, parseAny) where

import Data.Char
import Control.Monad
import qualified Data.IntSet as IS

import P

-----------------------------------------------------------------------------
-- パーサのコア部分

type Token = String

newtype Parser a
    = Parser
    { runParser :: Env -> State -> [Token] -> [(a, State, [Token])]
    }

instance Monad Parser where
    return x = Parser $ \_ s ts -> [(x,s,ts)]
    m >>= f = Parser $ \env s ts ->
              do (v,s',ts') <- runParser m env s ts
                 runParser (f v) env s' ts'

instance MonadPlus Parser where
    mzero = Parser $ \_ _ _ -> []
    mplus x y = Parser $ \env s ts ->
                runParser x env s ts ++ runParser y env s ts

infixr 0 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

anyToken :: Parser Token
anyToken = Parser g
    where g _ _ []     = []
          g _ s (t:ts) = [(t, s, ts)]

lookAhead :: Parser Token
lookAhead = Parser g
    where g _ _ []     = []
          g _ s (t:ts) = [(t, s, t:ts)]

-----------------------------------------------------------------------------
-- 環境

data Env
    = Env
    { s3env :: S3Env
    }

initialEnv :: Env
initialEnv = Env{ s3env = initialS3Env }

local :: (Env -> Env) -> Parser a -> Parser a
local f (Parser g) = Parser g'
    where g' env s ts = g (f env) s ts

ask :: Parser Env
ask = Parser g
    where g env s ts = [(env,s,ts)]

-----------------------------------------------------------------------------
-- 状態

data State
    = State
    { gensymState :: PronounNo
    , f10State    :: F10State
    }
initialState :: State
initialState = State{ gensymState = 0, f10State = initialF10State }

get :: Parser State
get = Parser g
    where g _ s xs = [(s,s,xs)]

put :: State -> Parser ()
put s = Parser g
    where g _ _ xs = [((),s,xs)]

-----------------------------------------------------------------------------
-- S3Env

type S3Env = [(PronounNo,Gender)] -- S3規則で使うためのデータ
initialS3Env :: S3Env
initialS3Env = []

localS3 :: (S3Env -> S3Env) -> Parser a -> Parser a
localS3 f = local (\env@Env{ s3env = x } -> env{ s3env = f x })

askS3 :: Parser S3Env
askS3 = liftM s3env ask

-----------------------------------------------------------------------------
-- Counter

gensym :: Parser PronounNo
gensym =
    do s@State{ gensymState = i } <- get
       put s{ gensymState = i+1 }
       return i

-----------------------------------------------------------------------------
-- F10のための処理

-- DVarStateみたいな名前にした方が良いか
type F10Entry = (PronounNo, Gender, P T, IS.IntSet)
type F10State = [F10Entry] -- F10で使うためのデータ
initialF10State :: F10State
initialF10State = []

getF10State :: Parser F10State
getF10State =
    do State{ f10State = s } <- get
       return s

putF10State :: F10State -> Parser ()
putF10State s =
    do x <- get
       put x{ f10State = s }

asPronoun :: Gender -> P T -> Parser (P T)
asPronoun g t =
    do n <- gensym
       s <- getF10State
       putF10State ((n,g,t,fvs t) : s)
       return (He n)

mayF10s :: F10 c => Parser (P c) -> Parser (P c)
mayF10s p = liftM fst $ mayF10s' $ p >>= \x -> return (x,())

mayF10s' :: F10 c => Parser (P c, a) -> Parser (P c, a)
mayF10s' p =
    do s <- getF10State
       putF10State []
       (x,a) <- localS3 ([(n,g) | (n,g,_,_)<-s]++) p
       x' <- introF10s x
       s' <- getF10State
       putF10State (s'++s)
       return (x',a)

introF10 :: F10 c => P c -> Parser (P c)
introF10 x =
    do (n, _, alpha, _) <- takeF10Entry
       return (F10 n alpha x)

introF10s :: F10 c => P c -> Parser (P c)
introF10s x = (introF10 x >>= introF10s) <|> return x

-- F10に変換可能なエントリを取り出す
takeF10Entry :: Parser F10Entry
takeF10Entry =
    do s <- getF10State
       (e@(n,_,_,_), s') <- msum $ map return $ pick s
       noDanglingRef n
       putF10State s'
       return e

pick :: [a] -> [(a,[a])]
pick []     = []
pick (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- pick xs] 

-- nへの参照が残っていないことを保障
noDanglingRef :: PronounNo -> Parser ()
noDanglingRef n =
    do s <- getF10State
       guard $ and [not (IS.member n vs) | (_,_,_,vs) <- s]

-----------------------------------------------------------------------------
-- パーサのユーティリティ

token :: Token -> Parser Token
token x =
    do y <- anyToken
       guard $ x==y
       return y

many, many1 :: Parser a -> Parser [a]
many p = many1 p <|> return []
many1 p =
    do x  <- p
       xs <- many p
       return (x:xs)

chainr1 :: Parser a -> Parser (a->a->a) -> Parser a
chainr1 p q =
    do x <- p
       f <- do{ op <- q; y <- chainr1 p q; return (`op` y) } <|> return id
       return (f x)

-----------------------------------------------------------------------------
-- API

parse :: String -> [P Sen]
parse = parse' p_t

parseAny :: String -> [PAny]
parseAny s = concat $
  [ f p_Det, f (liftM fst p_CN), f p_IAV_T, f p_t_t, f p_t, f p_Adj, f p_PP
  , f p_IAV, f p_PP_T, f (p_T [Subject, Object]) ] ++
  [ concat [ f (p_IV x), f (p_TV x), f (p_IV__IV x), f (p_DTV x), f (p_IV_t x)
           , f (p_IV_Adj x)] | x <- vfs ]
  where
    f :: CatType a => Parser (P a) -> [PAny]
    f p = map PAny (parse' p s)
    vfs :: [VerbForm]
    vfs = [VFOrig, VFPastParticiple] ++ [f True | f <- [VFPresent, VFFuture, VFPerfect]]
    -- 否定形の動詞それ自体はPで表現できないのでパース出来ない

parse' :: Parser (P a) -> String -> [P a]
parse' p s = [x | (x, State{ f10State = [] }, []) <- runParser p initialEnv initialState ts]
    where ts = tokenize s

tokenize :: String -> [Token]
tokenize = expandAbbr . words . map toLower . filter ('.'/=)

expandAbbr :: [String] -> [String]
expandAbbr = concatMap f
    where f "doesn't" = ["does", "not"]
          f "won't"   = ["will", "not"]
          f "hasn't"  = ["has",  "not"]
          f "isn't"   = ["is",   "not"]
          f x = [x]

-----------------------------------------------------------------------------
-- 各範疇のパーサ

p_Det :: Parser (P Det)
p_Det =
    do x@(B _ s) <- s1_Det
       let x' = if s=="an" then B (cat :: Cat Det) "a" else x -- XXX
       -- FIXME: anの場合には次の語が母音で始まるかチェック
       return $ x'

p_CN :: Parser (P CN, Gender)
p_CN = mayF10s' $ -- S15
    do (zeta,g) <- s1_CN
       zeta <- s3s g zeta
       return (zeta,g)

-- FIXME: 全ての組み合わせを網羅している?
p_T :: [Case] -> Parser (P T)
p_T cs = chainr1 (p <|> he_n cs) f9 -- S13
    where p = do (x,g) <- s1_T <|> s2
                 return x <|> asPronoun g x

-- He_n
he_n :: [Case] -> Parser (P T)
he_n cs =
    do g <- mplus
            (if Subject `elem` cs
             then msum [ token "he"  >> return Masculine
                       , token "she" >> return Feminine
                       , token "it"  >> return Neuter
                       ]
             else mzero)
            (if Object `elem` cs
             then msum [ token "him" >> return Masculine
                       , token "her" >> return Feminine
                       , token "it"  >> return Neuter
                       ]
             else mzero)
       xs <- getF10State
       ys <- askS3
       let ns = [(n,g') | (n, g', _, _) <- xs] ++ ys
       msum [ return (He n) | (n,g') <- ns, g==g' ]

-- FIXME: 全ての組み合わせを網羅している?
p_IV :: VerbForm -> Parser (P IV)
p_IV vf = mayF10s q -- S16
    where p = do x <- s1_IV vf
                      <|> s5 vf
                      <|> s7 vf 
                      <|> s8 vf
                      <|> s18 vf
                      <|> s19 vf
                      <|> s23 vf
                 liftM (foldl (flip F7) x) (many p_IAV) -- S10
          q = chainr1 p (f8 <|> f9) -- S12a, S12b

p_TV :: VerbForm -> Parser (P TV)
p_TV vf = s1_TV vf <|> s20 vf <|> s21 vf

p_IAV_T :: Parser (P (IAV :/ T))
p_IAV_T = s1_IAV_T

p_IV__IV :: VerbForm -> Parser (P (IV :// IV))
p_IV__IV = s1_IV__IV

p_t_t :: Parser (P (Sen :/ Sen))
p_t_t = s1_t_t

-- FIXME: 全ての組み合わせを網羅している?
p_t :: Parser (P Sen)
p_t = mayF10s $ -- S14
      chainr1 (s9 <|> s4_or_s17) (f8 <|> f9) -- S11a, S11b

p_IV_t :: VerbForm -> Parser (P (IV :/ Sen))
p_IV_t = s1_IV_t

p_IV_Adj :: VerbForm -> Parser (P (IV :/ Adj))
p_IV_Adj vf = s1_IV_Adj vf

p_Adj :: Parser (P Adj)
p_Adj = s1_Adj <|> s25

-- FIXME
p_DTV :: VerbForm -> Parser (P DTV)
p_DTV vf = mzero -- ???

p_PP :: Parser (P PP)
p_PP = s24

p_IAV :: Parser (P IAV)
p_IAV = s1_IAV <|> s6

p_PP_T :: Parser (P (PP :/ T))
p_PP_T = s1_PP_T

-----------------------------------------------------------------------------
-- 動詞の辞書

-- (原形, 三人称単数現在形, 過去分詞)
type VerbEntry = (String, String, String)

verb_be :: VerbEntry
verb_be = ("be", "is", "been")

{-# INLINE regularVerb #-}
regularVerb :: String -> VerbEntry
regularVerb s = (s, present, past_participle)
    where
      rs = reverse s
      present =
          case rs of
          's':'s':_ -> s ++ "es"
          'h':'c':_ -> s ++ "es"
          'h':'s':_ -> s ++ "es"
          'x':_ -> s ++ "es"
          'y':s' -> reverse s' ++ "ies"
          _      -> s ++ "s"
      past_participle =
          case rs of
          'e':_  -> s ++ "d"
          'y':s' -> reverse s' ++ "ied"
          _      -> s ++ "ed"

dict_IV :: [VerbEntry]
dict_IV =
    [ regularVerb "walk"
    , regularVerb "talk"
    , regularVerb "change"
    , ("run",  "runs",  "ran")
    , ("rise", "rises", "rosen")
    ]

dict_TV :: [VerbEntry]
dict_TV =
    [ ("find", "finds", "found")
    , ("lose", "loses", "lost")
    , ("eat",  "eats",  "eaten")
    , ("seek", "seeks", "sought")
    , regularVerb "love"
    , regularVerb "date"
    --, "coneive" -- FIXME: conceiveの間違い?
    , verb_be
    ]

dict_IV_t :: [VerbEntry]
dict_IV_t =
    [ regularVerb "believe"
    , regularVerb "assert"
    ]

dict_IV__IV :: [VerbEntry]
dict_IV__IV =
    [ regularVerb "try"
    , regularVerb "wish"
    ]

dict_IV_Adj :: [VerbEntry]
dict_IV_Adj = [verb_be]

-- DTVをTTVに変換することは出来るから、giveの範疇はDTVだろうな。多分。
dict_DTV :: [VerbEntry]
dict_DTV =
    [ ("give", "gives", "given")
    ]

-----------------------------------------------------------------------------
-- 動詞のパーサ

data VerbForm
    = VFOrig           -- 原形
    | VFPastParticiple -- 過去分詞
    | VFPresent !Bool  -- 三人称単数現在形とその否定形
    | VFFuture  !Bool  -- 三人称単数未来系とその否定形
    | VFPerfect !Bool  -- 三人称単数現在完了系とその否定形

{-# INLINE verbParser #-}
verbParser :: CatType c => [VerbEntry] -> VerbForm -> Parser (P c)
verbParser dict vf =
    do s <- verbParser' dict vf
       return (B cat s)

-- FIXME: 後で整理する
{-# INLINE verbParser' #-}
verbParser' :: [VerbEntry] -> VerbForm -> Parser String
verbParser' dict (VFPresent False) =
    mplus (do token "does"
              token "not"
              x <- verbParser' dict VFOrig
              guard (x/="be")
              return x)
          (do x <- verbParser' dict (VFPresent True)
              guard (x=="be")
              token "not"
              return x)
verbParser' dict (VFFuture b) =
    do token "will"
       unless b $ token "not" >> return ()
       verbParser' dict VFOrig
verbParser' dict (VFPerfect b) =
    do token "has"
       unless b $ token "not" >> return ()
       verbParser' dict VFPastParticiple
verbParser' dict vf =
    do x <- anyToken
       msum [ return o
            | (o, present, pastparticiple) <- dict
            , case vf of
              VFOrig           -> o==x
              VFPastParticiple -> pastparticiple==x
              VFPresent True   -> present==x
              _ -> False -- shouldn't happen
            ]

-----------------------------------------------------------------------------
-- 名詞の辞書とパーサ

data Case = Subject | Object deriving (Show,Eq,Ord)

data Gender = Masculine | Feminine | Neuter deriving (Show,Eq,Ord)
type NounEntry = (String, Gender)

dict_T :: [NounEntry]
dict_T =
    [ ("john"   , Masculine)
    , ("mary"   , Feminine)
    , ("bill"   , Masculine)
    , ("ninety" , Neuter)
    ]

dict_CN :: [NounEntry]
dict_CN =
    [ ("man"         , Masculine)
    , ("woman"       , Feminine)
    , ("park"        , Neuter)
    , ("fish"        , Neuter)
    , ("pen"         , Neuter)
    , ("unicorn"     , Neuter)
    , ("price"       , Neuter)
    , ("temperature" , Neuter)
    ]

{-# INLINE nounParser #-}
nounParser :: CatType c => [NounEntry] -> Parser (P c, Gender)
nounParser dict =
    do x <- anyToken
       case lookup x dict of
         Nothing -> mzero
         Just g  -> return (B cat x, g)

-----------------------------------------------------------------------------
-- それ以外の基本表現の辞書とパーサ

dict_IAV :: [String]
dict_IAV = 
    [ "rapidly"
    , "slowly"
    , "voluntarily"
    , "allegedly"
    ]

dict_t_t :: [String]
dict_t_t = ["necessarily"]

dict_IAV_T :: [String]
dict_IAV_T = ["in", "about"]

dict_Adj :: [String]
dict_Adj = ["asleep"]

dict_Det :: [String]
dict_Det = ["a", "an", "the", "every", "no"]

dict_PP_T :: [String]
dict_PP_T = ["by"]

{-# INLINE dictParser #-}
dictParser :: CatType c => [String] -> Parser (P c)
dictParser dict =
    do x <- anyToken
       guard $ x `elem` dict
       return $ B cat x

-----------------------------------------------------------------------------
-- 各統語規則のパーサ

s1_IV :: VerbForm -> Parser (P IV)
s1_IV = verbParser dict_IV

s1_TV :: VerbForm -> Parser (P TV)
s1_TV = verbParser dict_TV

s1_IV_t :: VerbForm -> Parser (P (IV :/ Sen))
s1_IV_t = verbParser dict_IV_t

s1_IV__IV :: VerbForm -> Parser (P (IV :// IV))
s1_IV__IV = verbParser dict_IV__IV

s1_IV_Adj :: VerbForm -> Parser (P (IV :/ Adj))
s1_IV_Adj = verbParser dict_IV_Adj

s1_T :: Parser (P T, Gender)
s1_T = nounParser dict_T

s1_CN :: Parser (P CN, Gender)
s1_CN = nounParser dict_CN

s1_IAV :: Parser (P IAV)
s1_IAV = dictParser dict_IAV

s1_t_t :: Parser (P (Sen :/ Sen)) 
s1_t_t = dictParser dict_t_t

s1_IAV_T :: Parser (P (IAV :/ T))
s1_IAV_T = dictParser dict_IAV_T

s1_Adj :: Parser (P Adj)
s1_Adj = dictParser dict_Adj

s1_Det :: Parser (P Det)
s1_Det = dictParser dict_Det

s1_PP_T :: Parser (P (PP :/ T))
s1_PP_T = dictParser dict_PP_T

s2 :: Parser (P T, Gender)
s2 =
    do delta <- p_Det
       -- w <- lookAhead
       (zeta,g) <- p_CN
       return (F2 delta zeta, g)

s3s :: Gender -> P CN -> Parser (P CN)
s3s g zeta = (s3_postfix g zeta >>= s3s g) <|> return zeta

s3_postfix :: Gender -> P CN -> Parser (P CN)
s3_postfix g zeta =
    do token "such"
       token "that"
       n <- gensym
       phi <- localS3 ((n,g):) p_t
       guard $ IS.member n (fvs phi) -- XXX: He n が現れていることを検査。
       noDanglingRef n -- nを参照している参照が残っていないことを保障
       return (F3 n zeta phi)

s4_or_s17 :: Parser (P Sen)
s4_or_s17 =
    do alpha <- p_T [Subject]
       msum [ do delta <- p_IV (VFPresent True)
                 return (F4 alpha delta) -- 三人称単数現在形 (S4)
            , do delta <- p_IV (VFPresent False) 
                 return (F11 alpha delta) -- 三人称単数現在の否定形 (S17)
            , do delta <- p_IV (VFFuture True)
                 return (F12 alpha delta) -- 三人称単数未来形 (S17)
            , do delta <- p_IV (VFFuture False)
                 return (F13 alpha delta) -- 三人称単数未来の否定形 (S17)
            , do delta <- p_IV (VFPerfect True)
                 return (F14 alpha delta) -- 三人称単数現在完了形 (S17)
            , do delta <- p_IV (VFPerfect False) 
                 return (F15 alpha delta) -- 三人称単数現在完了の否定形 (S17)
            ]

s5 :: VerbForm -> Parser (P IV)
s5 vf =
    do delta <- p_TV vf
       beta  <- p_T [Object]
       return (F5 delta beta)

s6 :: Parser (P IAV)
s6 =
    do delta <- p_IAV_T
       beta <- p_T [Object]
       return (F5 delta beta)

s7 :: VerbForm -> Parser (P IV)
s7 vf =
    do delta <- p_IV_t vf
       token "that"
       phi <- p_t
       return (F16 delta phi)

s8 :: VerbForm -> Parser (P IV)
s8 vf =
    do delta <- p_IV__IV vf
       token "to"
       beta <- p_IV VFOrig
       return (F17 delta beta)

s9 :: Parser (P Sen)
s9 =
    do delta <- p_t_t
       beta <- p_t
       return (F6 delta beta)

-- S10 は他のパーサの中に組み込んでしまっている
-- S11a, S11b, S12a, S12b, S13 は他のパーサの中に組み込んでしまっている
-- S14, S15, S16 は他のパーサの中に組み込んでしまっている

f8 :: F8 c => Parser (P c -> P c -> P c)
f8 = token "and" >> return F8

f9 :: F9 c => Parser (P c -> P c -> P c)
f9 = token "or" >> return F9

-- S17はS4のパーサの中に組み込んでしまっている

-- 講義資料でF9と書いてある???
s18 :: VerbForm -> Parser (P IV)
s18 vf =
    do alpha <- p_IV_Adj vf
       beta <- p_Adj
       return (F6 alpha beta)

s19 :: VerbForm -> Parser (P IV)
s19 vf = liftM F19 (p_TV vf)

-- FIXME: S20とS21のどちらかはTTVでは?
-- S20の方か?

-- ???: x が DTV ならば、x to him は TV
s20 :: VerbForm -> Parser (P TV)
s20 vf =
    do delta <- p_DTV vf
       token "to"
       beta <- p_T [Object]
       return (F20 delta beta)

s21 :: VerbForm -> Parser (P TV)
s21 vf =
    do delta <- p_DTV vf
       beta <- p_T [Object]
       return (F21 delta beta)

-- FIXME: この規則はどこからも使われていないけど良いのだろうか?
s22 :: VerbForm -> Parser (P TTV)
s22 vf = liftM F22 (p_DTV vf)

-- FIXME: αが-enならばというのは何を指している?
-- δ が been, rosen, eaten であること?
s23 :: VerbForm -> Parser (P IV)
s23 vf =
    do verbParser' [verb_be] vf
       delta <- p_TV VFPastParticiple
       alpha <- p_PP
       return (F23 alpha delta)

-- FIXME: αが-enならばというのは何を指している?
-- β≠he_n の間違いか?
s24 :: Parser (P PP)
s24 =
    do alpha <- p_PP_T
       beta <- p_T [Object]
       return (F24 alpha beta)

s25 :: Parser (P Adj)
s25 =
    do delta <- p_TV VFPastParticiple
       return (F25 delta)

-----------------------------------------------------------------------------

-- He n で表される「自由変数」の集合
fvs :: P c -> IS.IntSet
fvs (B _ _)     = IS.empty
fvs (He n)      = IS.singleton n
fvs (F2 x y)    = IS.union (fvs x) (fvs y)
fvs (F3 n x y)  = IS.delete n $ IS.union (fvs x) (fvs y)
fvs (F4 x y)    = IS.union (fvs x) (fvs y)
fvs (F5 x y)    = IS.union (fvs x) (fvs y)
fvs (F6 x y)    = IS.union (fvs x) (fvs y)
fvs (F7 x y)    = IS.union (fvs x) (fvs y)
fvs (F8 x y)    = IS.union (fvs x) (fvs y)
fvs (F9 x y)    = IS.union (fvs x) (fvs y)
fvs (F10 n x y) = IS.delete n $ IS.union (fvs x) (fvs y)
fvs (F11 x y)   = IS.union (fvs x) (fvs y)
fvs (F12 x y)   = IS.union (fvs x) (fvs y)
fvs (F13 x y)   = IS.union (fvs x) (fvs y)
fvs (F14 x y)   = IS.union (fvs x) (fvs y)
fvs (F15 x y)   = IS.union (fvs x) (fvs y)
fvs (F16 x y)   = IS.union (fvs x) (fvs y)
fvs (F17 x y)   = IS.union (fvs x) (fvs y)
fvs (F19 x)     = fvs x
fvs (F20 x y)   = IS.union (fvs x) (fvs y)
fvs (F21 x y)   = IS.union (fvs x) (fvs y)
fvs (F22 x)     = fvs x
fvs (F23 x y)   = IS.union (fvs x) (fvs y)
fvs (F24 x y)   = IS.union (fvs x) (fvs y)
fvs (F25 x)     = fvs x
