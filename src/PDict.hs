-----------------------------------------------------------------------------
-- |
-- Module      :  PDict
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  portable

module PDict where

import P

-----------------------------------------------------------------------------

john, mary, bill, ninety :: P T
john   = B cat_T "john"
mary   = B cat_T "mary"
bill   = B cat_T "bill"
ninety = B cat_T "ninety"

run, walk, talk, rise, change :: P IV
run    = B cat_IV "run"
walk   = B cat_IV "walk"
talk   = B cat_IV "talk"
rise   = B cat_IV "rise"
change = B cat_IV "change"

find, lose, eat, love, date, seek, coneiveg :: P TV
find     = B cat_TV "find"
lose     = B cat_TV "lose"
eat      = B cat_TV "eat"    
love     = B cat_TV "love"
date     = B cat_TV "date"
seek     = B cat_TV "seek"
coneiveg = B cat_TV "coneiveg"

man, woman, park, fish, pen, unicorn, price, temperature :: P CN
man         = B cat_CN "man"
woman       = B cat_CN "woman"
park        = B cat_CN "park"
fish        = B cat_CN "fish"
pen         = B cat_CN "pen"
unicorn     = B cat_CN "unicorn"
price       = B cat_CN "price"
temperature = B cat_CN "temperature"

slowly :: P IAV
slowly = B cat_IAV "slowly"

believe, assert :: P (IV :/ Sen)
believe = B (cat_IV :/ cat_Sen) "beleave"
assert  = B (cat_IV :/ cat_Sen) "assert"

asleep :: P Adj
asleep = B cat_Adj "asleep"

try', wish :: P (IV :// IV)
try' = B (cat_IV :// cat_IV) "try"
wish = B (cat_IV :// cat_IV) "wish"

in', about :: P (IAV :/ T)
in'   = B (cat_IAV :/ cat_T) "in"
about = B (cat_IAV :/ cat_T) "about"

a, every :: P Det
a     = B cat_Det "a"
every = B cat_Det "every"

necessarily :: P (Sen :/ Sen)
necessarily = B (cat_Sen :/ cat_Sen) "necessarily"

class Be c where
    be :: P c
instance Be TV where
    be = B cat_TV "be"
instance Be (IV :/ Adj) where
    be = B (cat_IV :/ cat_Adj) "be"
