-----------------------------------------------------------------------------
-- |
-- Module      :  PTest
-- Copyright   :  (c) Masahiro Sakai 2007-2009
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

import Report

test :: String -> IO ()
test = putStr . report

-----------------------------------------------------------------------------

ex4_1 = "John seeks a unicorn."
-- ex4_2
ex4_3 = "John finds a unicorn."
ex4_4 = "Necessarily John walks."
ex4_5 = "John is Bill."
ex4_6 = "John is a unicorn."

ex_1 = "John believes that a unicorn talks."
ex_2 = "Every woman loves a unicorn."
ex_3 = "Every woman loves John."
ex_4 = "A unicorn walks and it talks."
ex_5 = "Every unicorn such that it walks talks."
ex_6 = "John walks slowly."
ex_7 = "John walks in a park."
ex_8 = "John wishes to find a unicorn and eat it."
ex_9  = "A man or a woman is asleep."
ex_10 = "A man is asleep or a woman is asleep."
ex_11 = "A man or a woman loves every unicorn."
ex_12 = "A man loves every unicorn or a woman loves every unicorn."
ex_13 = "Every unicorn walks or talks."
