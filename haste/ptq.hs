{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Masahiro Sakai 2007-2014
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

module Main where

import Control.Monad
import Haste
import Report

main :: IO ()
main = do
  withElems ["button-translate-input", "button-translate-sample", "input", "output", "sample"] $ \[button1, button2, input, output, sample] -> do
    let update s = do
          clearChildren output
          e <- newTextElem (report s)
          addChild e output

    _ <- onEvent input OnKeyPress $ \c -> do
      when (c == fromEnum '\r' || c == fromEnum '\n') $ do
        update =<< getProp input "value"
    _ <- button1 `onEvent` OnClick $ \_ _ -> do
      update =<< getProp input "value"
    _ <- button2 `onEvent` OnClick $ \_ _ -> do
      update =<< getProp sample "value"
    return ()
