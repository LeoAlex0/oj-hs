{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2 #-}

module Luogu.P3156 where

import           Data.Array.Unboxed (UArray, listArray, (!))
import           Data.Either        (fromRight)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import qualified Data.Text.Read     as T

readInt :: T.Text -> Int
readInt = fst. fromRight undefined . T.decimal

main :: IO ()
main = do
  n:_ <- fmap readInt . T.words <$> TIO.getLine
  list :: UArray Int Int <- listArray (1,n).fmap readInt. T.words <$> TIO.getLine
  mapM_ (print.(list !).readInt). T.words =<< TIO.getLine
