{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FingerTree.Measure where

import Control.DeepSeq (NFData)
import Data.FingerTree (Measured (..))
import GHC.Generics (Generic)

newtype Value a = Value a deriving (Num, Eq, Ord, Real, Integral, Enum, Generic)

newtype Size = Size Int deriving (Num, Enum, Real, Integral, Eq, Ord, Show, Generic)

instance Semigroup Size where
  (<>) = (+)

instance Monoid Size where
  mempty = 0

instance Measured Size (Value a) where
  measure _ = 1

instance (NFData a) => NFData (Value a)

instance NFData Size
