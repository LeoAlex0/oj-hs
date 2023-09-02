{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FingerTree.Measure where

import Control.DeepSeq (NFData)
import Data.FingerTree (Measured (..))
import GHC.Generics (Generic)

newtype Value a
  = Value a
  deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

newtype Size
  = Size Int
  deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

instance Semigroup Size where
  (<>) = (+)

instance Monoid Size where
  mempty = 0

instance Measured Size (Value a) where
  measure _ = 1

instance (NFData a) => NFData (Value a)

instance NFData Size
