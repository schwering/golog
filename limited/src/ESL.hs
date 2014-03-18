{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module ESL
  () where

type StdName = Int
type Var = Int
data Term = StdName StdName | Var Var

class (Eq a, Enum a) => ProperBAT a where
   data StdName a :: *


jo


