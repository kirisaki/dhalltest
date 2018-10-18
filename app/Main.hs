{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Dhall
import GHC.TypeLits
import Data.Extensible
import Data.Functor.Identity
import Data.Proxy
import Data.Text



instance Forall (KeyValue KnownSymbol (Instance1 Interpret h)) xs => Interpret (Field h :* xs) where
  autoWith _ = Dhall.record (hgenerateFor
              (Proxy :: Proxy (KeyValue KnownSymbol (Instance1 Interpret h)))
              (\m -> let k = (pack . symbolVal . proxyAssocKey) m
                     in field k (fmap Field auto)))

deriving instance Interpret (h (AssocValue kv)) => Interpret (Field h kv)
deriving instance Interpret a => Interpret (Identity a)

type Example = Record '["foo" >: Natural]

main :: IO ()
main = do
    x <- input auto "{ foo= 1 }"
    print (x :: Example)
