{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy
import           Data.Text
import           Dhall
import           GHC.TypeLits


instance Forall (KeyValue KnownSymbol (Instance1 Interpret h)) xs => Interpret (Field h :* xs) where
  autoWith _ = Dhall.record (hgenerateFor
              (Proxy :: Proxy (KeyValue KnownSymbol (Instance1 Interpret h)))
              (\m -> let k = (pack . symbolVal . proxyAssocKey) m
                     in field k (fmap Field auto)))

deriving instance Interpret (h (AssocValue kv)) => Interpret (Field h kv)
deriving instance Interpret a => Interpret (Identity a)

type Example1 = Record '[ "foo" >: Natural ]
example1 :: IO Example1
example1 = input auto "{ foo= 1 }"

type Example2 = Record '[ "foo" >: Text
                        , "bar" >: Record
                          '[ "baz" >: Text
                           , "qux" >: Natural]
                        ]
example2 :: IO Example2
example2 = input auto "{foo=\"nyaan\",bar={baz=\"hoge\",qux=1234}}"

type Example3 = Example2
example3 :: IO Example3
example3 = input auto "{foo=\"nyaan\", bar=./config.dhall}"
{- config.dhall
{	baz = "nyaan"
,	qux = 12
}
-}

main :: IO ()
main = do
  print =<< example1
  print =<< example2
  print =<< example3
