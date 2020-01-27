{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad
import GHC.Generics
import Data.Proxy
import GHC.TypeLits

data Permission' a = Authorized a | NoPermission
    deriving ( Show )

data Report = Report { cprice :: Permission' (Maybe Double) }
    deriving ( Generic, Show )

class HasPermField (fname :: Symbol) a rt where
    getField :: Proxy fname -> a -> Permission' (Maybe rt)

instance HasPermField "cprice" Report Double where
    getField _ = cprice

data PermField (fname :: Symbol) a rt

data PermFields :: [ Symbol ] -> * -> * where
    End :: PermFields fields repo -> PermFields (y ': fields) repo
    Cont :: (HasPermField fname a rt)
        => PermField fname a rt -> PermFields fields repo



encode :: PermFields xs t -> Report -> String
encode _ r = undefined

main :: IO ()
main = do
  print $ encode undefined Report {cprice = Authorized (Just 1)}
