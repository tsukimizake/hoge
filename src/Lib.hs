{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
module Lib
    ( someFunc
    ) where
import Data.List
import GHC.Generics
import Data.Proxy
import Data.Data
import Data.Typeable
import Data.Extensible
import Control.Lens

type Hoge = Record '["id" >: Int, "status" >: Maybe String ]
type Huga = Record '["id" >: Int, "status" >: String ]

type family ToMaybeRec a

type instance ToMaybeRec Huga = Hoge

f :: forall a. a -> ToMaybeRec a
f x = let stat = Just $ x ^. #status
       in shrink $ #status @= stat <: x


r1 :: Hoge
r1 = #id @= 0 <: #status @= Nothing <: emptyRecord 


someFunc :: IO ()
someFunc = do 
  print r1
  putStrLn "someFunc"
