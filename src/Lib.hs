{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
module Lib
    ( 
    ) where
import Data.List
import GHC.Generics
import Data.Proxy
import Data.Data
import Data.Typeable
import Data.Extensible

--type HId = '["id" >: Int]
--type Hoge = Record  (HId >: '["status" >: Maybe String ] )
--type Huga = Record '["id" >: Int, "status" >: String ]
--
--type family ToMaybeRec a
--
--type instance ToMaybeRec Huga = Hoge
--
--collectiveOf :: (Associate "name" String s, Associate "collective" String s)
--  => Record s -> String
--collectiveOf x = concat $ [x ^. #name, x ^. #collective]
--
--f :: forall s t. (Lookup s "status" String , Lookup t "status'" (Maybe String)) => Record s -> Record t
--f x = let stat = Just $ x ^. #status
--       in #status' @= Nothing <: emptyRecord 
--
---- id, statusを持つ任意のものを返せないといけなくなるのでこういうことは書けない
---- mkHoge :: forall a. (Lookup a "id" Int, Lookup a "status" (Maybe String)) => Record a
---- mkHoge = #id @= 0 <: #status @= Nothing <: emptyRecord 
--
--r1 :: Hoge
--r1 = #id @= 0 <: #status @= Nothing <: emptyRecord 
--
--
--someFunc :: IO ()
--someFunc = do 
--  print r1
--  putStrLn "someFunc"
