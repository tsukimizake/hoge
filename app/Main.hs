{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Monad
import Lib
-- import qualified Data.Vector as V

data Hoge = Hoge Int Int Int Int Int Int Int Int Int Int Int Int deriving(Show)
data Huga = Huga !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int  deriving(Show)
#define Type Hoge
main :: IO ()
main = do
  someFunc
  --let !xs = replicate (1000000*1) (Type 100 200 300 100 200 300 100 200 300 100 200 300)
  --seq xs $ pure ()
  ---- forM_ xs $ \x -> do
  ----   seq x $ pure ()
  --print $ foldr  (\(Type a b c d e f g h i j k l) x -> x + a+b+c+d+e+f+g+h+i+j+k+l) (0 :: Int) xs
