module Main (main) where

import System.Environment
import Web.Fedora.Bodhi

import Data.Time.LocalTime

main :: IO ()
main = do
  args <- getArgs
  let pkg = head args
  moverride <- bodhiOverride pkg
  print $ (lookupKey' "expiration_date" <$> moverride :: Maybe LocalTime)
