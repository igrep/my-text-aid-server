{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import System.IO
import System.Process
import System.IO.Temp
import System.Environment
import System.Directory

main :: IO ()
main = scotty 61000 $ do
  post "/" $ do
    bs <- body
    name <- liftIO getProgName
    response <- liftIO $ withSystemTempFile name $ \path h -> do
      BSL.putStrLn bs
      BSL.hPut h bs
      hClose h
      callProcess "gvim" [path]
      BSL.readFile path
    raw response
