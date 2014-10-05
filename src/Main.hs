{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import System.IO
import System.Process
import System.Environment
import System.Directory

main :: IO ()
main = scotty 61000 $ do
  post "/" $ do
    bs <- body
    response <- liftIO $ do
      name <- getProgName
      tempDir <- getTemporaryDirectory
      (path, h) <- openBinaryTempFile tempDir name
      BSL.hPut h bs
      hClose h
      callProcess "gvim" [path]
      BSL.readFile path
    raw response
