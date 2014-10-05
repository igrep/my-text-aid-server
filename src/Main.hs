{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy.IO as TLI
import System.IO
import System.Process
import System.Environment
import System.Directory
import Network.HTTP.Types.Status

main :: IO ()
main = scotty 61000 $ do
  defaultHandler $ \t -> do
    liftIO $ do
      logPath <- (++ "MyTextAidServer-Error.log") <$> getHomeDirectory
      TLI.appendFile logPath t
    status $ internalServerError500
    text t

  post "/" $ do
    bs <- body
    (path, h) <- liftIO createTemporaryFileForEditor
    raw =<< (liftIO $ editBinaryOnFile bs path h)

removeFileIfExist :: FilePath -> IO ()
removeFileIfExist path = do
  b <- doesFileExist path
  when b $ removeFile path

createTemporaryFileForEditor :: IO (FilePath, Handle)
createTemporaryFileForEditor = do
  name <- getProgName
  tempDir <- getTemporaryDirectory
  openBinaryTempFile tempDir name

editBinaryOnFile :: BSL.ByteString -> FilePath -> Handle -> IO BSL.ByteString
editBinaryOnFile bs path h = do
  BSL.hPut h bs
  hClose h
  callProcess "gvim" [path]
  r <- BS.readFile path
  removeFileIfExist path
  return $ BSL.fromStrict r
