{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

main :: IO ()
main = scotty 61000 $ get "/" $ html "<h1> hello, world!"
