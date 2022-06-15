module Main where

import Web.Spock

import App (getConfig, app)


main :: IO ()
main = do 
    spockCfg <- getConfig
    runSpock 8080 (spock spockCfg app)
