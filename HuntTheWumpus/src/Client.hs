{-# LANGUAGE DeriveDataTypeable #-}
module Client
    ( huntClient
    ) where

import Data.List (intercalate)
import Network.BufferType
import Network.HTTP
import Network.URI
import System.IO
import Text.JSON.Generic

data Guess = Guess { guess :: Int } deriving (Eq,Data,Typeable,Show)

huntClient :: [String] -> IO ()
huntClient args = clientLoop serverURI
  where
    Just serverURI = case intercalate ":" (take 2 args) of
                       ""  -> parseURI "http://localhost:2018" 	
                       uri -> parseURI ("http://" ++ uri)

clientLoop :: URI -> IO ()
clientLoop uri = do
    g   <- promptStart
    --msg <- submitGuess g uri
    --putStrLn msg
    --clientLoop uri

promptStart :: IO Guess
promptStart = putStr "Welcome to Hunt the Wumpus. \n Press [Enter] to start: " >> hFlush stdout >> Guess <$> readLn

submitGuess :: Guess -> URI -> IO String
submitGuess g uri = do
    let rq = setRequestBody (mkRequest POST uri)
                            ("text/json",encodeJSON g)
    rsp <- Network.HTTP.simpleHTTP rq
    getResponseBody rsp

