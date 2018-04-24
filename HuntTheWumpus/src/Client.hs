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
huntClient args = clientStart serverURI
  where
    Just serverURI = case intercalate ":" (take 2 args) of
                       ""  -> parseURI "http://127.0.0.1:2018"  
                       uri -> parseURI ("http://" ++ uri)

clientStart :: URI -> IO ()
clientStart uri = do
    startGame <- promptStart
    if (startGame == "y")
      then putStr "Welcome to the game"
      else (if (startGame == "i")
              then (putStr "Instructions: \n \
                             \ You have 1 arrow that can shoot down a path of 5 rooms. \n \
                             \ You will be prompted with the rooms that it will travel. \n \
                             \ Enter \"y\" to start. \n " )
              else putStr "Invalid command")

--clientLoop :: URI -> IO ()
--clientLoop uri = do
    --g   <- promptStart
    --putStr "game start"
    --msg <- submitGuess g uri
    --putStrLn msg
    --clientLoop uri

promptStart :: IO String
promptStart = 
  putStr "Welcome to Hunt the Wumpus. \n \
          \ You have been brought to Room 4 in a cave with 20 rooms. \n \
          \ Some have bats and some have a deep pit. \n \
          \ The Wumpus is also in one of them. Find where he is and shoot \n \
          \ him with your arrow before he finds and eats you. \n \
          \ Are you ready to hunt? \n \
          \ Enter \"y\" to start. \n \
          \ Enter \"i\" for more instructions." >> hFlush stdout >> getLine

--showInstructions :: I
--showInstructions = putStrLn "Instructions: \n" ++
--                           "You have 1 arrow that can shoot down a path of 5 rooms. \n" ++
--                           "You will be prompted with the rooms that it will travel. \n" ++
--                           "Enter \"y\" to start. \n " 
                           -- >> hFlush stdout >> getLine

submitGuess :: Guess -> URI -> IO String
submitGuess g uri = do
    let rq = setRequestBody (mkRequest POST uri)
                            ("text/json",encodeJSON g)
    rsp <- Network.HTTP.simpleHTTP rq
    getResponseBody rsp

