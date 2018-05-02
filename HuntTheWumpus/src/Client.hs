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

-- data Guess = Guess { guess :: Int } deriving (Eq,Data,Typeable,Show)
data UserInput = UserInput {currRoom::Int, stage::String, command::String, value::String} deriving (Eq,Data,Typeable,Show)
data ServerMsg = ServerMsg {newRoom::Int, msg::String} deriving (Eq,Data,Typeable,Show)

welcome :: String
welcome = "Welcome to Hunt the Wumpus. \n \
            \-------------------------\n\
            \ You have been brought to Room 4 in a cave with 20 rooms. \n\
            \ Some have bats and some have a deep pit. \n\
            \ The Wumpus is also in one of them. Find where he is and shoot \n\
            \ him with your arrow before he finds and eats you. \n\
            \ Enter [y] to begin. \n\
            \ Enter [i] to view instructions. \n"

huntClient :: [String] -> IO ()
huntClient args = clientStart serverURI
  where
    Just serverURI = case intercalate ":" (take 2 args) of
                       ""  -> parseURI "http://127.0.0.1:2018"  
                       uri -> parseURI ("http://" ++ uri)

clientStart :: URI -> IO ()
clientStart uri = do
    usrCmd <- putStr (welcome) >> hFlush stdout >> getLine
    let input = UserInput 4 "welcome" "starts" usrCmd
    rsp <- submitGuess input uri
    if (usrCmd == "i")
        then clientInstruction uri rsp
        else clientLoop uri rsp

clientInstruction :: URI -> String -> IO ()
clientInstruction uri rsp = do
    usrCmd <- putStr (msg response) >> hFlush stdout >> getLine
    let input = UserInput 4 "welcome" "starts" usrCmd
    newRsp <- submitGuess input uri
    clientLoop uri newRsp
  where 
    response = decodeJSON rsp

clientLoop :: URI -> String -> IO ()
clientLoop uri rsp = do
    command <- putStr (msg response) >> hFlush stdout >> getLine
    let usrCmd = words command
    let input = UserInput (newRoom response) "game" (head usrCmd) (last usrCmd)
    newRsp <- submitGuess input uri
    clientLoop uri newRsp
  where 
    response = decodeJSON rsp
    -- promptGame :: IO String


--showInstructions :: I
--showInstructions = putStrLn "Instructions: \n" ++
--                           "You have 1 arrow that can shoot down a path of 5 rooms. \n" ++
--                           "You will be prompted with the rooms that it will travel. \n" ++
--                           "Enter \"y\" to start. \n " 
                           -- >> hFlush stdout >> getLine

submitGuess :: UserInput -> URI -> IO String
submitGuess g uri = do
    let rq = setRequestBody (mkRequest POST uri)
                            ("text/json",encodeJSON g)
    rsp <- Network.HTTP.simpleHTTP rq
    getResponseBody rsp

