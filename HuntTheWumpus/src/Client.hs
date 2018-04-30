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
data UserInput = UserInput {stage::String, command::String, value::String} deriving (Eq,Data,Typeable,Show)
welcome :: String
welcome = "Welcome to Hunt the Wumpus. \n \
            \-------------------------\n\
            \ You have been brought to Room 4 in a cave with 20 rooms. \n \
            \ Some have bats and some have a deep pit. \n \
            \ The Wumpus is also in one of them. Find where he is and shoot \n \
            \ him with your arrow before he finds and eats you. \n\n"
instructions :: String;
instructions = "Instructions: \n\
                \-------------------------\n\
                \  This is a 1-player game. You are the player, who has \n\
                \  been placed in a random room in a cave containing 20 rooms.\n\
                \  \nYou see three tunnels to other rooms that are either \n\ 
                \  a) just like this one with nothing \n\
                \  b) contains bats that will transport you to a random room \n\
                \  c) contains a bottomless pit, in which you will fall into and die \n\
                \  d) contains a wumpus that will eat you alive \n\
                \  \nTo win the game, you have to find and kill the wumpus before \n\
                \  it finds you. You have three special abilities: \n\
                \  1) Smell - you can smell the smelly wumpus that is in one of the \n\
                \             adjacent rooms \n\
                \  2) Feel  - you can feel the cool breeze from the bottomless pit \n\
                \             that is in one of the adjcaent rooms \n\
                \  3) Hear  - you can hear the bats that are in one of the adjacent \n\
                \             rooms \n\
                \  \nYou also have a special crooked arrow that you will use to kill the \n\
                \  wumpus. This special arrow can travel down a max number of 5 rooms \n\
                \  from your current room. \n\
                \  \nEvery turn you can enter the following commands to: \n\
                \  a) move # - where # is the one of the three adjacent rooms \n\
                \  b) shoot # - where # is the first room that the crooked arrow will go \n\
                \  \nAre you ready to hunt the wumpus? Enter [y] to begin  \n"

huntClient :: [String] -> IO ()
huntClient args = clientStart serverURI
  where
    Just serverURI = case intercalate ":" (take 2 args) of
                       ""  -> parseURI "http://127.0.0.1:2018"  
                       uri -> parseURI ("http://" ++ uri)

clientStart :: URI -> IO ()
clientStart uri = do
    g <- promptStart
    let input = UserInput "welcome" "start" g
    msg <- submitGuess input uri
    clientLoop uri msg

clientLoop :: URI -> String -> IO ()
clientLoop uri msg= do
    command <- putStr msg >> hFlush stdout >> getLine
    let usrCmd = words command
    let input2 = UserInput "game" (head usrCmd) (last usrCmd)
    newMsg <- submitGuess input2 uri
    clientLoop uri msg
    -- promptGame :: IO String

promptStart :: IO String
promptStart = 
  putStr (welcome ++ instructions) >> hFlush stdout >> getLine
--Asks user for input

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

