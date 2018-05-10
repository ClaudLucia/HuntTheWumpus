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

data UserInput = UserInput {currRoom::Int, command::String, value::Int} deriving (Eq,Data,Typeable,Show)
data ServerMsg = ServerMsg {newRoom::Int, msg::String} deriving (Eq,Data,Typeable,Show)

--initial message
welcome :: String
welcome = "Welcome to Hunt the Wumpus. \n \
            \-------------------------\n\
            \ You have been brought to a room in a cave with 20 rooms. \n\
            \ Some have bats and some have a deep pit. \n\
            \ The Wumpus is also in one of them. Find where he is and shoot \n\
            \ him with your arrow before he finds and eats you. \n\
            \ Enter [y] to begin. \n\
            \ Enter [i] to view instructions. \n"
--instruction message
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
                \  wumpus. This special arrow can be shot down to an adjacent room. \n\
                \  \nEvery turn you can enter the following commands to: \n\
                \  a) move # - where # is the one of the three adjacent rooms \n\
                \  b) shoot # - where # is the room that the crooked arrow will go \n\
                \  \nAre you ready to hunt the wumpus? Enter [y] to begin  \n"
--initial room
gameStart :: String;
gameStart = "You are now in Room 16. \n\
             \Tunnels lead to 15 17 18. Move or Shoot? \n"

--handle user input
huntClient :: [String] -> IO ()
huntClient args = clientStart serverURI welcome
  where
    Just serverURI = case intercalate ":" (take 2 args) of
                       ""  -> parseURI "http://127.0.0.1:2018"  
                       uri -> parseURI ("http://" ++ uri)

--initial welcome screen
clientStart :: URI -> String -> IO ()
clientStart uri msg = do
    usrCmd <- putStr msg >> hFlush stdout >> getLine
    if (usrCmd == "i")
        then clientStart uri instructions
        else (if (usrCmd == "y")
                then clientLoop uri response
                else clientStart uri "Invalid command.\n\
                                     \Enter [y] to begin. \n\
                                     \Enter [i] to view instructions. \n")
  where
    response = ServerMsg 16 gameStart

--check commands entered in welcome screen
checkStartCmd :: URI -> String -> IO ()
checkStartCmd uri "i" = do
    clientStart uri instructions
checkStartCmd uri "y" = do
    clientLoop uri response
        where
            response = ServerMsg 4 gameStart
checkStartCmd uri _ = do
    clientStart uri "Invalid command.\n\
                    \Enter [y] to begin. \n\
                    \Enter [i] to view instructions. \n"

--game has started
--handle user input and server response
clientLoop :: URI -> ServerMsg -> IO ()
clientLoop uri response = do
    command <- putStr (msg response) >> hFlush stdout >> getLine
    let usrCmd = words command
    checkGameCmd uri (newRoom response) usrCmd

--check commands entered during game
checkGameCmd :: URI -> Int -> [String] -> IO ()
checkGameCmd uri currRoom ["move", r] = do
    let input = UserInput currRoom "move" (read r)
    newRsp <- submitCmd input uri
    checkResult uri newRsp
checkGameCmd uri currRoom ["shoot", r] = do
    let input = UserInput currRoom "shoot" (read r)
    newRsp <- submitCmd input uri
    checkResult uri newRsp
checkGameCmd uri currRoom usrCmd = do
    let response = ServerMsg currRoom "Invalid command. \n\
                                        \Enter [move #] or [shoot #] where #\
                                        \ is the target room.\n"
    clientLoop uri response

--check if user died or not depending on the room
checkResult :: URI -> String -> IO()
checkResult uri rsp = do
    if ((newRoom response) == -1)
        then putStr (msg response)
        else clientLoop uri response
    where 
        response = decodeJSON rsp

--post user input to server
submitCmd :: UserInput -> URI -> IO String
submitCmd cmd uri = do
    let rq = setRequestBody (mkRequest POST uri)
                            ("text/json",encodeJSON cmd)
    rsp <- Network.HTTP.simpleHTTP rq
    getResponseBody rsp
