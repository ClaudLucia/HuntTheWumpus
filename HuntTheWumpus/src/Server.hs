{-# LANGUAGE DeriveDataTypeable #-}
module Server
    ( huntServer
    ) where

import Codec.Binary.UTF8.String
import Network.BufferType
import Network.HTTP.Server (defaultConfig,
                            insertHeader,
                            respond,
                            rqBody,
                            rspBody,
                            serverWith,
                            srvLog,
                            srvPort,
                            Handler,
                            HeaderName(HdrContentLength,HdrContentEncoding),
                            Response,
                            StatusCode(OK)
                           )
import Network.HTTP.Server.Logger (stdLogger)
import System.Random (getStdGen,randomR)
import System.Environment (getArgs)
import Text.JSON.Generic
import Data.List

type CaveMap = [Room]
data Room = Empty | Bat | Pit | Wumpus
  deriving (Eq,Data,Typeable)

data UserInput = UserInput {currRoom::Int, stage::String, command::String, value::String} deriving (Eq,Data,Typeable,Show)
data ServerMsg = ServerMsg {newRoom::Int, msg::String} deriving (Eq,Data,Typeable,Show)

-- create the map
caveMap :: CaveMap
caveMap = [Bat,   Pit,   Wumpus, Empty, 
           Empty, Empty, Empty,  Bat,
           Pit,   Empty, Bat,    Empty, 
           Pit,   Empty, Empty,  Empty, 
           Empty, Empty, Pit,    Empty]

paths :: [[Int]]
paths = [[2,8,5],    [1,3,10],   [2,4,12],  [3,5,14],
         [4,6,1],    [5,7,15],   [6,8,17],  [1,7,11],
         [10,12,19], [2,9,11],   [8,10,20], [3,9,13],
         [12,14,18], [4,13,15],  [6,14,16], [15,17,18],
         [7,16,20],  [13,16,19], [9,18,20], [11,17,19]]
--data RoomType = BatRm | PitRm | WumpusRm | EmptyRm
--data Room = Room Int RoomType

-- instance Show Room where
--   show BatRm    = "BatRm"
--   show PitRm    = "PitRm"
--   show WumpusRm = "WumpusRm"
--   show EmptyRm  = "EmptyRm"

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

huntServer :: IO ()
huntServer = do
    args <- map read <$> getArgs
    let port = if null args then 2018 else head args
    serverWith defaultConfig { srvLog = stdLogger, srvPort = port } $ handleCmd


handleCmd :: Handler String
handleCmd addr url req | ((stage input) == "welcome") = if ((value input) == "y")
                                                          then return $ sendText OK (ServerMsg 4 ((handleMove 4 5)))
                                                          else (if ((value input) == "i")
                                                                  then return $ sendText OK (ServerMsg 4 instructions)
                                                                  else return $ sendText OK (ServerMsg 4 "Invalid command"))
                        | ((stage input) == "game"  ) = if ((command input) == "move")
                                                          then return $ sendText OK (ServerMsg (read (value input)) 
                                                                                                (handleMove (read (value input))
                                                                                                            (currRoom input)))
                                                          else (if ((command input) == "shoot")
                                                                  then return $ sendText OK (ServerMsg (read (value input)) 
                                                                                                        (handleMove (read (value input))
                                                                                                        (currRoom input)))
                                                                  else return $ sendText OK (ServerMsg (read (value input)) "Invalid command"))
                        | otherwise                   = return $ sendText OK (ServerMsg (read (value input)) "huh")
  where input = decodeJSON $ rqBody req

handleMove :: Int -> Int -> String
handleMove roomNum currRoom | (roomNum `elem` (paths !! (currRoom-1))) = if (roomNum `elem` [2,8,13,19])
                                                                            then "You fell in the pit!"
                                                                            else (if (roomNum `elem` [1,8,11])
                                                                                    then "You encountered bats!" --handleBat
                                                                                    else "You are now in room "     
                                                                                    ++ show roomNum
                                                                                    ++ ". \nTunnel leads to ")
                                                                                    ++ (printRooms (paths !! (roomNum-1)))
                                                                                    ++ "\n"
                            | otherwise = "Invalid move"

handleShoot :: Int -> Int -> String
handleShoot roomNum currRoom | (roomNum `elem` (paths !! (currRoom-1))) = "You are now in room. " 
                                                                        ++ show roomNum
                                                                        ++ "Tunnel leads to "
                                                                        ++ (printRooms (paths !! (roomNum-1)))
                            | otherwise = "Invalid move"

printRooms :: [Int] -> String
printRooms [] = []
printRooms (x:xs) = (show x) ++ " " ++ printRooms xs

-- pitFall :: Int -> Int -> String
-- pitFall roomNum currRoom | roomNum `elem` [1,8,12,19] = "You fell in the pit!"

sendText :: StatusCode -> ServerMsg -> Response String
sendText s v = insertHeader HdrContentLength (show (length txt))
             $ insertHeader HdrContentEncoding "UTF-8"
             $ insertHeader HdrContentEncoding "application/json"
             $ (respond s :: Response String) { rspBody = txt }
  where
    -- txt = encodeString v
    txt = encodeJSON v

--functions needed:
--handle shoot
--room numbers
--handle commands(shoot and move, bats and pit)
--pitFall
  --if command = shoot 
      --then handle shoot rm
      --else (if command = move 
               --then user location = rm
                 --else sendText "invalid command")
--handle initial(y and i)
--move user to random room