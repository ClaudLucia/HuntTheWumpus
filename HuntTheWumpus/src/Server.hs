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

data UserInput = UserInput {currRoom::Int, command::String, value::Int} deriving (Eq,Data,Typeable,Show)
data ServerMsg = ServerMsg {newRoom::Int, msg::String} deriving (Eq,Data,Typeable,Show)

-- create the map
caveMap :: CaveMap
caveMap = [Bat,   Empty, Wumpus, Empty, 
           Pit,   Empty, Bat,    Empty,
           Empty, Empty, Bat,    Empty, 
           Pit,   Empty, Empty,  Empty, 
           Empty, Empty, Pit,    Empty]

paths :: [[Int]]
paths = [[2,8,5],    [1,3,10],   [2,4,12],  [3,5,14],
         [4,6,1],    [5,7,15],   [6,8,17],  [1,7,11],
         [10,12,19], [2,9,11],   [8,10,20], [3,9,13],
         [12,14,18], [4,13,15],  [6,14,16], [15,17,18],
         [7,16,20],  [13,16,19], [9,18,20], [11,17,19]]

huntServer :: IO ()
huntServer = do
    args <- map read <$> getArgs
    let port = if null args then 2018 else head args
    serverWith defaultConfig { srvLog = stdLogger, srvPort = port } $ handleCmd


handleCmd :: Handler String
handleCmd addr url req | (command input) == "move"  = return $ sendText OK (handleMove (value input) (currRoom input))
                       | (command input) == "shoot" = return $ sendText OK (handleShoot (value input) (currRoom input))
                       | otherwise                  = return $ sendText OK (ServerMsg (value input) "Invalid command. \n\
                                                                                                    \Enter [move #] or [shoot #] where #\
                                                                                                    \is the target room.\n")
  where input = decodeJSON $ rqBody req

handleMove :: Int -> Int -> ServerMsg
handleMove newRoom currRoom | (newRoom `elem` (paths !! (currRoom-1))) = handleRoom newRoom
                            | otherwise = ServerMsg currRoom "Invalid room. Move or shoot? \n"

handleShoot :: Int -> Int -> ServerMsg
handleShoot targetRoom currRoom | (targetRoom `elem` (paths !! (currRoom-1))) = if (targetRoom == 3)
                                                                                    then (ServerMsg (-1) ("You killed the Wumpus!"
                                                                                                          ++ "Congratulations"))
                                                                                    else (ServerMsg currRoom "No Wumpus was shot. Move or shoot? \n")
                                | otherwise                                = (ServerMsg currRoom "Invalid room. Move or shoot? \n")

handleBat :: ServerMsg
handleBat = ServerMsg randRoom ("Oh no! You were picked up by bats")
                          -- ++ "You are now in room " 
                          -- ++ show randRoom 
                          -- ++ "\nTunnel leads to " 
                          -- ++ (printRooms (paths !! (randRoom-1))) ++ "\n"
                          -- ++ handleSense randRoom)
                  where 
                       randomRIO (0,6)


-- getRandom :: Int -> ServerMsg
-- getRandom = do
--         gen <- getStdGen
--         index <- (fst $ randomR (0,6) gen :: Int)
--         let randRoom = (index !! [1,3,5,7,11,13,19])
--         handleBat randRoom

        --else return $ sendText Ok ("Tunnel leads to " ++ printRooms(paths !! (newRoom-1)) ) 

handleRoom :: Int -> ServerMsg
handleRoom newRoom | (newRoom == 3)                    = ServerMsg (-1) "You lose! The Wumpus ate you...\n"
                   | (newRoom `elem` [5,9,13,19])      = ServerMsg (-1) "You lose! You fell in the pit...\n"
                   | (newRoom `elem` [1,7,11])         = handleBat
                   | otherwise = ServerMsg (newRoom) ("You are now in room " 
                                                      ++ show newRoom 
                                                      ++ "\nTunnel leads to " 
                                                      ++ (printRooms (paths !! (newRoom-1))) ++ "\n"
                                                      ++ handleSense newRoom)

handleSense :: Int -> String
handleSense roomNum | (roomNum `elem` [2,4,12]) = "I smell the Wumpus.\n"
                    | (roomNum `elem` [2,8,6,17,10,20]) = "I hear the bats.\n"
                    | (roomNum `elem` [4,6,14,18,9,20]) = "I feel a breeze.\n"
                    | otherwise                       = ""
           
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