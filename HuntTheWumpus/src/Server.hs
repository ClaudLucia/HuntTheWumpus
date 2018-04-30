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

data UserInput = UserInput {stage::String, command::String, value::String} deriving (Eq,Data,Typeable,Show)

-- create the map
caveMap :: CaveMap
caveMap = [Bat,   Pit,   Wumpus, Empty, Empty,
           Empty, Empty, Bat,    Pit,   Empty,
           Bat,   Empty, Pit,    Empty, Empty,
           Empty, Empty, Empty,  Pit,   Empty]

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


huntServer :: IO ()
huntServer = do
    args <- map read <$> getArgs
    gen <- getStdGen
    roomNum <- return (fst $ randomR (1,20) gen :: Int)
    let port = if null args then 2018 else head args
    serverWith defaultConfig { srvLog = stdLogger, srvPort = port } $ handleCmd roomNum


handleCmd :: Int -> Handler String
-- handleGuess addr url req =
--     if (head userCMD) == "start"
--       then (if (last userCMD) == "y"
--               then return $ sendText OK ("Game Started ")
--               --else return $ sendText OK ("Try again... the number is not " ++ (show $ userCMD) ++ "\n")
--               else (if ((last userCMD) == "i")
--                         then (return $ sendText OK( "Instructions: \n \
--                                       \ You have 1 arrow that can shoot down a path of 5 rooms. \n \
--                                       \ You will be prompted with the rooms that it will travel. \n \
--                                       \ Enter \"y\" to start. \n " ))
--                         else return $ sendText OK ("Invalid command")))
--       else 
--   where userCMD = decodeJSON $ rqBody req
handleCmd roomNum addr url req | (stage input) == "welcome" = if ((value input) == "y")
                                                                then return $ sendText OK (handleMove 1 2 ++ (command input)) 
                                                                else return $ sendText OK ("Invalid command")
                               | (stage input) == "game"    = if ((command input) == "move")
                                                                then return $ sendText OK (handleMove roomNum (read (value input)) ++ (command input) )
                                                                else (if ((command input) == "shoot")
                                                                        then return $ sendText OK (handleShoot roomNum (read(value input)) ++ (command input))
                                                                        else return $ sendText OK ("Invalid command"))
                               | otherwise                = return $ sendText OK ("huh")
  where input = decodeJSON $ rqBody req

handleMove :: Int -> Int -> String
handleMove roomNum currRoom | (roomNum `elem` (paths !! (currRoom-1))) = "You are now in room" 
                                                                       ++ show roomNum
                                                                      --  ++ "Tunnel leads to "
                                                                      --  ++ (printRooms (paths !! (roomNum-1)))
                            | otherwise = "Invalid move"

handleShoot :: Int -> Int -> String
handleShoot roomNum currRoom | (roomNum `elem` (paths !! (currRoom-1))) = "You are now in room" 
                                                                        ++ show roomNum
                                                                        ++ "Tunnel leads to "
                                                                        ++ (printRooms (paths !! (roomNum-1)))
                            | otherwise = "Invalid move"

printRooms :: [Int] -> String
printRooms (x:xs) = (show x) ++ printRooms xs

sendText :: StatusCode -> String -> Response String
sendText s v = insertHeader HdrContentLength (show (length txt))
             $ insertHeader HdrContentEncoding "UTF-8"
             $ insertHeader HdrContentEncoding "text/plain"
             $ (respond s :: Response String) { rspBody = txt }
  where
    --txt = encodeString v
    txt = encodeJSON v

--functions needed:
--handle shoot
--room numbers
--handle commands(shoot and move, bats and pit)
  --if command = shoot 
      --then handle shoot rm
      --else (if command = move 
               --then user location = rm
                 --else sendText "invalid command")
--handle initial(y and i)
--move user to random room