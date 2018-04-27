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

-- type CaveMap = [Room]
-- data Room = BatRm | PitRm | WumpusRm | EmptyRm
--   deriving (Eq,Data,Typeable)

data userInput = userInput {getStage::String, getCommand::String, getValue::String}

--data RoomType = BatRm | PitRm | WumpusRm | EmptyRm
--data Room = Room Int RoomType

instance Show Room where
  show BatRm    = "BatRm"
  show PitRm    = "PitRm"
  show WumpusRm = "WumpusRm"
  show EmptyRm  = "EmptyRm"


huntServer :: IO ()
huntServer = do
    args <- map read <$> getArgs
    -- create the map
    --caveMap :: CaveMap
    --caveMap <- [BatRm, PitRm, WumpusRm, Empty, Empty,
    --            Empty, Empty, BatRm, PitRm , Empty,
    --            BatRm, Empty, PitRm, Empty, Empty,
    --            Empty, Empty, Empty, PitRm, Empty]
    -- gen <- getStdGen
    -- num <- return (fst $ randomR (1,10) gen :: Int)
    let port = if null args then 2018 else head args
    serverWith defaultConfig { srvLog = stdLogger, srvPort = port } $ handleGuess --num

handleGuess :: Handler String
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
handleGuess addr url req | (stage input) == "Welcome" = if ((value input) == "y")
                                                        then return $ sendText OK ("Game Started ") 
                                                        else (if ((value input) == "i")
                                                                then (return $ sendText OK( "Instructions: \n \
                                                                              \ You have 1 arrow that can shoot down a path of 5 rooms. \n \
                                                                              \ You will be prompted with the rooms that it will travel. \n \
                                                                              \ Enter \"y\" to start. \n " ))
                                                                else return $ sendText OK ("Invalid command"))) 
                         | otherwise                = return $ sendText OK ("huh")))
  where input = decodeJSON $ rqBody req


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