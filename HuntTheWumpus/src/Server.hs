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
  -- deriving (Eq,Data,Typeable)

caveMap :: Map
caveMap = 
  let 
    1 = BatRm
    2 = Empty
    3 = PitRm
    4 = Empty
    5 = Empty
    6 = Empty
    7 = BatRm
    8 = PitRm
    9 = Empty
    10 = BatRm
    11 = Empty
    12 = PitRm
    13 = Empty
    14 = Empty
    15 = WumpusRm
    16 = Empty
    17 = Empty
    18 = PitRm
    19 = Empty
    20 = Empty



data UserInput = UserInput {stage::String, command::String, value::String} deriving (Eq,Data,Typeable,Show)

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
handleGuess addr url req | (stage input) == "welcome" = if ((value input) == "y")
                                                        then return $ sendText OK ("Game Started ") 
                                                        else (if ((value input) == "i")
                                                                then (return $ sendText OK( "Instructions: \n \
                                                                              \ This is a 1-player game. You are the player, who has &#x000A;\n \
                                                                              \ been placed in a random room in a cave containing 20 rooms. \n \
                                                                              \ You see three tunnels to other rooms that are either 0x0A. \n \
                                                                              \ a) just like this one with nothing \r\n \
                                                                              \ b) contains bats that will transport you to a random room\n \
                                                                              \ c) contains a bottomless pit, in which you will fall into and die\n \
                                                                              \ d) contains a wumpus that will eat you alive\n \
                                                                              \  To win the game, you have to find and kill the wumpus before\n \
                                                                              \ it finds you. You have three special abilities:\n \
                                                                              \ 1) Smell - you can smell the smelly wumpus that is in one of the\n \
                                                                              \            adjacent rooms\n \
                                                                              \ 2) Feel  - you can feel the cool breeze from the bottomless pit\n \
                                                                              \            that is in one of the adjcaent rooms\n \
                                                                              \ 3) Hear  - you can hear the bats that are in one of the adjacent\n \
                                                                              \            rooms\n \
                                                                              \ You also have a special crooked arrow that you will use to kill the\n \
                                                                              \ wumpus. This special arrow can travel down a max number of 5 rooms\n \
                                                                              \ from your current room.\n \
                                                                              \ Every turn you can enter the following commands to:\n \
                                                                              \ a) move # - where # is the one of the three adjacent rooms\n \
                                                                              \ b) shoot # - where # is the first room that the crooked arrow will go\n \
                                                                              \  Are you ready to hunt the wumpus? Enter [y] to begin" ))
                                                                else return $ sendText OK ("Invalid command"))
                         | otherwise                = return $ sendText OK ("huh")
  where input = decodeJSON $ rqBody req



type Action = String -> command
defAction :: Action
defAction _ = OK ("Invalid command")




sendText :: StatusCode -> String -> Response String
sendText s v = insertHeader HdrContentLength (show (length txt))
            --  $ insertHeader HdrContentEncoding "UTF-8"
             $ insertHeader HdrContentEncoding "text/html"
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