# Client-server Hunt the Wumpus Game in Haskell
By Claudia Rojas and Wendy Ni

This is our final project for Marist College's Language Study course.

## Setup the game
1. Download this repository
2. Unzip the files
3. Make sure you have the the GHC compiler and the Cabal and Stack tools
* You can download the package with all three here: https://www.haskell.org/downloads
4. Open a terminal and enter the HuntTheWumpus directory
``` cd /RojasNi-FinalProject/HuntTheWumpus ```
5. Build the game with the following command:
``` stack build ```
6. Once the build completes, run the following command to start the server
``` stack exec HuntTheWumpus ```
7. Open another terminal and enter the HuntTheWumpus directory
``` cd /RojasNi-FinalProject/HuntTheWumpus ```
8. Then run the following command to start the client and the game
``` stack exec HuntTheWumpus ```

## How to play
This is a 1-player game. You are the player, who has been placed in a random room in a cave containing 20 rooms.

You see three tunnels to other rooms that are either  
  a) just like this one with nothing  
  b) contains bats that will transport you to a random room  
  c) contains a bottomless pit, in which you will fall into and die  
  d) contains a wumpus that will eat you alive  

To win the game, you have to find and kill the wumpus before it finds you. You have three special abilities:  
  1) Smell - you can smell the smelly wumpus that is in one of the adjacent rooms  
  2) Feel  - you can feel the cool breeze from the bottomless pit that is in one of the adjcaent rooms  
  3) Hear  - you can hear the bats that are in one of the adjacent rooms  

You also have a special crooked arrow that you will use to kill the wumpus. This special arrow can travel down a max number of 5 rooms from your current room.

Every turn you can enter the following commands to:  
  a) move # - where # is the one of the three adjacent rooms  
  b) shoot # - where # is the first room that the crooked arrow will go  

### Are you ready to hunt the wumpus?
