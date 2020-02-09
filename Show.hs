module Show where

import Prelude hiding (lookup)
import Data.Map (Map, lookup)
import Data.Maybe (fromJust)
import Confrontation (Region(..), Piece(..))
import BoardState (GameState, positions)

showPositions :: Map Region [Piece] -> [Region] -> String
showPositions regionMap selectedRegions = unlines [
    "                           +-----------------+",
    "                           | " ++ f Mordor   ++ "|",
    "                           | ------          |",
    "                           | " ++ g Mordor 0 ++ " |",
    "                           | " ++ g Mordor 1 ++ " |",
    "                           | " ++ g Mordor 2 ++ " |",
    "                           | " ++ g Mordor 3 ++ " |",
    "                  +--------+--------+--------+--------+",
    "                  | " ++ f Dagorlad   ++ "| " ++ f Gondor   ++ "|",
    "                  | --------        | ------          |",
    "                  | " ++ g Dagorlad 0 ++ " | " ++ g Gondor 0 ++ " |",
    "                  | " ++ g Dagorlad 1 ++ " | " ++ g Gondor 1 ++ " |",
    "         +--------+--------+--------+--------+--------+--------+",
    "         | " ++ f Mirkwood   ++ "| " ++ f Fangorn   ++ "| " ++ f Rohan   ++ "|",
    "         | --------        | -------         | -----           |",
    "         | " ++ g Mirkwood 0 ++ " | " ++ g Fangorn 0 ++ " | " ++ g Rohan 0 ++ " |",
    "         | " ++ g Mirkwood 1 ++ " | " ++ g Fangorn 1 ++ " | " ++ g Rohan 1 ++ " |",
    "+--------+--------+--------+--------+--------+--------+--------+--------+",
    "| " ++ f TheHighPass   ++ "| " ++ f MistyMountains   ++ "| " ++ f Moria   ++ "| " ++ f GapOfRohan   ++ "|",
    "| -------------   | --------------- | -----           | ------------    |",
    "| " ++ g TheHighPass 0 ++ " | " ++ g MistyMountains 0 ++ " | " ++ g Moria 0 ++ " | " ++ g GapOfRohan 0 ++ " |",
    "+--------+--------+--------+--------+--------+--------+--------+--------+",
    "         | " ++ f Rhudaur   ++ "| " ++ f Eregion   ++ "| " ++ f Enedwaith   ++ "|",
    "         | -------         | -------         | ---------       |",
    "         | " ++ g Rhudaur 0 ++ " | " ++ g Eregion 0 ++ " | " ++ g Enedwaith 0 ++ " |",
    "         | " ++ g Rhudaur 1 ++ " | " ++ g Eregion 1 ++ " | " ++ g Enedwaith 1 ++ " |",
    "         +--------+--------+--------+--------+--------+--------+",
    "                  | " ++ f Arthedain   ++ "| " ++ f Cardolan   ++ "|",
    "                  | ---------       | --------        |",
    "                  | " ++ g Arthedain 0 ++ " | " ++ g Cardolan 0 ++ " |",
    "                  | " ++ g Arthedain 1 ++ " | " ++ g Cardolan 1 ++ " |", 
    "                  +--------+--------+--------+--------+",
    "                           | " ++ f TheShire   ++ "|",
    "                           | ---------       |",
    "                           | " ++ g TheShire 0 ++ " |",
    "                           | " ++ g TheShire 1 ++ " |",
    "                           | " ++ g TheShire 2 ++ " |",
    "                           | " ++ g TheShire 3 ++ " |",
    "                           +-----------------+"
    ]
    where
        f :: Region -> String
        f region = regionTitle region ++ if region `elem` selectedRegions then "*" else " "

        g :: Region -> Int -> String
        g region index = case x of
                Just string -> string
                Nothing -> "               "
            where
                x = do
                        pieces <- lookup region regionMap
                        piece <- maybeAt index pieces
                        return $ show piece

        maybeAt :: Int -> [x] -> Maybe x
        maybeAt _ [] = Nothing
        maybeAt 0 (x:xs) = Just x
        maybeAt nonzero (x:xs) = maybeAt (nonzero - 1) xs

        regionTitle :: Region -> String
        regionTitle TheShire       = "The Shire      "
        regionTitle Cardolan       = "Cardolan       "
        regionTitle Arthedain      = "Arthedain      "
        regionTitle Enedwaith      = "Enedwaith      "
        regionTitle Eregion        = "Eregion        "
        regionTitle Rhudaur        = "Rhudaur        "
        regionTitle GapOfRohan     = "Gap of Rohan   "
        regionTitle Moria          = "Moria          "
        regionTitle MistyMountains = "Misty Mountains"
        regionTitle TheHighPass    = "The High Pass  "
        regionTitle Rohan          = "Rohan          "
        regionTitle Fangorn        = "Fangorn        "
        regionTitle Mirkwood       = "Mirkwood       "
        regionTitle Gondor         = "Gondor         "
        regionTitle Dagorlad       = "Dagorlad       "
        regionTitle Mordor         = "Mordor         "

instance Show Piece where
    show Frodo        = "Frodo          "
    show Pippin       = "Pippin         "
    show Gandalf      = "Gandalf        "
    show Sam          = "Sam            "
    show Legolas      = "Legolas        "
    show Aragorn      = "Aragorn        "
    show Gimli        = "Gimli          "
    show Merry        = "Merry          "
    show Boromir      = "Boromir        "
    show Orcs         = "Orcs           "
    show Shelob       = "Shelob         "
    show Saruman      = "Saruman        "
    show FlyingNazgul = "Flying Nazgul  "
    show Balrog       = "Balrog         "
    show Warg         = "Warg           "
    show BlackRider   = "Black Rider    "
    show WitchKing    = "Witch King     "
    show CaveTroll    = "Cave Troll     "

instance Show GameState where
    show gameState = showPositions (positions gameState) []
