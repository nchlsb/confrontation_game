module BoardState where

import Data.Map (Map, lookup)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)
import Confrontation (sideOf, Region(..), isMountain, GoodCard(..), EvilCard(..), Piece(..), Side(..))


regions :: [Region]
regions = 
    [ TheShire
    , Cardolan
    , Arthedain
    , Enedwaith
    , Eregion
    , Rhudaur
    , GapOfRohan
    , Moria
    , MistyMountains
    , TheHighPass
    , Rohan
    , Fangorn
    , Mirkwood
    , Gondor
    , Dagorlad
    , Mordor
    ]

initialGoodCards :: [GoodCard]
initialGoodCards =
    [ Good1
    , Good2
    , Good3
    , Good4
    , Good5
    , GoodRetreat
    , GoodMagic
    , NobleSacrifice
    ]

initialEvilCards :: [EvilCard]
initialEvilCards =
    [ Evil1
    , Evil2
    , Evil3
    , Evil4
    , Evil5
    , Evil6
    , EvilMagic
    , EvilRetreat
    , EyeOfSauron
    ]

data GameState = GameState
    { positions :: Map Region [Piece]
    , goodCards :: [GoodCard]
    , evilCards :: [EvilCard]
    }

occupants :: GameState -> Region -> [Piece]
occupants gameState region = (fromJust . lookup region . positions) gameState

opponentOccupiesRegion :: GameState -> Side -> Region -> Bool
opponentOccupiesRegion gameState side region = case occupants gameState region of
    [] -> False
    (x:_) -> (sideOf x) /= side

canMoveIntoRegion :: GameState -> Side -> Region -> Bool
canMoveIntoRegion gameState side region =
    null (occupants gameState region) ||
    opponentOccupiesRegion gameState side region ||
    length (occupants gameState region) < (maximumCapacity region)

maximumCapacity :: Region -> Int
maximumCapacity region
    | region == TheShire || region == Mordor = 4
    | isMountain region = 1
    | otherwise = 2
