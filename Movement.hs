module Movement where

import Confrontation (Region(..), Piece(..), Side (..), sideOf, isMountain)
import BoardState (GameState, positions)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Data.Map (lookup)

sideways :: [(Region, Region)]
sideways =
    [ (Arthedain, Cardolan), (Cardolan, Arthedain)
    , (Rhudaur, Eregion), (Eregion, Rhudaur)
    , (Eregion, Enedwaith), (Enedwaith, Eregion)
    , (Gondor, Dagorlad), (Dagorlad, Gondor)
    , (Rohan, Fangorn), (Fangorn, Rohan)
    , (Fangorn, Mirkwood), (Mirkwood, Fangorn)
    ]

towardsMordor :: [(Region, Region)]
towardsMordor =
    [ (TheShire, Arthedain)
    , (TheShire, Cardolan)
    , (Arthedain, Rhudaur)
    , (Arthedain, Eregion)
    , (Cardolan, Eregion)
    , (Cardolan, Enedwaith)
    , (Rhudaur, TheHighPass)
    , (Rhudaur, MistyMountains)
    , (Eregion, MistyMountains)
    , (Eregion, Moria)
    , (Enedwaith, Moria)
    , (Enedwaith, GapOfRohan)
    , (TheHighPass, Mirkwood)
    , (MistyMountains, Mirkwood)
    , (MistyMountains, Fangorn)
    , (Moria, Fangorn)
    , (Moria, Rohan)
    , (GapOfRohan, Rohan)
    , (Mirkwood, Dagorlad)
    , (Fangorn, Dagorlad)
    , (Fangorn, Gondor)
    , (Rohan, Gondor)
    , (Dagorlad, Mordor)
    , (Gondor, Mordor)
    ]

towardsTheShire :: [(Region, Region)]
towardsTheShire = map (\(a, b) -> (b, a)) towardsMordor

shortcuts :: [(Region, Region)]
shortcuts = 
    [ (Eregion, Fangorn)
    , (Mirkwood, Fangorn)
    , (Fangorn, Rohan)
    ]


lookupAll :: (Eq a) => a -> [(a, b)] -> [b]
lookupAll _ [] = []
lookupAll x ((y1, y2):ys) = if x == y1 then y2 : lookupAll x ys else lookupAll x ys

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

standardGoodMove :: GameState -> Region -> [Region]
standardGoodMove gameState region = filter isLegalMove potentialMoves
    where
        isLegalMove :: Region -> Bool
        isLegalMove move = canMoveIntoRegion gameState Good move

        potentialMoves :: [Region]
        potentialMoves = lookupAll region (towardsMordor ++ shortcuts)


standardEvilMove :: GameState -> Region -> [Region]
standardEvilMove gameState region = filter isLegalMove potentialMoves
    where
        isLegalMove :: Region -> Bool
        isLegalMove move = canMoveIntoRegion gameState Evil move

        potentialMoves :: [Region]
        potentialMoves = lookupAll region (towardsTheShire)


