module Movement where

import Confrontation (Region(..), Piece(..), Side (..), sideOf, isMountain)
import BoardState (GameState, positions, regions)
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

standardGoodMoves :: GameState -> Region -> [Region]
standardGoodMoves gameState region = filter isLegalMove potentialMoves
    where
        isLegalMove :: Region -> Bool
        isLegalMove move = canMoveIntoRegion gameState Good move

        potentialMoves :: [Region]
        potentialMoves = lookupAll region (towardsMordor ++ shortcuts)


standardEvilMoves :: GameState -> Region -> [Region]
standardEvilMoves gameState region = filter isLegalMove potentialMoves
    where
        isLegalMove :: Region -> Bool
        isLegalMove move = canMoveIntoRegion gameState Evil move

        potentialMoves :: [Region]
        potentialMoves = lookupAll region (towardsTheShire)

frodoRetreatMoves :: GameState -> Region -> [Region]
frodoRetreatMoves gameState region = undefined -- Can Frodo retreat into spaces occupied by the enemy?

-- Aragorn can move into any adjacent region -- forwards, sideways, or backwards -- if
-- he attacks at least one enemy character by doing so.
aragornMoves :: GameState -> Region -> [Region]
aragornMoves gameState region = standardGoodMoves gameState region ++ attacks
    where
        attacks :: [Region]
        attacks =
            filter (opponentOccupiesRegion gameState Evil) $ 
            map snd $ 
            filter (\(source, _) -> source == region) $
            sideways ++ towardsTheShire

-- The Witch King can move sideways into an adjacent region
-- if he attacks at least one enemy character by doing so.
witchKingMoves :: GameState -> Region -> [Region]
witchKingMoves gameState region = standardEvilMoves gameState region ++ attacks
    where
        attacks :: [Region]
        attacks =
            filter (opponentOccupiesRegion gameState Good) $
            map snd $
            filter (\(source, _) -> source == region) $
            sideways

-- The black Rider can move forward any number of regions if he attacks
-- at least one enemy character by doing so. If the Black Rider does
-- not want to attack, then he can only move forward into an adjacent region
-- like the other characters. The Black Rider may never move into or through
-- a region containing the maximum number of Dark characters, nor may he move
-- through a region occupied by one or more enemies.
blackRiderMoves :: GameState -> Region -> [Region]
blackRiderMoves gameState region = standardEvilMoves gameState region ++ attacks
    where
        attacks :: [Region]
        attacks = undefined

flyingNazgulMoves :: GameState -> Region -> [Region]
flyingNazgulMoves gameState region = standardEvilMoves gameState region ++ attacks
    where
        attacks :: [Region]
        attacks =
            filter
            (\r -> length (occupants gameState r) == 1 && opponentOccupiesRegion gameState Evil r)
            regions 
