module Movement where

import Confrontation (Region(..), Piece(..), Side (..), sideOf, isMountain)
import GameState (GameState(..), positions, regions)
import Data.Maybe (fromJust)
import Data.List (nub)

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

opponentOccupiesRegion :: GameState -> Side -> Region -> Bool
opponentOccupiesRegion (GameState positions _ _) side region = any (\(piece, region') -> sideOf piece /= side && region == region') positions

numberOfOccupants :: GameState -> Region -> Int
numberOfOccupants gameState region = count (\(_, region') -> region' == region) (positions gameState)
    where
        count :: (a -> Bool) -> [a] -> Int
        count _ [] = 0
        count p (x:xs) = (if p x then 1 else 0) + count p xs

canMoveIntoRegion :: GameState -> Side -> Region -> Bool
canMoveIntoRegion gameState side region =
    opponentOccupiesRegion gameState side region ||
    numberOfOccupants gameState region < (maximumCapacity region)

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


standardDarkMoves :: GameState -> Region -> [Region]
standardDarkMoves gameState region = filter isLegalMove potentialMoves
    where
        isLegalMove :: Region -> Bool
        isLegalMove move = canMoveIntoRegion gameState Dark move

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
            filter (opponentOccupiesRegion gameState Dark) $ 
            map snd $ 
            filter (\(source, _) -> source == region) $
            sideways ++ towardsTheShire

-- The Witch King can move sideways into an adjacent region
-- if he attacks at least one enemy character by doing so.
witchKingMoves :: GameState -> Region -> [Region]
witchKingMoves gameState region = standardDarkMoves gameState region ++ attacks
    where
        attacks :: [Region]
        attacks =
            filter (opponentOccupiesRegion gameState Good) $
            map snd $
            filter (\(source, _) -> source == region) $
            sideways

-- The Black Rider can move forward any number of regions if he attacks
-- at least one enemy character by doing so. If the Black Rider does
-- not want to attack, then he can only move forward into an adjacent region
-- like the other characters. The Black Rider may never move into or through
-- a region containing the maximum number of Dark characters, nor may he move
-- through a region occupied by one or more enemies.
blackRiderMoves :: GameState -> Region -> [Region]
blackRiderMoves gameState region = nub $ standardDarkMoves gameState region ++ attacks
    where
        attacks :: [Region]
        attacks = go region

        go region' = if opponentOccupiesRegion gameState Dark region' then [region'] else concatMap go $ standardDarkMoves gameState region'

flyingNazgulMoves :: GameState -> Region -> [Region]
flyingNazgulMoves gameState region = nub $ standardDarkMoves gameState region ++ attacks
    where
        attacks :: [Region]
        attacks =
            filter
            (\region' -> numberOfOccupants gameState region' == 1 && opponentOccupiesRegion gameState Dark region')
            regions 



possibleMoves :: GameState -> Piece -> [Region]
possibleMoves gameState piece = case lookup piece (positions gameState) of
        Just region -> (case piece of
            Aragorn -> aragornMoves
            WitchKing -> witchKingMoves
            FlyingNazgul -> flyingNazgulMoves
            BlackRider -> blackRiderMoves
            standardPiece -> case sideOf standardPiece of
                Good -> standardGoodMoves
                Dark -> standardDarkMoves) gameState region
        Nothing -> []