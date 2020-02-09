module BoardState where

import Data.Map (Map, lookup, fromList)
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

sampleStartingState :: GameState
sampleStartingState = GameState
    { goodCards = initialGoodCards
    , evilCards = initialEvilCards
    , positions = fromList
        [ (TheShire, [Frodo, Sam, Merry, Pippin])
        , (Arthedain, [Legolas])
        , (Cardolan, [Gimli])
        , (Rhudaur, [Boromir])
        , (Eregion, [Gandalf])
        , (Enedwaith, [Aragorn])
        , (TheHighPass, [])
        , (MistyMountains, [])
        , (Moria, [])
        , (GapOfRohan, [])
        , (Mirkwood, [Balrog])
        , (Fangorn, [Shelob])
        , (Rohan, [WitchKing])
        , (Dagorlad, [FlyingNazgul])
        , (Gondor, [BlackRider])
        , (Mordor, [Saruman, Orcs, Warg, CaveTroll])
        ]
    }