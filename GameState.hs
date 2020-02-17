module GameState where

import Cards (GoodCard(..), DarkCard(..))
import Confrontation (sideOf, Region(..), isMountain, Piece(..), Side(..))

pieces :: [Piece]
pieces =
    [ Frodo
    , Pippin
    , Gandalf
    , Sam
    , Legolas
    , Aragorn
    , Gimli
    , Merry
    , Boromir
    , Orcs
    , Shelob
    , Saruman
    , FlyingNazgul
    , Balrog
    , Warg
    , BlackRider
    , WitchKing
    , CaveTroll
    ]

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

initialDarkCards :: [DarkCard]
initialDarkCards =
    [ Dark1
    , Dark2
    , Dark3
    , Dark4
    , Dark5
    , Dark6
    , DarkMagic
    , DarkRetreat
    , EyeOfSauron
    ]

data GameState = GameState
    { positions :: [(Piece, Region)]
    , goodCards :: [GoodCard]
    , darkCards :: [DarkCard]
    }

samplePositions :: [(Piece, Region)]
samplePositions = zip pieces
    [ TheShire
    , TheShire
    , TheShire
    , TheShire
    , Cardolan
    , Arthedain
    , Enedwaith
    , Eregion
    , Rhudaur
    , Rohan
    , Fangorn
    , Mirkwood
    , Gondor
    , Dagorlad
    , Mordor
    , Mordor
    , Mordor
    , Mordor
    ]

sampleStartingState :: GameState
sampleStartingState = GameState
    { positions = samplePositions
    , goodCards = initialGoodCards
    , darkCards = initialDarkCards
    }

