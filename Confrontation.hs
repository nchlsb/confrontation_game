module Confrontation where

import Data.Map hiding (map, lookup)
import Data.Set hiding (map, insert)

data Piece
    = Frodo
    | Pippin
    | Gandalf
    | Sam
    | Legolas
    | Aragorn
    | Gimli
    | Merry
    | Boromir
    | Orcs
    | Shelob
    | Saruman
    | FlyingNazgul
    | Balrog
    | Warg
    | BlackRider
    | WitchKing
    | CaveTroll deriving (Show, Eq, Ord, Read)

strength :: Piece -> Int
strength Frodo = 1
strength Pippin = 1
strength Gandalf = 5
strength Sam = 2
strength Legolas = 3
strength Aragorn = 4
strength Gimli = 3
strength Merry = 2
strength Boromir = 0
strength Orcs = 2
strength Shelob = 5
strength Saruman = 4
strength FlyingNazgul = 3
strength Balrog = 5
strength Warg = 2
strength BlackRider = 3
strength WitchKing = 5
strength CaveTroll = 9

data Side = Good | Dark deriving (Eq, Show)

sideOf :: Piece -> Side
sideOf Frodo = Good
sideOf Pippin = Good
sideOf Gandalf = Good
sideOf Sam = Good
sideOf Legolas = Good
sideOf Aragorn = Good
sideOf Gimli = Good
sideOf Merry = Good
sideOf Boromir = Good
sideOf Orcs = Dark
sideOf Shelob = Dark
sideOf Saruman = Dark
sideOf FlyingNazgul = Dark
sideOf Balrog = Dark
sideOf Warg = Dark
sideOf BlackRider = Dark
sideOf WitchKing = Dark
sideOf CaveTroll = Dark

data Region
    = TheShire
    | Cardolan
    | Arthedain
    | Enedwaith
    | Eregion
    | Rhudaur
    | GapOfRohan
    | Moria
    | MistyMountains
    | TheHighPass
    | Rohan
    | Fangorn
    | Mirkwood
    | Gondor
    | Dagorlad
    | Mordor deriving (Show, Eq, Ord)

isMountain :: Region -> Bool
isMountain GapOfRohan = True
isMountain Moria = True
isMountain MistyMountains = True
isMountain TheHighPass = True
isMountain _ = False

goodImmediatelyDefeats :: Piece -> Piece -> Bool
goodImmediatelyDefeats Merry   WitchKing    = True
goodImmediatelyDefeats Legolas FlyingNazgul = True
goodImmediatelyDefeats Gimli   Orcs         = True