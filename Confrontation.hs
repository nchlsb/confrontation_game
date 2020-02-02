module Confrontation where

import Data.Map hiding (map, lookup)
import Data.Set hiding (map, insert)


data GoodCard
    = Good1
    | Good2
    | Good3
    | Good4
    | Good5
    | GoodRetreat
    | GoodMagic
    | NobleSacrifice
    | ElvenCloak
    deriving (Eq, Ord, Show)

data EvilCard
    = Evil1
    | Evil2
    | Evil3
    | Evil4
    | Evil5
    | Evil6
    | EvilRetreat
    | EvilMagic
    | EyeOfSauron
    deriving (Eq, Ord, Show)

data Choice
    = WhereToMove
    | WhichCardToPlay
    | WhichCardToPlayFirst
    | WhichCardToPlaySecond
    | NoCardsArePlayed
    | KillMoriaCrosser
    | RetreatOrFight
    | WhichDiscardedCardToPlay

data BattleOutcome
    = BothDie
    | GoodPieceDies
    | EvilPieceDies
    | EvilRetreatsSideways
    | GoodRetreatsBackwards
    | GoodRetreatSideways

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
    | CaveTroll

battle :: Piece -> Piece -> BattleOutcome
battle Gimli Orcs = EvilPieceDies
battle Merry WitchKing = EvilPieceDies
battle Legolas FlyingNazgul = EvilPieceDies
battle Boromir _ = BothDie 
battle _ Orcs = GoodPieceDies

battle goodPiece evilPiece = case (compare (strength goodPiece) (strength evilPiece)) of
    LT -> GoodPieceDies
    EQ -> BothDie
    GT -> EvilPieceDies



simpleBattle
    :: Piece
    -> Piece
    -> GoodCard
    -> EvilCard
    -> Set GoodCard
    -> Set EvilCard
    -> [BattleOutcome]
simpleBattle = undefined 


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

data Side = Good | Bad deriving (Eq, Show)

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
sideOf _ = Bad

data Region =
    TheShire |
    Cardolan |
    Arthedain |
    Enedwaith |
    Eregion |
    Rhudaur |
    GapOfRohan |
    Moria |
    MistyMountains |
    TheHighPass |
    Rohan |
    Fangorn |
    Mirkwood |
    Gondor |
    Dagorlad |
    Mordor deriving (Show, Eq, Ord)

isMountain :: Region -> Bool
isMountain GapOfRohan = True
isMountain Moria = True
isMountain MistyMountains = True
isMountain TheHighPass = True
isMountain _ = False
