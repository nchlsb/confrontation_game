module Confrontation where

import Data.Map hiding (map, lookup)
import Data.Set hiding (map, insert)


data GoodCard
    = GoodNumberCard Int -- 1 to 5
    | GoodRetreat
    | GoodMagic
    | NobleSacrifice
    | ElvenCloak
    deriving (Eq, Ord, Show)

data EvilCard
    = EvilNumberCard Int -- 1 to 6
    | EvilRetreat
    | EvilMagic
    | EyeOfSauron
    deriving (Eq, Ord, Show)

data GoodCardsState = GoodCardsState {
    hasGood1 :: Bool,
    hasGood2 :: Bool,
    hasGood3 :: Bool,
    hasGood4 :: Bool,
    hasGood5 :: Bool,
    hasGoodMagic :: Bool,
    hasGoodRetreat :: Bool,
    hasNobleSacrifice :: Bool,
    hasElvenCloak :: Bool
}

data EvilCardsState = EvilCardsState {
    hasEvil1 :: Bool,
    hasEvil2 :: Bool,
    hasEvil3 :: Bool,
    hasEvil4 :: Bool,
    hasEvil5 :: Bool,
    hasEvil6 :: Bool,
    hasEvilRetreat :: Bool,
    hasEvilMagic :: Bool,
    hasEyeOfSauron :: Bool
}

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

side :: Piece -> Side
side Frodo = Good
side Pippin = Good
side Gandalf = Good
side Sam = Good
side Legolas = Good
side Aragorn = Good
side Gimli = Good
side Merry = Good
side Boromir = Good
side _ = Bad

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
    Mordor deriving (Show, Eq)

isMountain :: Region -> Bool
isMountain GapOfRohan = True
isMountain Moria = True
isMountain MistyMountains = True
isMountain TheHighPass = True
isMountain _ = False

-- balrogIsInMoria :: BoardState -> Maybe Bool

data GoodInfo = GoodInfo
    (Map Region (Set (Maybe Piece)))
    GoodCardsState
    EvilCardsState
    (Maybe EvilCard)