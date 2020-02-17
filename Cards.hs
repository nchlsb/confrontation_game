module Cards where

import Data.Maybe (fromMaybe)

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

data DarkCard
    = Dark1
    | Dark2
    | Dark3
    | Dark4
    | Dark5
    | Dark6
    | DarkRetreat
    | DarkMagic
    | EyeOfSauron
    deriving (Eq, Ord, Show)

data CardOutcome
    = StrengthIncrease Int Int
    | GoodRetreatsBackwards
    | DarkRetreatsSideways
    | BothRetreat
    | NobleSacrificePlayed
    | GoodMagicPlayed
    | DarkMagicPlayed
    | BothMagic

playCards :: GoodCard -> DarkCard -> CardOutcome
playCards Good1          Dark1 = StrengthIncrease 1 1
playCards Good2          Dark1 = StrengthIncrease 2 1
playCards Good3          Dark1 = StrengthIncrease 3 1
playCards Good4          Dark1 = StrengthIncrease 4 1
playCards Good5          Dark1 = StrengthIncrease 5 1
playCards GoodRetreat    Dark1 = GoodRetreatsBackwards
playCards GoodMagic      Dark1 = GoodMagicPlayed
playCards NobleSacrifice Dark1 = NobleSacrificePlayed
playCards ElvenCloak     Dark1 = StrengthIncrease 0 0

playCards Good1          Dark2 = StrengthIncrease 1 2
playCards Good2          Dark2 = StrengthIncrease 2 2
playCards Good3          Dark2 = StrengthIncrease 3 2
playCards Good4          Dark2 = StrengthIncrease 4 2
playCards Good5          Dark2 = StrengthIncrease 5 2
playCards GoodRetreat    Dark2 = GoodRetreatsBackwards
playCards GoodMagic      Dark2 = GoodMagicPlayed
playCards NobleSacrifice Dark2 = NobleSacrificePlayed
playCards ElvenCloak     Dark2 = StrengthIncrease 0 0

playCards Good1          Dark3 = StrengthIncrease 1 3
playCards Good2          Dark3 = StrengthIncrease 2 3
playCards Good3          Dark3 = StrengthIncrease 3 3
playCards Good4          Dark3 = StrengthIncrease 4 3
playCards Good5          Dark3 = StrengthIncrease 5 3
playCards GoodRetreat    Dark3 = GoodRetreatsBackwards
playCards GoodMagic      Dark3 = GoodMagicPlayed
playCards NobleSacrifice Dark3 = NobleSacrificePlayed
playCards ElvenCloak     Dark3 = StrengthIncrease 0 0

playCards Good1          Dark4 = StrengthIncrease 1 4
playCards Good2          Dark4 = StrengthIncrease 2 4
playCards Good3          Dark4 = StrengthIncrease 3 4
playCards Good4          Dark4 = StrengthIncrease 4 4
playCards Good5          Dark4 = StrengthIncrease 5 4
playCards GoodRetreat    Dark4 = GoodRetreatsBackwards
playCards GoodMagic      Dark4 = GoodMagicPlayed
playCards NobleSacrifice Dark4 = NobleSacrificePlayed
playCards ElvenCloak     Dark4 = StrengthIncrease 0 0

playCards Good1          Dark5 = StrengthIncrease 1 5
playCards Good2          Dark5 = StrengthIncrease 2 5
playCards Good3          Dark5 = StrengthIncrease 3 5
playCards Good4          Dark5 = StrengthIncrease 4 5
playCards Good5          Dark5 = StrengthIncrease 5 5
playCards GoodRetreat    Dark5 = GoodRetreatsBackwards
playCards GoodMagic      Dark5 = GoodMagicPlayed
playCards NobleSacrifice Dark5 = NobleSacrificePlayed
playCards ElvenCloak     Dark5 = StrengthIncrease 0 0

playCards Good1          Dark6 = StrengthIncrease 1 6
playCards Good2          Dark6 = StrengthIncrease 2 6
playCards Good3          Dark6 = StrengthIncrease 3 6
playCards Good4          Dark6 = StrengthIncrease 4 6
playCards Good5          Dark6 = StrengthIncrease 5 6
playCards GoodRetreat    Dark6 = GoodRetreatsBackwards
playCards GoodMagic      Dark6 = GoodMagicPlayed
playCards NobleSacrifice Dark6 = NobleSacrificePlayed
playCards ElvenCloak     Dark6 = StrengthIncrease 0 0

playCards Good1          DarkRetreat = DarkRetreatsSideways
playCards Good2          DarkRetreat = DarkRetreatsSideways
playCards Good3          DarkRetreat = DarkRetreatsSideways
playCards Good4          DarkRetreat = DarkRetreatsSideways
playCards Good5          DarkRetreat = DarkRetreatsSideways
playCards GoodRetreat    DarkRetreat = BothRetreat
playCards GoodMagic      DarkRetreat = GoodMagicPlayed
playCards NobleSacrifice DarkRetreat = DarkRetreatsSideways
playCards ElvenCloak     DarkRetreat = DarkRetreatsSideways

playCards Good1          DarkMagic = DarkMagicPlayed
playCards Good2          DarkMagic = DarkMagicPlayed
playCards Good3          DarkMagic = DarkMagicPlayed
playCards Good4          DarkMagic = DarkMagicPlayed
playCards Good5          DarkMagic = DarkMagicPlayed
playCards GoodRetreat    DarkMagic = DarkMagicPlayed
playCards GoodMagic      DarkMagic = BothMagic
playCards NobleSacrifice DarkMagic = DarkMagicPlayed
playCards ElvenCloak     DarkMagic = DarkMagicPlayed

playCards Good1          EyeOfSauron = StrengthIncrease 1 0
playCards Good2          EyeOfSauron = StrengthIncrease 2 0
playCards Good3          EyeOfSauron = StrengthIncrease 3 0
playCards Good4          EyeOfSauron = StrengthIncrease 4 0
playCards Good5          EyeOfSauron = StrengthIncrease 5 0
playCards GoodRetreat    EyeOfSauron = StrengthIncrease 0 0
playCards GoodMagic      EyeOfSauron = StrengthIncrease 0 0
playCards NobleSacrifice EyeOfSauron = StrengthIncrease 0 0
playCards ElvenCloak     EyeOfSauron = StrengthIncrease 0 0