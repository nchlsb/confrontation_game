-- data Card = Good GoodCard | Bad BadCard

data GoodCard =
    GoodNumberCard Int | -- 1 to 5
    GoodRetreat |
    GoodMagic |
    NobleSacrifice |
    ElvenCloak deriving (Show, Eq, Ord)

data BadCard =
    BadNumberCard Int | -- 1 to 6
    BadRetreat |
    BadMagic |
    EyeOfSauron deriving (Show, Eq, Ord)

data Piece =
    Frodo |
    Pippin |
    Gandalf |
    Sam |
    Legolas |
    Aragorn |
    Gimli |
    Merry |
    Boromir |
    Orcs |
    Shelob |
    Saruman |
    FlyingNazgul |
    Balrog |
    Warg |
    BlackRider |
    WitchKing |
    CaveTroll deriving (Show, Eq, Ord)

{--

class List
abstract List map()

class Nil extends List
public Nil()
@Override
map() return Nil()

class Cons extends List
private int car
private List cdr
public Cons(int a, List b)

map() return new Cons(f(car), map(cdr))

--}

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
    Mordor deriving (Show)

isMountain :: Region -> Bool
isMountain GapOfRohan = True
isMountain Moria = True
isMountain MistyMountains = True
isMountain TheHighPass = True
isMountain _ = False

-- balrogIsInMoria :: BoardState -> Maybe Bool

data BoardState = BoardState
    (Set GoodCard) -- discarded good cards
    (Set BadCard) -- discarded bad cards
    (Set Piece) -- dead pieces
    (Map Region (Set Piece)) -- The regions' occupants

removeFromRegion :: (Map Region (Set Piece)) -> Region -> Piece -> (Map Region (Set Piece))
removeFromRegion regions region piece = insert region (remove piece (lookup region regions)) regions

move :: Piece -> Region -> Region -> BoardState -> BoardState
move x oldRegion newRegion (BoardState goodCards badCards deadPieces positions) =
    (BoardState goodCards badCards deadPieces newPositions) where
        