module Movement where

import Confrontation (Region (..))

sidewaysAttacks :: [(Region, Region)]
sidewaysAttacks =
    [ (Arthedain, Cardolan), (Cardolan, Arthedain)
    , (Rhudaur, Eregion), (Eregion, Rhudaur)
    , (Eregion, Enedwaith), (Enedwaith, Eregion)
    , (Gondor, Dagorlad), (Dagorlad, Gondor)
    , (Rohan, Fangorn), (Fangorn, Rohan)
    , (Fangorn, Mirkwood), (Mirkwood, Fangorn)
    ]

goodMovement :: [(Region, Region)]
goodMovement =
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

evilMovement :: [(Region, Region)]
evilMovement = map (\(a, b) -> (b, a)) goodMovement

shortcuts :: [(Region, Region)]
shortcuts = 
    [ (Eregion, Moria)
    , (Mirkwood, Fangorn)
    , (Fangorn, Rohan)
    ]