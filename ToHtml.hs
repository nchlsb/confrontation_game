module ToHtml where

import Prelude hiding (lookup)
import Data.Map (lookup)
import Data.Maybe (fromJust)
import Confrontation (Region(..), Piece(..))
import GameState (GameState, positions, regions)

showPieceWithSpaces :: Piece -> String
showPieceWithSpaces FlyingNazgul = "Flying Nazgul"
showPieceWithSpaces BlackRider   = "Black Rider"
showPieceWithSpaces WitchKing    = "Witch King"
showPieceWithSpaces CaveTroll    = "Cave Troll"
showPieceWithSpaces x = show x

showRegionWithSpaces :: Region -> String
showRegionWithSpaces TheShire       = "The Shire"
showRegionWithSpaces GapOfRohan     = "Gap of Rohan"
showRegionWithSpaces MistyMountains = "Misty Mountains"
showRegionWithSpaces TheHighPass    = "The High Pass"
showRegionWithSpaces x = show x

toHtml :: GameState -> [Region] -> String
toHtml gameState selectedRegions =
    "<html><head><link rel=\"stylesheet\" href=\"styles.css\"></head><body>" ++
    concatMap toSection regions ++
    "</body></html>"
        where
            toSection :: Region -> String
            toSection region =
                "<section id=\"" ++ show region ++ "\">" ++
                "<h1 " ++ (if region `elem` selectedRegions then "class=\"selected\"" else "") ++ ">" ++ showRegionWithSpaces region ++ "</h1>" ++
                "<ul>" ++ 
                    concatMap (\x -> "<li>" ++ showPieceWithSpaces x ++ "</li>") (piecesInRegion region)
                ++ "</ul></section>"

            piecesInRegion :: Region -> [Piece]
            piecesInRegion region = map fst $ filter (\(_, region') -> region == region') (positions gameState)


showHtml :: GameState -> [Region] -> IO ()
showHtml gameState selectedRegions = writeFile "output.html" $ toHtml gameState selectedRegions