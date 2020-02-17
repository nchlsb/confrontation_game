import Movement
import ToHtml
import Confrontation
import GameState

main :: IO ()
main = do
    putStrLn "Enter a piece:"
    pieceString <- getLine
    writeFile "output.html" $ toHtml sampleStartingState $ possibleMoves sampleStartingState (read pieceString :: Piece)