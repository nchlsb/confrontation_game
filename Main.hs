import Movement
import Show
import Confrontation
import BoardState

showMoves :: Side -> Region -> IO ()
showMoves Good region = putStr $ showPositions (positions sampleStartingState) $ standardGoodMoves sampleStartingState region
showMoves Evil region = putStr $ showPositions (positions sampleStartingState) $ standardEvilMoves sampleStartingState region

main :: IO ()
main = putStr $ showPositions (positions sampleStartingState) $ flyingNazgulMoves sampleStartingState Dagorlad
-- main = print $ sampleStartingState