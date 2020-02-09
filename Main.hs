import Movement
import Show
import Confrontation
import BoardState

main :: IO ()
main = putStr $ showPositions (positions sampleStartingState) $ standardEvilMove sampleStartingState Fangorn