-- test

import System.Environment

import Basketball (topN)
import GameState
import GameStateIn (gameState_of_string)

import Data.Map ((!))

main = do
	[f] <- getArgs
	s <- readFile f
	putStrLn $ show (map getName $ topN' 10 s)

getName (History ( (playerIndex, x) :_)) = (playerData x) ! "name"

topN' :: Int -> String -> [History]
topN' i gs = topN i (gameState_of_string gs)


sample = "{\"players\" : 10, \"n\" : 4, \"playerIndex\" : 3,\"history\" : [[1, [[\"4\",\"5\"]]]],\"horizon\" : 3,\"preHistory\" : [],\"sweep\" : 1,\"ignoreFirst\" : [],\"inventory\" : []}" 