-- test

import System.Environment

import Basketball (topN)
import GameState
import GameStateIn (gameState_of_string)

main = getArgs >>=  (\ xs -> putStrLn (show (topN' 5 (head xs))))

topN' :: Int -> String -> [History]
topN' i gs = topN i (gameState_of_string gs)


sample = "{\"players\" : 10, \"n\" : 4, \"playerIndex\" : 3,\"history\" : [[1, [[\"4\",\"5\"]]]],\"horizon\" : 3,\"preHistory\" : [],\"sweep\" : 1,\"ignoreFirst\" : [],\"inventory\" : []}"