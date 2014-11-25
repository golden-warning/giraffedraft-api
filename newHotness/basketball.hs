module Basketball where

import Text.JSON

import qualified Data.Map as Map
import Data.Map (Map, fromList)

import qualified Data.Function as Function
import Data.Function (on)

import qualified Data.List as List
import Data.List (nub, sortBy, maximumBy)

import qualified Control.Monad as Monad

import qualified Data.Maybe as Maybe
import Data.Maybe (isNothing, fromJust)

import qualified Text.JSON as JSON

import qualified GameStateIn as In

import GameState

-- have to cabal install JSON


playerHeuristic :: Player -> Double
playerHeuristic p = sum $ Map.elems $ playerNumeric p


vectorValues v = Map.elems (map_of_vector v)

map_of_vector :: Vector -> Map String Double
map_of_vector (VectorPlayer p) = playerNumeric p
map_of_vector (VectorTeam t) = t

instance (Ord k, Num a) => Num (Map k a) where
	(+) = Map.unionWith (+)
	(-) = Map.unionWith (-)
	(*) = Map.unionWith (*)

	negate = undefined
	abs = undefined
	signum = undefined
	fromInteger = undefined

instance Num Vector where
	x + y = VectorTeam (map_of_vector x + map_of_vector y)
	x - y = VectorTeam (map_of_vector x - map_of_vector y)
	x * y = VectorTeam (map_of_vector x * map_of_vector y)

	negate = undefined
	abs = undefined
	signum = undefined
	fromInteger = undefined

categoryCount :: Ord a => [a] -> [a] -> Int
categoryCount [] [] = 0
categoryCount (x:xs) (y:ys) = 
	fromEnum (compare x y) + categoryCount xs ys
categoryCount _ _ = undefined



concatHistory :: History -> History -> History
concatHistory (History hist) (History hist') = History (hist ++ hist')

getTeams :: History -> [(Int, Maybe Vector)]
getTeams (History pairs) =
	[(x, get x) | x <- keys] where
		players x = [ player | (idx, player) <- pairs, idx == x ]

		get x 
			| null (players x) = Nothing
			| otherwise = Just (combine (players x))

		combine [] = undefined
		combine pl = foldl1 (+) (map VectorPlayer pl)

		keys = nub (map fst pairs)

-- assumes that all the keys of pairs are unique!
evalTeam :: [(Int, Maybe Vector)] -> Int -> Int
evalTeam pairs idx
	| isNothing myVec = 0
	| otherwise = sum [headToHead myVec p | (i, p) <- pairs , i /= idx ] where
		myVec = Monad.join (lookup idx pairs)

headToHead :: Maybe Vector -> Maybe Vector -> Int
headToHead Nothing _ = 0
headToHead _ Nothing = 0
headToHead (Just v0) (Just v1) = categoryCount (vectorValues v0) (vectorValues v1)

historyHeuristic :: Int -> History -> Int
historyHeuristic i x = evalTeam (getTeams x) i



-- order ["player_index", "history", "inventory", "horizon", "prehistory", "sweep", "ignore_first"]

--data GameState = GameState { 
--	gsN :: Int,
--	gsPlayerIndex :: Int,
--	gsHistory :: History,
--	gsHorizon :: Int,
--	gsPrehistory :: History,
--	gsSweep :: Sweep,
--	gsIgnoreFirst :: [Int],
--	gsInventory :: [Player]
--} deriving (Eq, Show)

-- normalize sweeping direction
-- doesn't check boundaries
normalize :: GameState -> GameState
normalize gs
	| gsPlayerIndex gs >= gsPlayers gs + 1 = error $ "normalize: index too big : " ++ show gs
	| gsPlayerIndex gs <= -2 = error $ "normalize: index too small : " ++ show gs

	| gsPlayerIndex gs == gsN gs =
		gs { gsN = gsN gs - 1, gsSweep = SweepLeft }
	| gsPlayerIndex gs == (-1) =
		gs {gsN = 0, gsSweep = SweepRight }

	| otherwise = gs

skipIndex :: Int -> [a] -> [a]
skipIndex idx xs =
	[x | (i,x) <- zip [0..] xs, i /= idx ]

children :: GameState -> [GameState]
children gs = [ normalize (pickChildren gs i) | i <- [0 .. length (gsInventory gs) - 1], not (elem i (gsIgnoreFirst gs)) ] 
	
pickChildren :: GameState -> Int -> GameState
pickChildren gs i 
	| i >= length (gsInventory gs) = error ("children : " ++ show gs)
	| otherwise = 
		gs {
			gsN = gsN gs - 1,
			gsPlayerIndex = gsPlayerIndex gs + direction,
			gsHistory = concatHistory (gsHistory gs) (History [(gsPlayerIndex gs, picked)]),
			gsHorizon = gsHorizon gs - 1,
			gsIgnoreFirst = [],
			gsInventory = skipIndex i (gsInventory gs)
		} where
			direction = if (gsSweep gs) == SweepRight then 1 else -1

			picked = (gsInventory gs) !! i


sortedInventory :: GameState -> [Player]
sortedInventory gs =
	reverse $ sortBy (compare `on` playerHeuristic) (gsInventory gs)

firstMaxBy :: Ord a => (b -> a) -> [b] -> b
firstMaxBy _ [] = error "firstMaxBy cannot take max of empty list"
firstMaxBy fun xs = fst (maximumBy comparator (zip xs [0 ..])) where
	comparator (x0,i0) (x1,i1) = compare (fun x0, negate i0) (fun x1, negate i1)


-- topNBy :: Ord a => (b -> a) -> Int -> [b] -> [a]
--topNBy _ _ [] = undefined
--topNBy i fun xs = take i $ map fst (sortBy comparator (zip xs [0 ..])) where
--	comparator (x0,i0) (x1,i1) = rev (compare (fun x0, i0) (fun x1, i1))
--
--        rev LT = GT
--        rev EQ = EQ
--        rev GT = LT

       


solve :: GameState -> History
solve gs = solve' (gs {
	gsInventory = sortedInventory gs
})

solve' :: GameState -> History
solve' gs
	-- n is 0, return history
	| gsN gs < 0 =
		error "solve' : negative moves remaining"
	| gsN gs == 0 = 
		gsHistory gs
	| gsHorizon gs < 0 =
		error "solve' : negative horizon"
	-- horizon is zero terminate you're done!
	| gsHorizon gs == 0 =
		gsHistory gs
	| otherwise = 
		firstMaxBy (historyHeuristic (gsPlayerIndex gs)) (map solve' (children gs))

bestMoves :: GameState -> [History]
bestMoves gs = 
      greatestBy (historyHeuristic (gsPlayerIndex gs)) (map solve' (children gs)) where
           greatestBy key x = reverse (sortBy (compare `on` key) x)

topN :: Int -> GameState -> [History]
topN i gs = take i (filter good (bestMoves gs)) where

     good :: History -> Bool
     good (History ((index, player):_) ) = not (player `elem` prohibited)

     prohibited :: [Player]
     prohibited = [(gsInventory gs) `gimme` j | j <- (gsIgnoreFirst gs)]

     gimme :: [a] -> Int -> a
     gimme xs k 
     	| 0 <= k && k < length xs = xs !! k
     	| otherwise = error "out of bounds in topN you should look into that"
      
-- testing section yay! 

--player1 = Player (fromList [("a", 10), ("b", 30)]) Map.empty
--player2 = Player (fromList [("a", 9), ("b", 20)]) Map.empty
--player3 = Player (fromList [("a", 2), ("b", 70)]) Map.empty
--player4 = Player (fromList [("a", 1), ("b", 80)]) Map.empty

--gameState = GameState {
--	gsN = 10,
--	gsHistory = History [],
--	gsPrehistory = History [],
--	gsInventory = [player1, player2, player3, player4],
--	gsHorizon = 3,
--	gsSweep = SweepRight,
--	gsPlayerIndex = 0,
--	gsIgnoreFirst = []
--}

---- crap

----data GameStateJSON = GameStateJSON { 
----	gsN' :: Int,
----	gsPlayerIndex' :: Int,
----	gsHistory' :: [(Int, Map String String)],
----	gsHorizon' :: Int,
----	gsPrehistory' :: [(Int, Map String String)],
----	gsSweep' :: Int,
----	gsIgnoreFirst' :: [Int],
----	gsInventory' :: [Map String String]
----} deriving (Eq, Show)


--gameStateJSON = "{\"n\":10,\"history\":[],\"prehistory\":[],\"inventory\":[],\"horizon\":4,\"sweep\":1,\"playerIndex\":0,\"ignoreFirst\":[]}"

--gameStateParsed :: Result JSValue
--gameStateParsed = decode gameStateJSON

--Ok topObj = gameStateParsed

--JSObject obj2 = topObj

--obj3 = fromJSObject obj2

--getItem :: Eq a => a -> [(a, b)] -> b
--getItem key xs = fromJust (lookup key xs)

--getN = getItem "n" obj3

--fromRight :: Either a b -> b
--fromRight (Right x) = x
--fromRight (Left x) = undefined

--extract :: JSON b => String -> b
--extract x = fromRight $ resultToEither $ readJSON (getItem x obj3)

--n :: Int
--n = extract "n"

--history :: [(Int, Map String String)]
--history = extract "history"

--horizon :: Int
--horizon = extract "horizon"

--prehistory :: [(Int, Map String String)]
--prehistory = extract "prehistory"

--inventory :: [Map String String]
--inventory = extract "inventory"










