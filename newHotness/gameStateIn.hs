{-# LANGUAGE DeriveDataTypeable #-}

module GameStateIn where

import Text.JSON.Generic

import GameState

import qualified Data.Map as Map

magicFields = ["id", "name", "yahoo-rank", "playerName", "injured"]

data GameStateIn = GameStateIn { 
	n :: Int,
	playerIndex :: Int,
	history :: [(Int, [(String, String)])],
	horizon :: Int,
	preHistory :: [(Int, [(String, String)])],
	sweep :: Int,
	ignoreFirst :: [Int],
	inventory :: [[(String, String)]],
	players :: Int
} deriving (Eq, Show, Data, Typeable)

readGame :: String -> GameStateIn
readGame = decodeJSON

readPlayer :: [(String, String)] -> Player
readPlayer [] = Player Map.empty Map.empty
readPlayer ((key, value):rest) 
	| key `elem` magicFields = 
		player { playerData = Map.insert key value (playerData player) }
	| otherwise =
		player { playerNumeric = Map.insert key (read value) (playerNumeric player) } where
			player = readPlayer rest


gameState_of :: GameStateIn -> GameState
gameState_of g =
	GameState {
		gsN = n g,
		gsPlayerIndex = playerIndex g,
		gsHistory = History [(index, readPlayer list) | (index, list) <- history g],
		gsHorizon = 3,
		gsPrehistory = History [(index, readPlayer list) | (index, list) <- preHistory g],
		gsSweep = sw (sweep g),
		gsIgnoreFirst = ignoreFirst g,
		gsInventory = [readPlayer list | list <- inventory g],
		gsPlayers = players g
	} where
		sw 1 = SweepRight
		sw (-1) = SweepLeft
		sw _ = error "illegal sweeping direction"

gameState_of_string :: String -> GameState
gameState_of_string st = gameState_of (readGame st)
