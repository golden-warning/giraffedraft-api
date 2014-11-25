module GameState where

import Data.Map

data Player = Player {playerNumeric :: Map String Double, playerData :: Map String String} deriving (Show, Eq)

data Vector = VectorPlayer Player | VectorTeam (Map String Double) deriving (Show, Eq)

data Sweep = SweepLeft | SweepRight deriving (Eq, Show)

data History = History [(Int, Player)] deriving (Eq, Show)

data GameState = GameState { 
	gsN :: Int,
	gsPlayerIndex :: Int,
	gsPlayers :: Int,
	gsHistory :: History,
	gsHorizon :: Int,
	gsPrehistory :: History,
	gsSweep :: Sweep,
	gsIgnoreFirst :: [Int],
	gsInventory :: [Player]
} deriving (Eq, Show)