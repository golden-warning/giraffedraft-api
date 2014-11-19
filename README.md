giraffedraft-api
================

The Python API giraffe draft thing.

documentation

parser.py

parses static html snapshot of yahoo sports page and reads off stats for classes
ys-stat (name of stat)
and ys-player (player stats and name and stuff)

exposes a bunch of dictionaries with all you want to know about the players.

abstractGame.py

a big, complicated module that solves for the next few moves of "perfect play" using minimax

the abstract game (get it?) that it solves has the following rules:

1) you're trying to win as many categories as possible
2) you "win" a category if your team's total is higher than that of the opponent
3) you alternately go back and forth.
