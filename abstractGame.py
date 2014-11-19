from collections import defaultdict

def dictify(itr):
	return dict(enumerate(itr))

def category_compare(d0, d1):
	out = 0
	assert d0.keys() == d1.keys()
	for key in d0:
		out += cmp(d0[key], d1[key])
	return out

def heuristic_individual(d):
	return sum(d.values())

def add_dict(d0,d1):
	out = {}
	if d0.keys() == d1.keys() or isinstance(d1, defaultdict):
		pass
	else:
		print d0, d1
		assert False

	for key in d0:
		out[key] = d0[key] + d1[key]
	return out

def remove_once(lst, item):
	idx = lst.index(item)
	return lst[:idx] + lst[idx+1:]

def eval_history( history, player ):
	player_to_dict = {}
	for name, d in history:
		# update da team
		if name not in player_to_dict:
			player_to_dict[name] = defaultdict( lambda : 0 )

		player_to_dict[name] = add_dict( d, player_to_dict[name] )

	# compute the probability that you win a face-off against any of the other players

	outcomes = [
		category_compare(
			player_to_dict[player],
			player_to_dict[name]
		) for name in player_to_dict if name != player]

	return sum(outcomes)/len(outcomes)

def solve(n, player_index, players, history=None, inventory=None):
	if history is None:
		history = []

	assert inventory is not None

	if n == 0:
		return history

	if n >= 1:
		best = None
		best_score = None
		for item in sorted(inventory, key = heuristic_individual, reverse = True):
			new = {
				"n" : n-1,
				"player_index" : (player_index + 1) % players,
				"history" : history + [(player_index, item)],
				"players" : players,
				"inventory" : [x for x in inventory if x != item]
			}

			cand = solve(**new)
			cand_score = eval_history(cand, player_index)

			better = cand_score > best_score

			if better:
				best = cand
				best_score = cand_score

		return best

	raise ValueError("invalid value for n: " + n)


# average for a dictionary
def average_dicts(lst):
	sum_dict = defaultdict(lambda : 0)
	for d in lst:
		sum_dict = add_dict(d, sum_dict)
	return {k : v/float(len(sum_dict)) for k,v in sum_dict.iteritems()}

# take each team and compute the average stats per player. then see how they do when facing each other
def heuristic_team(history, player):
	player_to_dict = defaultdict(lambda : [])
	for name, d in history:
		player_to_dict[name].append(d)

	outcomes = [
		category_compare(
			average_dicts( player_to_dict[player] ),
			average_dicts( player_to_dict[name] )
		) for name in player_to_dict if name != player]

	return sum(outcomes)/len(outcomes)

def solve_heuristic(n, player_index, players, history=None, inventory=None, horizon=None, prehistory=None):
	"""\
n: 				Total number of moves left
player_index: 	Index of current player
players: 		Number of players in the game
history: 		Current sequence of considered moves
inventory: 		Set of available moves
horizon: 		Number of moves into the future we're allowed to look before falling back to heuristic
prehistory: 	All moves the happened before your current move (not under your control)

	"""
	if history is None:
		history = []

	if prehistory is None:
		prehistory = []

	assert inventory is not None
	assert horizon is not None

	# if n is zero you're done!
	if n == 0:
		return history

	# if horizon is zero, pick the player that maximizes the heuristic

	if horizon == 0:
		best_cand = None
		best_score = None

		for cand in sorted(inventory, key = heuristic_individual, reverse = True):

			cand_history = prehistory + history + [(player_index, cand)]

			cand_score = heuristic_team( cand_history, player_index)

			if cand_score > best_score or best_score is None:
				best_cand = cand
				best_score = cand_score

		return history + [(player_index, best_cand)]


	if n >= 1:
		best = None
		best_score = None
		for item in sorted(inventory, key = heuristic_individual, reverse = True):
			new = {
				"n" : n-1,
				"player_index" : (player_index + 1) % players,
				"history" : history + [(player_index, item)],
				"players" : players,
				"inventory" : remove_once(inventory, item)
				"horizon" : horizon-1,
				"prehistory" : prehistory
			}

			cand = solve_heuristic(**new)
			cand_score = eval_history(cand, player_index)

			better = cand_score > best_score

			if better:
				best = cand
				best_score = cand_score

		return best

	raise ValueError("invalid value for n: " + n)



# testing section

print solve(n = 4, player_index = 0, players = 2, history = None, inventory = [
	{"a" : 1, "b" : 2, "c" : 10},
	{"a" : 3, "b" : 4, "c" : 21},
	{"a" : 9, "b" : 8, "c" : 15},
	{"a" : 3, "b" : 5, "c" : 2}
]);

print average_dicts([
	{"a" : 1, "b" : 2, "c" : 10},
	{"a" : 3, "b" : 4, "c" : 21},
	{"a" : 9, "b" : 8, "c" : 15},
	{"a" : 3, "b" : 5, "c" : 2}
])

print solve_heuristic(n = 4, player_index = 0, players = 2, prehistory = None, horizon = 2, history = None, inventory = [
	{"a" : 1, "b" : 2, "c" : 10},
	{"a" : 3, "b" : 4, "c" : 21},
	{"a" : 9, "b" : 8, "c" : 15},
	{"a" : 3, "b" : 5, "c" : 2}
]);
