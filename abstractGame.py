from collections import defaultdict

def dictify(itr):
	return dict(enumerate(itr))

def category_compare(d0, d1):
	out = 0
	assert d0.keys() == d1.keys()
	for key in d0:
		out += cmp(d0[key], d1[key])
	return out

def heuristic(d):
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
		for item in sorted(inventory, key = heuristic, reverse = True):
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


print solve(n = 4, player_index = 0, players = 3, history = None, inventory = [
	{"a" : 1, "b" : 2},
	{"a" : 3, "b" : 4},
	{"a" : 9, "b" : 8},
	{"a" : 3, "b" : 5}
]);










