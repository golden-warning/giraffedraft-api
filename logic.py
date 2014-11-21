from abstractGame import solve_heuristic, solve_heuristic_args

def fill_seq(*args):
	out = {}
	for d in args:
		for (k,v) in d.iteritems():
			out[k] = v
	return out

class Vector(dict):
	def __init__(self, iterable, data, id_, means):
		# copy the iterable keys into the dictionary that you inherit from
		for key, value in iterable.iteritems():
			self[key] = value/means[key]

		self.raw = iterable

		assert isinstance(data, dict)
		assert isinstance(id_, int)

		self.data = data
		self.id = id_

	# def __repr__(self):

	# 	out = {}
	# 	out["raw"] = self.raw
	# 	out["data"] = self.data
	# 	out["id"] = self.id
	# 	out["numeric"] = dict.__repr__(self)

	# 	return "Vector: %s" % repr(out)

	def __repr__(self):
		return "Vector # %d : %s" % (self.id, repr(self.data))

class VectorCollection:
	def __init__(self, objs):
		self.list = []
		for obj in objs:
			if isinstance(obj, Vector):
				self.list.append(obj)
			else:
				self.list.append(Vector(**obj))

	@classmethod
	def create(cls, dicts, means, numeric_keys):
		objs = []
		for id_, d in enumerate(dicts):

			objs.append({
				"iterable" : {k : v for k,v in d.iteritems() if k in numeric_keys},
				"data" : { k : v for k,v in d.iteritems() if k not in numeric_keys},
				"id_" : id_,
				"means" : means
			})
		return VectorCollection(objs)

	def query(self,**stat):
		# subset of args
		assert all( (arg in stat) for arg in solve_heuristic_args if arg not in ["ignore_first", "inventory"] )
		assert "ignore_first" not in stat

		assert "top_n" in stat

		out = []
		excluded_ids = []
		for x in range(stat["top_n"]):

			# build argument object! gotta take away some keys and add others
			# it's nuts

			# print excluded_ids

			arg_obj = fill_seq(stat,
				{
					"ignore_first" : excluded_ids,
					"inventory" : self.list
				}
			)

			del arg_obj["top_n"]

			hist = solve_heuristic(**arg_obj)
			recommended_player = hist[0][1]

			out.append(hist)

			excluded_ids.append(recommended_player.id)

		return out;




		


