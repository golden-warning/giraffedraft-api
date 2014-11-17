import numpy as np

from collections import namedtuple, defaultdict



class VectorContext:
	def __init__(self, dicts):
		self.dicts = dicts
		self.stats = set()
		for d in dicts:
			for key in d:
				self.stats.add(key)

	def apply_stat(self, stat, fun):
		return fun( d[key] for d in self.dicts if key in d )

	def mean_stat(self, stat):
		return self.apply_stat(stat, np.mean)

	def median_stat(self, stat):
		return self.apply_stat(stat, np.median)

	def sum_indices(self, stat, indices):
		out = defaultdict(lambda : 0)
		for index in indices:
			for d in self.dicts:
				for stat, value in d.iteritems():
					out[d] += value

	def unit_weight_list(self):

		def unit_sum(d):
			return sum(d.values())

		return sorted(self.dicts, key=unit_sum, reverse=True)

	def weight_list(self, weights):

		def weight_sum(d):
			out = 0
			for k,v in d.iteritems:
				out += weights[k]*v
			return out

		return sorted(self.dicts, key=weight_sum, reverse=True)



class Player:
	def __init__(self, context, indices, stat_to_weight):
		self.context = context
		self.indices = indices
		self.stat_to_weight = stat_to_weight

	def compare_stats(self, other):
		"compare self and other on chosen stats, return dictionary"
		out = {}
		for (stat, val) in self.stat_to_weight.iteritems():
			# get the scores in each category for self and other
			my_score = self.context.sum_indices( stat, self.indices )
			your_score = other.context.sum_indices( stat, other.indices )

			out[stat] = cmp(my_score, your_score) * val
		return out

	def compare(self, other):
		return sum( self.compare_stats(other).values() )





# testing portion

VectorContext( ({4 : 5}, {6 : 7}) )









