# various utility methods and classes

class ExcludeDict(dict):
	def __init__(self, d, exclude):
		assert isinstance(d, dict)
		assert isinstance(exclude, set)

		self.dict = d
		self.exclude = exclude

	def __contains__(self, x):
		return dict.__contains__(self.d, x) and (x not in self.exclude)

	def __iter__(self,x):
		return (k for k in self.keys() if x not in self.exclude)

	def exclude_copy(self, exclude):
		return ExcludeDict( d = self.d, exclude = self.exclude.union(exclude) )

	def keys(self):
		return [x for x in self.d.keys() if x not in self.exclude]

	def values(self):
		return [self[x] for x in self.keys()]

	def iteritems(self):
		return ( (k,v) for k,v in self.d.iteritems() if k not in self.exclude )

	def __repr__(self):
		return "ExcludeDict: %s" % repr({"d" : self.d, "exclude" : self.exclude})

	