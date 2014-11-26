class Container:
	def __init__(self, numeric_keys, data, means):
		self.numeric_keys = numeric_keys
		self.data = data
		self.means = means

	def _strpairs(self):
		out = []
		for x in self.data:

			row = [["name", str(x["name"])], ["id", str(x["id"])]]

			for key in self.numeric_keys:
				val = x[key] / self.means[key]
				row.append([key, str(val)])

			out.append(row)
		return out

	def strpairs(self):
		if hasattr(self, "_strpairs_cache"):
			return self._strpairs_cache
		else:
			out = self._strpairs()
			self._strpairs_cache = out
			return out