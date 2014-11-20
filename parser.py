from bs4 import BeautifulSoup

with open("yahoosports.html","r") as f:
	parsed_html = BeautifulSoup(f.read())

stat_names = [x.text for x in parsed_html.find_all("th", class_ = "ys-stat")]

def floatify(x):
	try:
		return float(x)
	except ValueError:
		return x

def mean(arr):
	nums = [x for x in arr if isinstance(x,int) or isinstance(x,float)]
	if len(nums) == 0:
		print arr
		assert False
	return sum(nums)/len(nums)

def num_rat(arr):

	if len(arr) == 0:
		print arr
		assert False

	num_count = 0
	for x in arr:
		if isinstance(x,int) or isinstance(x,float):
			num_count += 1
	return num_count/float(len(arr))

def all_keys(dicts):
	out = set()
	for d in dicts:
		out.update(d.keys())
	return out

data = []


# get all the players and add the team name, position, and player name

id_counter = 1

for player in parsed_html.find_all("tr", class_ = "ys-player"):

	player_raw = [x.text for x in player.find_all("td")]

	# Last td is blank
	player_raw.pop()

	# there are some useless tds at the beginning if we go from right to left we can avoid
	# including them
	stat_pairs = []
	for i in range(-1, -len(stat_names), -1):
		stat_pairs.append( (stat_names[i], floatify(player_raw[i]) ) )

	#stat_pairs.reverse()

	# the players name is the second span ... kinda janky but whatever
	player_dict = dict(stat_pairs)
	player_dict["name"] = player.find_all("span")[1].text

	# team and position
	abbrs = player.find_all("abbr")

	player_dict["team"] = abbrs[0].text
	player_dict["position"] = abbrs[1].text
	player_dict["id"] = id_counter

	id_counter += 1

	data.append(player_dict)

# get means of numerical columns

means = {}
for key in all_keys(data):
	col = [d[key] for d in data]

	if len(col) == 0:
		print key, "empty key"
		assert False

	if num_rat(col) >= 0.9:
		means[key] = mean(col)

print means






