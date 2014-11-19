from bs4 import BeautifulSoup
import re

print "start"

with open("yahoosports.html","r") as f:
	parsed_html = BeautifulSoup(f.read())

print "done"



stat_names = [x.text for x in parsed_html.find_all("th", class_ = "ys-stat")]


data = []



for player in parsed_html.find_all("tr", class_ = "ys-player"):

	player_raw = [x.text for x in player.find_all("td")]

	player_raw.pop()


	stat_pairs = []
	for i in range(-1, -len(stat_names), -1):
		stat_pairs.append( (stat_names[i], player_raw[i] ) )

	stat_pairs.reverse()

	data.append(dict(stat_pairs))

print data[:5]



