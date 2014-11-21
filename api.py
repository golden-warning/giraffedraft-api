from bottle import route, run, template, post, get, response
from json import dumps, loads

from logic import Vector, VectorCollection

import random

from parser import data, means
#from parser import data

# NOTE: no average draft pick in numeric keys!
numeric_keys = r"3PTM AST BLK FG% FGA FGM FT% FTA FTM PTS REB ST TO".split(" ")


# sample data

sample = ("""{
    "player1": {"2":{"injured":false,"ADP":"2.6","FGM":"583","FGA":"1102","FG%":".530","FTM":"326","FTA":"411","FT%":".793","3PTM":"2.5","PTS":"1496","REB":"682","AST":"112","ST":"107","BLK":"202","TO":"110","playerName":"Anthony Davis NO - PF,C"}},
    "player2: {
"12":{"injured":false,"ADP":"14.5","FGM":"608","FGA":"1222","FG%":".498","FTM":"172","FTA":"236","FT%":".728","3PTM":"1.6","PTS":"1389","REB":"675","AST":"136","ST":"59.5","BLK":"87.0","TO":"94.9","playerName":"Al Jefferson Cha - PF,C"},
"17":{"injured":false,"ADP":"20.9","FGM":"462","FGA":"967","FG%":".478","FTM":"345","FTA":"422","FT%":".819","3PTM":"65.7","PTS":"1336","REB":"556","AST":"137","ST":"65.7","BLK":"61.2","TO":"122","playerName":"Chris Bosh Mia - PF,C"}}
  }""")


players = VectorCollection.create(dicts = data[:20], means = means, numeric_keys = numeric_keys)

print "=" * 10
print "created!"
print "=" * 10

foo = players.query(
	top_n = 5,
	n = 12,
	player_index = 0,
	players = 3,
	history = None,
	horizon = 3,
	prehistory = None,
	sweep = 1
)

print "\n".join([str(foo[x][0][1]) for x in range(5)])


@route('/api')
def index():
	return "welcome to the API! Enjoy your stay"

@route('/api/allPlayers')
def index():
	response.content_type = "application/json"
	return dumps(data)

@route('/api/means')
def index():
	response.content_type = "application/json"
	return dumps(means)

@route('/test/suggest')
def index():
	response.content_type = "application/json"
	keys = r"3PTM ADP AST BLK FG% FGA FGM FT% FTA FTM PTS REB ST TO".split(" ")

	out = []
	for x in range(3):
		fake_player = { k : random.random() for k in keys }
		fake_player["injured"] = True
		fake_player["playerName"] = "Player #" + str(random.random())
		out.append(fake_player)

	return dumps(out)

@route('/api/suggest'):
def index():
	pass


# run(host='localhost', port=8080)
