from bottle import route, run, template, post, get, response
from json import dumps, loads

import random

from parser import data, means
#from parser import data


print "cheese"


#import numpy as np

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

@route('/test/suggestions')
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




run(host='0.0.0.0', port=8080)
