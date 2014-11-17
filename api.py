from bottle import route, run, template, post, get, response
from json import dumps, loads

import numpy as np

with open('../data/combined.txt') as f:
	combined_str = f.read()
	combined = loads(combined_str)


@route('/api')
def index():
	return "welcome to the API! Enjoy your stay"

@route('/api/allPlayers')
def index():
	rv = [{"turn" : "down", "for" : "what"}]
	response.content_type = "application/json"
	return dumps(rv)

@route('/api/whateva')
def index():
	response.content_type = "application/json"
	return combined_str



run(host='localhost', port=8080)