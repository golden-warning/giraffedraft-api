from bottle import route, run, template, post, get, response
from json import dumps, loads
from parser import data


print "cheese"


import numpy as np

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
