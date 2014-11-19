from bottle import route, run, template, post, get, response
from json import dumps, loads

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



run(host='0.0.0.0', port=8080)
