from BeautifulSoup import BeautifulSoup, __version__
import re

print "start"

with open("yahoosports.html","r") as f:
	parsed_html = BeautifulSoup(f.read())

print "done"

def wrap(string):
	return re.compile(r"\b" + string + "\b", re.IGNORECASE)

print parsed_html.body.findAll(True , {"class": wrap("TA-end")})
input()
print parsed_html.body.findAll(True, { "class" : wrap("ys-player") })