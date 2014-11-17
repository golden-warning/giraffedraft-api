from bs4 import BeautifulSoup
import re

print "start"

with open("yahoosports.html","r") as f:
	parsed_html = BeautifulSoup(f.read())

print "done"



# still only grabbing 35 players wtf

print len(parsed_html.find_all("th", class_ = "ys-stat"))

print "========="

print len(parsed_html.find_all("tr", class_ = "ys-player"))