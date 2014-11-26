# python test script

import os
from subprocess import Popen, PIPE

def query(haskell_args):
	process = Popen(["./query", haskell_args], stdout=PIPE)
	(output, err) = process.communicate()
	exit_code = process.wait()

	print "query in test.py :", output

	return output
