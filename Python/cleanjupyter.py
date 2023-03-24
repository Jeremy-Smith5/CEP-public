
## removes output from specified Jupyter notebook (.ipynb) 
## and creates a clean copy with prefix 'no_output_'
## this is just meant as a command line (Linux) tool if
## the Jupyter browser tool is not available / inconvenient

import sys, shutil, json

nb = sys.argv[1]
if not nb.endswith('.ipynb'):
	print ('\n...you must supply a file name ending with .ipynb\n')
	exit()

try:
	f = open(nb, 'r')
	f.close()
except IOError:
	print ('\n...sorry, the file %s does not exist in this directory!\n' % nb)
	exit()

noout = 'no_output_' + nb

shutil.copyfile(nb, noout)

f = open(noout, 'r')
flines = f.readlines()
f.close()

f = open(noout, 'w')

inoutputs = False
n_altered = 0
for ln in flines:
	if ln.strip().startswith('"source":'):
		inoutputs = False
	if inoutputs:
		continue
	if ln.strip().startswith('"outputs": [') and not ln.strip().startswith('"outputs": []'):
		inoutputs = True
		n_altered += 1
		f.write(ln[:ln.index('[') + 1] + '],\n')
	else:
		f.write(ln)

f.close()

# check if the new notebook is still a valid JSON file

f = open(noout, 'r')
fread = f.read()
f.close()

try:
	json.loads(fread)
except ValueError:
	print ('\n*****************************************************************************\n')
	print ('\n...WARNING: the file %s was NOT successfully written as a valid Jupyter file!\n' % noout)
	print ('\n*****************************************************************************\n')
	exit()

print ('\n...the notebook %s has been cleaned of output for %s cells\n' % (noout, n_altered))

