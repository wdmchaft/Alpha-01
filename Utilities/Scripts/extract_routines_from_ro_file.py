#!/usr/bin/python
#
# Usage:
#
#   extract_routines_from_ro_file.py   VISTA.ro
#
#--------------------------------------------
#
#  Based on a script written by Brad King
#
#--------------------------------------------

import sys
import string

inputFile = open(sys.argv[1],'r')

#
# Discard the first two lines
#
sys.stdout.write(inputFile.readline())
sys.stdout.write(inputFile.readline())

outputFile = None

state = 'FirstLine'

for line in inputFile:
	if state == 'FirstLine':
		name,up,rest = line.partition('^')
		directory = name[0]
		directory = directory.capitalize()
		name = name.strip()
		outputFile = open(directory+'/'+name+'.m','w')
		sys.stdout.write('Extracting Routine %s\n' % name)
		state = 'InsideRoutine'
	elif state == 'InsideRoutine':
		if line == '\n':
			outputFile.close()
			state = 'FirstLine'
		else:
			outputFile.write(line)

#
#  Close the last file
#
if outputFile:
	outputFile.close()
	outputFile = None
