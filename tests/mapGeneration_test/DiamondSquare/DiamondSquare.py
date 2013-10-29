from Queue import Queue
from random import randint, uniform
from math import log


def translate(dimension, row, col):
	return row * dimension + col


def getIndex(array, dimension, row, col):
	return array[row * dimension + col]


def setIndex(array, dimension, row, col, val):
	array[row * dimension + col] = val


def getRandom(maxVal):
	rand = uniform(0.0, 1.0)

	return maxVal * (rand - 0.5)


#################### dimension of subgrid ##############################
def getSquare(array, actualDimension, dimension, x, y):
	a = getIndex(array, actualDimension, x - dimension, y - dimension)
	b = getIndex(array, actualDimension, x - dimension, y + dimension)
	c = getIndex(array, actualDimension, x + dimension, y + dimension)
	d = getIndex(array, actualDimension, x + dimension, y - dimension) 

	return [a, b, c, d]


def getDiamond(array, actualDimension, dimension, x, y):

	a = (x - dimension, y) 
	b = (x, y + dimension)
	c = (x + dimension, y)
	d = (x, y - dimension)

	#print "(%s, %s) - (%s, %s, %s, %s)" % (x, y, a, b, c, d)
	
	counter = 0
	if (a[0] >= 0 and a[0] < actualDimension) and (a[1] >= 0 and a[1] < actualDimension):
		counter = counter + 1
		a = getIndex(array, actualDimension, a[0], a[1])
	else:
		a = 0.0

	if (b[0] >= 0 and b[0] < actualDimension) and (b[1] >= 0 and b[1] < actualDimension):
		counter = counter + 1
		b = getIndex(array, actualDimension, b[0], b[1])
	else:
		b = 0.0

	if (c[0] >= 0 and c[0] < actualDimension) and (c[1] >= 0 and c[1] < actualDimension):
		counter = counter + 1
		c = getIndex(array, actualDimension, c[0], c[1])
	else:
		c = 0.0

	if (d[0] >= 0 and d[0] < actualDimension) and (d[1] >= 0 and d[1] < actualDimension):
		counter = counter + 1
		d = getIndex(array, actualDimension, d[0], d[1])
	else:
		d = 0.0
		
		
		
		
	

	return [a, b, c, d, counter]




def generate(initValA, initValB, initValC, initValD, rangeRand = 255.0, diminish = 0.7, size = 1024):
	if int(log(size) % log(2)) != 0:
		raise Exception("Invalid Dimension")


	arraySize = size + 1
	array = [None] * (arraySize * arraySize)


	# Initialize 4 corners
	setIndex(array, arraySize, 0, 0, initValA + getRandom(2.0))
	setIndex(array, arraySize, 0, arraySize - 1, initValB + getRandom(2.0))
	setIndex(array, arraySize, arraySize - 1, 0, initValC + getRandom(2.0))
	setIndex(array, arraySize, arraySize - 1, arraySize - 1, initValD + getRandom(2.0))


	# Populate
	currentStep = arraySize / 2
	while currentStep > 0:
		# diamond step
		row = currentStep
		while row < arraySize:
			col = currentStep

			while col < arraySize:
				square = getSquare(array, arraySize, currentStep, row, col)
				setIndex(array, arraySize, row, col, (square[0] + square[1] + square[2] + square[3]) / 4.0 + getRandom(rangeRand))
				
				col = col + (currentStep * 2)

			row = row + (currentStep * 2)


		# square step
		rowOn = False

		for row in range(0, arraySize, currentStep):
			colOn = False

			for col in range(0, arraySize, currentStep):
				if colOn ^ rowOn:
					if rowOn:
						diamond = getDiamond(array, arraySize, currentStep, row, col)
						setIndex(array, arraySize, row, col, (diamond[0] + diamond[1] + diamond[2] + diamond[3]) / diamond[4] + getRandom(rangeRand))

					else:
						diamond = getDiamond(array, arraySize, currentStep, row, col)
						setIndex(array, arraySize, row, col, (diamond[0] + diamond[1] + diamond[2] + diamond[3]) / diamond[4] + getRandom(rangeRand))

				colOn = not colOn
			
			rowOn = not rowOn

		rangeRand = rangeRand * diminish
		currentStep = currentStep / 2

	return array



