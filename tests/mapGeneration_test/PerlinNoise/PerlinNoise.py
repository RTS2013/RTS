import random
import numpy
from PIL import Image


FREQ 	= 1/256.0
AMPL 	= 0.05
LAYERS 	= 4


def lerp(a, b, x):
	return (a * (1 - x) + (b * x))



def smoothCurve(x):
	#return x * x * (3 - (2 * x))
	return (6 * pow(x, 5)) - (15 * pow(x, 4)) + (10 * pow(x, 3))


def bilinear(x, y, tf, tr, bl, br):
	sX = smoothCurve(x)
	sY = smoothCurve(y)

	v1 = lerp(tf, tr, sX)
	v2 = lerp(bl, br, sX)

	return lerp(v1, v2, sY)



def rand(x, y, f, seed):
	random.seed((x, y, f, seed))
	return random.uniform(0.0, 1.0)



def noise(x, y, seed):
	f = FREQ
	a = AMPL
	val = 0.0

	for l in range(0, LAYERS):
		layerX = float(x) * f		# convert to layer coordinate
		layerY = float(y) * f		# convert to layer coordinate

		intX = int(layerX)
		intY = int(layerY)

		remX = layerX - intX
		remY = layerY - intY

		tl = rand(intX, intY, f, seed) 
		tr = rand(intX + 1, intY, f, seed)
		bl = rand(intX, intY + 1, f, seed)
		br = rand(intX + 1, intY + 1, f, seed)

		val = val + (bilinear(remX, remY, tl, tr, bl, br) * a)

		f = f * 2.0
		a = a / 2.0 

	return val


def test(size, seed):
	grid = [0] * (size * size)
	maxV = 0.0
	minV = 1000000.0

	for y in range(0, size):
		for x in range(0, size):
			coor = y * size + x
			grid[coor] = noise(x, y, seed)
			if grid[coor] > maxV:
				maxV = grid[coor]
			if grid[coor] < minV:
				minV = grid[coor]

	rangeV = maxV - minV
	for y in range(0, size):
		for x in range(0, size):
			coor = y * size + x
			grid[coor] = ((grid[coor] - minV) / rangeV) * 255.0



	ar = numpy.array(grid)
	ar = numpy.reshape(ar, (size, size))

	img = Image.fromarray(ar)
	img.show()
