from random import uniform
from PIL import Image
import numpy
import sys


# x is ratio
def lerp(a, b, x):
	return (a * (1 - x)) + (b * x)


# x and y is ratio
def bilinear(x, y, tl, tr, bl, br):
	sX = x * x * (3 - (2 * x))
	sY = y * y * (3 - (2 * y))

	x1 = lerp(tl, tr, sX)
	x2 = lerp(bl, br, sX)

	return lerp(x1, x2, sY)


def translate(size, x, y):
	return y * size + x


def interpolate(period, amplitude):
	imageSize = 512
	gridSize = 512 / period

	grid = [0] * ((gridSize + 1) * (gridSize + 1))
	for y in range(0, gridSize):
		for x in range(0, gridSize):
			grid[translate(gridSize + 1, x, y)] = uniform(0.0, 255.0) * amplitude



	imgArray = [0] * (imageSize * imageSize)
	for y in range(0, imageSize):
		for x in range(0, imageSize):
			gridX = float(x) / imageSize * gridSize
			gridY = float(y) / imageSize * gridSize


			intX = int(gridX)
			intY = int(gridY)

			remX = gridX - intX
			remY = gridY - intY

			tl = grid[translate(gridSize + 1, intX, intY)]
			tr = grid[translate(gridSize + 1, intX + 1, intY)]
			bl = grid[translate(gridSize + 1, intX, intY + 1)]
			br = grid[translate(gridSize + 1, intX + 1, intY + 1)]

			imgArray[translate(imageSize, x, y)] = bilinear(remX, remY, tl, tr, bl, br)


	return imgArray


def test(t, a):
	imgArr = interpolate(t, a)

	ar = numpy.array(imgArr)
	ar = numpy.reshape(ar, (512, 512))

	img =Image.fromarray(ar)
	img.show()



	
			
