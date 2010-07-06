#!/usr/bin/env python

def getNewPairs(depth,a,b):
	m = 2**depth

	if a == b:
		return [(a,b),(a,b + m),(a + m,b + m)]

	return [(a,b),(a,b + m),(a + m,b),(a + m,b + m)]

def lastDigits(number,depth):
	return number & (2**depth - 1)

def multiply(a,b):
	return a * b

def factor(number, pairs = [(0,0)], depth=0):
	test = lastDigits(number,depth)

	newpairs = []

	for pair in pairs:
		product = multiply(*pair)

		if product == number:
			return pair

		elif lastDigits(product,depth) == test:
			newpairs += getNewPairs(depth,*pair)

	return factor(number,newpairs,depth + 1)

if __name__ == '__main__':
	print factor(611951 * 611953)
