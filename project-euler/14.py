"""This code is the result of my attempt to solve Project Euler problem 14.  It
takes a positive integer from the command line and finds the positive integer
less than the given integer which starts the longest hailstone sequence.  For
more information on hailstone sequences, see
http://en.wikipedia.org/wiki/Collatz_conjecture .

While solving this problem I came across a novel usage of a few different
advanced Python features for a very generalizeable and reusable way to implement
memoization.  For more information on memoization, see
http://en.wikipedia.org/wiki/Memoization ."""

class Memoize:
	def __init__(self,function):
		self.function = function
		self.memo = {}

	def __call__(self,*arguments):
		if arguments in self.memo:
			return self.memo[arguments]

		result = self.function(*arguments)
		self.memo[arguments] = result
		return result

@Memoize
def hailstone(start):
	if start == 1:
		return 1

	if start % 2 == 0:
		return hailstone(start / 2) + 1

	return hailstone(start * 3 + 1) + 1

if __name__ == '__main__':
	import sys

	# Needed in order to calculate the hailstone number of large numbers.
	sys.setrecursionlimit(1000000)

	limit = int(sys.argv[1])
	max = 1
	max_start = 1

	for i in xrange(1,limit):
		chain = hailstone(i)

		if chain > max:
			max = chain
			max_start = i

	print max_start
