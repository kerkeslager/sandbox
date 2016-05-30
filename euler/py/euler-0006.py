#!/usr/bin/env python

from euler import *

def sum_of_squares(n):
    return sum(itertools.imap(square, count_to(n)))

def square_of_sum(n):
    return square(sum(count_to(n)))

result = square_of_sum(100) - sum_of_squares(100)

print result

