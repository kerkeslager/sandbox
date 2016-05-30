#!/usr/bin/env python

from euler import *

def divisible_by_three_or_five(dividend):
    return divisible_by(dividend, 3) or divisible_by(dividend, 5)

result = sum([n for n in range(1000) if divisible_by_three_or_five(n)])

print result

