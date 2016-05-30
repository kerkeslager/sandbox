#!/usr/bin/env python

from euler import *

def sum_is_one_thousand(xs):
    return sum(xs) == 1000

result = product(first(pythagorean_triplets(), sum_is_one_thousand))

print result
