#!/usr/bin/env python

from euler import *
import itertools

def products_of_three_digit_numbers():
    three_digit_pairs = itertools.combinations(numbers_with_digits(3),2)
    return itertools.imap(product, three_digit_pairs)

result = max(itertools.ifilter(is_palindromic, products_of_three_digit_numbers()))

print result
