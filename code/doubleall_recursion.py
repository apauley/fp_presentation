#! /usr/bin/env python

def doubleAll(numbers):
    if numbers == []:
        return []
    else:
        first = numbers[0]
        rest = numbers[1:]
        return [first * 2] + doubleAll(rest)
