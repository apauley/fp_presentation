#! /usr/bin/env python

def doubleAll(numbers):
    return map(lambda x: x * 2, numbers)

if __name__ == '__main__':
    print doubleAll(range(1,500))
