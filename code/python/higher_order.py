#! /usr/bin/env python

def f(x):
    return (2 * x**2) - (2 * x) + 3

def g(x):
    return f(x) + 1

def h(f1, f2):
    return f1(3) + f2(2)

if __name__ == '__main__':
    print "f(3) =", f(3)
    print "g(2) =", g(2)
    print "h(f, g) =", h(f, g)
