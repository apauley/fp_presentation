#! /usr/bin/env python

def f(x):
    return (2*x) + 3

def g(x):
    return f(x) + 1

def h(func_a, func_b, x):
    return func_a(x) + func_b(2)

if __name__ == '__main__':
    print "f(7) =", f(7)
    print "g(2) =", g(2)
    print "h(f, g, 7) =", h(f, g, 7)
