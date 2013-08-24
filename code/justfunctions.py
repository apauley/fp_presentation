#!/usr/bin/env python

from sys import argv

def main(args):
    result = do_something_with_args(args)
    print result

def do_something_with_args(args):
    return '-'.join(args[1:])

if __name__ == '__main__':
    main(argv)
