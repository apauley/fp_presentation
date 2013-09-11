#!/usr/bin/env python

from sys import argv
from datetime import datetime

def main(args):
    timestamp = datetime.now()
    result = str(timestamp) + " " + join_args(args)
    print result

def join_args(args):
    return '-'.join(args[1:])

if __name__ == '__main__':
    main(argv)
