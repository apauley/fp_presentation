#!/usr/bin/env python

import sys
from datetime import datetime

def main():
    time = datetime.now()
    args = sys.argv[1:]
    print outputString(time, args)

def outputString(time, args):
    return str(time) + " " + joinArgs(args)

def joinArgs(args):
    return "-".join(args)

if __name__ == '__main__':
    main()
