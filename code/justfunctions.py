#!/usr/bin/env python

def main(args):
    result = do_something_with_args(args)
    return result

def do_something_with_args(args):
    return ' '.join(args[1:])

if __name__ == '__main__':
    import sys
    print main(sys.argv)
