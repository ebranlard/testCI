from __future__ import print_function
from ctypes import *
import sys
# import os
# import platform

if __name__ == "__main__":
    if len(sys.argv)>1:
        libname=sys.argv[1]
        print('Loading ',libname)
        lib=cdll.LoadLibrary(libname)
        print('[ OK ] ')
    else:
        print('Provide one argument')
