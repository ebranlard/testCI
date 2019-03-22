from __future__ import print_function
from ctypes import *
import os
import re
import numpy as np


# --------------------------------------------------------------------------------}
# --- Useful tools to communicate with c library 
# --------------------------------------------------------------------------------{
TRUE=byref(c_double(1))
FALSE=byref(c_double(0))
def c_to_py(var_c, var=0):
    if isinstance(var,np.ndarray):
        if var.ndim ==1:
            n=var.shape[0]
            tmp=np.zeros((n))
            tmp[0:n]=var_c[0:n]
            var=tmp
        elif var.ndim==2:
            (n,m)=var.shape
            tmp=np.zeros((n*m,))
            tmp[0:(n*m)]=var_c[0:(n*m)]
            var=tmp.reshape(n,m,order='F')
        elif var.ndim==3:
            (n,m,p)=var.shape
            tmp=np.zeros((n*m*p,))
            tmp[0:(n*m*p)]=var_c[0:(n*m*p)]
            var=tmp.reshape(n,m,p,order='F')
        elif var.ndim==4:
            (n,m,p,q)=var.shape
            tmp=np.zeros((n*m*p*q,))
            tmp[0:(n*m*p*q)]=var_c[0:(n*m*p*q)]
            var=tmp.reshape(n,m,p,q,order='F')
        else:
            print('Error numpy array of dim5')
            var=-1
    elif isinstance(var,int):
        var=int(var_c.value)
    else:
        print('Error TODO'+str(type(var)))
        pass

    return var

def real_to_py(var_c, n,m=-1):
    import numpy as np
    if m==-1:
        var=np.zeros((n))
        return c_to_py(var_c,var)
    else:
        var=np.zeros((n,m))
        return c_to_py(var_c,var)

def to_cp(var):
    return byref(to_c(var))
def to_c_intp(var):
    return byref(to_c_int(var))
def to_c_boolp(var):
    return byref(to_c_bool(var))


def zeros_c(n,m):
    import numpy as np
    var_c=(c_double*(n*m))(0)
    return(var_c)


def to_c(var):
    import numpy as np
    if isinstance(var,np.ndarray):
        if var.ndim ==1:
            n=var.shape[0]
            var_c=(c_double*n)(0)
            var_c[0:n]=var[0:n]
        elif var.ndim==2:
            (n,m)=var.shape
            var_c=(c_double*(n*m))(0)
            var_c[0:(n*m)]=var.flatten(1)[0:(n*m)]
        elif var.ndim==3:
            (n,m,p)=var.shape
            var_c=(c_double*(n*m*p))(0)
            var_c[0:(n*m*p)]=var.flatten(1)[0:(n*m*p)]
        elif var.ndim==4:
            (n,m,p,q)=var.shape
            var_c=(c_double*(n*m*p*q))(0)
            var_c[0:(n*m*p*q)]=var.flatten(1)[0:(n*m*p*1)]
        else:
            print('ERROR numpy of dim5')
            var_c=c_int(2)
    elif isinstance(var,list):
        n=len(var)
        var_c=(c_double*n)(0)
        var_c[0:n]=var[0:n]
    else:
        #print('type is:',type(var))
        var_c=c_double(var)
    return var_c

def to_c_int(var):
    try:
        n=len(var)
        var_c=(c_int*n)(0)
        var_c[0:n]=var[0:n]
    except TypeError:
        var_c=c_int(var)
    return var_c

def to_c_bool(var):
    if isinstance(var,bool):
        if var:
            var_c=c_int(1)
        else:
            var_c=c_int(0)
    else:
        if var==1:
            var_c=c_int(1)
        else:
            var_c=c_int(0)
    return var_c



class WrapLib:
    raw=[]
    libpath=''
    bLibLoaded=False
    bLibInit=False

    def __init__(self,libpath=None):
        self.bDEBUG=0;
        self.libpath =libpath;
        if libpath is not None:
            self.load()

    def __del__(self):
        self.close()

    def load(self):
        """ Load"""
        # Loading library
        if ispc():
            self.raw = cdll.LoadLibrary(self.libpath)
            #         libHandle = ctypes.windll.kernel32.LoadLibraryA('mydll.dll')
            #         lib = ctypes.WinDLL(None, handle=libHandle)
            #         # Alternative:
            #         lib = ctypes.cll(file)
            #         libHandle = lib._handle

        else: #unix
            self.raw = cdll.LoadLibrary(self.libfile)
            self.bLibLoaded=true;


    def close(self):
        """ Close library"""
        if self.bLibLoaded:
            # Then attempting to unload the library
            self.bLibLoaded=false;
            del self.raw
            self.raw=[]
    # --------------------------------------------------------------------------------
    # --- Library call
    # --------------------------------------------------------------------------------
    def call(self,func_name,*args):
        func_call='self.raw.%s'%func_name
        if self.bDEBUG==1:
            print(func_call)
        return eval(func_call)(*args)


if __name__ == "__main__":
    import sys
    if len(sys.argv)>1:
        print('Called with: ', int(sys.argv[1]))
        WrapLib(sys.argv[1])


#         nprof=self.call('ip_get',to_c_intp(idb));
#         if(iprof>nprof):
#             n=-1
#             return(false,0,0,0,0)
#         
#         n=self.call('ip_get',to_c_intp(idb),to_c_intp(iprof),to_c_intp(igeom),to_c_intp(itype));
#         if n==-1:
#             return(false,0,0,0,0)
#         
#         x    = zeros(1,n) ;
#         y    = zeros(1,n) ;
#         # intent inout variables
#         x_c    = to_c(x   ) ;
#         y_c    = to_c(y   ) ;
#         t_rel_c = to_c(0.0)
#         s_rel_c = to_c(0.0)
#         self.call('ip_prof',to_c_intp(idb),to_c_intp(iprof),to_c_intp(igeom),to_c_intp(itype),to_c_intp(n),byref(x_c),byref(y_c),byref(t_rel_c),byref(s_rel_c))
#         x    = c_to_py(x_c,x) ;
#         y    = c_to_py(y_c,y) ;
# 

    else:
        print('Provide one argument')
    pass
#     import sys
