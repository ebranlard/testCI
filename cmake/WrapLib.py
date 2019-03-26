from __future__ import print_function
from ctypes import *
import os
import re
import numpy as np
import platform

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

    def __init__(self,libpath=None):
        self.bDEBUG=0;
        self.libpath =libpath;
        self.raw=None
        if libpath is not None:
            self.load()

    def __del__(self):
        self.close()

    def load(self):
        """ Load"""
        self.raw=cdll.LoadLibrary(self.libpath)


    def close(self):
        """ Close library"""
        if self.raw is not None:
            del self.raw
            self.raw=None
    # --------------------------------------------------------------------------------
    # --- Library call
    # --------------------------------------------------------------------------------
    def call(self,func_name,*args):
        func_call='self.raw.%s'%func_name
        if self.bDEBUG==1:
            print(func_call)
        return eval(func_call)(*args)

    def call_db(self,func_name,*args):
        func_call='self.raw.%s'%func_name
        settype='self.raw.{}.restype=c_double'.format(func_name)
        exec(settype)
        if self.bDEBUG==1:
            print(func_call)
        return eval(func_call)(*args)


# --------------------------------------------------------------------------------}
# ---  
# --------------------------------------------------------------------------------{
def test_lib(libname):
        print('Loading  : ', libname)
        lib=WrapLib(libname)
        if libname.find('matlib')>=0:
            print('Skipping matlib')
            return

        # Simple call
        lib.call('purelib_print_version')

        # Getting an int
        myint = lib.call('purelib_getint')
        print('>>GetInt  :',myint)

        # Getting a double
        mydb  = lib.call_db('purelib_getdb')
        print('>>GetDB   :',mydb)
        
        # Getting a string, option 1:
        version=create_string_buffer(30)
        lib.call('purelib_get_version',version)
        print('>> Version:',version.value)

        # Getting a string, option 2:
        version=create_string_buffer(b' '*30)
        lib.call('purelib_get_version',version)
        print('>> Version:',version.value)

        # Passing a string
        inputfile=create_string_buffer(b'Inputfile.inp')
        res=lib.call('purelib_init',inputfile)
        print('>>InitCall:',res==1)

        # Passing doubles, getting a double
        add = lib.call_db('purelib_add', byref(to_c(12.0)),byref(to_c(100.0))) 
        print('>>Result  :',add)

        # Passing arrays in and out
        n=4
        x    = np.zeros(n) ;
        y    = np.zeros(n) ;
        z    = np.zeros(n) ;
        x=x+12
        y=y+100
        x_c    = to_c(x) ;
        y_c    = to_c(y) ;
        z_c    = to_c(z) ;
        lib.call('purelib_addvec',byref(x_c),byref(y_c),byref(z_c),to_c_intp(n))
        x    = c_to_py(x_c,x) ;
        y    = c_to_py(y_c,y) ;
        z    = c_to_py(z_c,z) ;
        print('>>AddVec  :',z)


if __name__ == "__main__":
    import sys
    from ctypes import *
    #lib = cdll.LoadLibrary(sys.argv[1])
    #A=lib.purelib_getdb()
    #print(A)
    #print(type(A))
    if len(sys.argv)>1:
        libname=sys.argv[1]
        test_lib(libname)

 
    else:
        print('Provide one argument')
#     import sys
