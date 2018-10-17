import platform
import os

def which(program):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            exe_file = os.path.join(path, program)
            print(exe_file)
            if is_exe(exe_file):
                return exe_file

    return None


if platform.system()=='Linux':
    if which('pip') is not None:
        os.system('pip install -r requirements.txt')
    else:
        raise Exception('pip command not found')
elif platform.system()=='Windows':
    os.system('pipi install -r requirements.txt')
    os.system('pip install -r requirements.txt')
#     if which('pip') is not None:
#     else:
#         raise Exception('pip command not found')
elif platform.system()=='Darwin':
    os.system('pip install -r requirements.txt')

