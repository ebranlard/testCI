module CompileVars
    integer, parameter :: ISTRING_LEN=64
    character(len=ISTRING_LEN), parameter :: VERSION = '${GIT_DESCRIBE}'
    character(len=ISTRING_LEN), parameter :: FORTRAN_COMPILER = '${CMAKE_Fortran_COMPILER_ID}'
    !character(len=ISTRING_LEN), parameter :: ARCHITECTURE = 'amd64'
!     integer, parameter :: RELEASE  = 1
!     integer, parameter :: OPENMP  = 1
!     integer, parameter :: MPI  = 0
!     integer, parameter :: CUDA  = 0
end module CompileVars
