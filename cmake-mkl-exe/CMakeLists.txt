# CMAKE EXAMPLE FILE TO COMPILE AN EXE AND STATICALLY LINK THE MKL LIBRARIES

# NOTE: it's best to use option 2 since then it's unified between dlls and exe 

cmake_minimum_required(VERSION 2.9)
enable_language (Fortran)

# --- OPTION 1 : set exe linker flags directly
if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    if(WIN32)
        SET(Fortran_MKL_EXE_FLAGS   "mkl_core.lib mkl_intel_lp64.lib  mkl_lapack95_lp64.lib mkl_sequential.lib mkl_rt.lib")
        SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${Fortran_MKL_EXE_FLAGS} ")
    endif()
endif()

# --- Program test_lapack_min
add_executable(test_lapack_min test_lapack_min.f90 )



# --- OPTION 2 : use MKL libraries
## Adding cmake packages
#set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_inc") 
#
#if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
#  find_package(MKL)
#endif()
#if (MKL_FOUND)
#    set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
#endif()
#
### --- Matlib
#file(GLOB smatlib matlib/*.f90)
#add_library(matlib SHARED ${smatlib})
#target_link_libraries(matlib ${LAPACK_LIBRARIES})
#
