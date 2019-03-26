#
cmake_minimum_required (VERSION 2.6)
enable_language (Fortran)

# Adding cmake packages
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_inc") 

if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  find_package(MKL)
endif()
if (MKL_FOUND)
    set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
endif()

## --- Matlib
file(GLOB smatlib matlib/*.f90)
add_library(matlib SHARED ${smatlib})
target_link_libraries(matlib ${LAPACK_LIBRARIES})

