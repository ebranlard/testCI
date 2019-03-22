# CMake project file for Omnivor
# NOTE:
# on windows: cmake -G"NMake Makefiles"
cmake_minimum_required (VERSION 2.6)
project (Omnivor)

# Project requirements
enable_language (Fortran)
find_package(LAPACK)


# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING "Build options: None Debug Release." FORCE)
endif (NOT CMAKE_BUILD_TYPE)



file(GLOB sources matlib/*.f90)
set(CMAKE_CURRENT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/current_binary_dir") 
set(CMAKE_BINARY_DIR "${CMAKE_BINARY_DIR}/cmake_binary_dir") 
set(PROJECT_BINARY_DIR "${PROJECT_BINARY_DIR}/project_binary_dir") 



message("----------------------------------------------------------------")
# message("CMAKE_Fortran_COMPILER            : " ${CMAKE_Fortran_COMPILER})
message("CMAKE_Fortran_COMPILER_ID MATCHES : " ${CMAKE_Fortran_COMPILER_ID})

message("CMAKE_Fortran_FLAGS               : " ${CMAKE_Fortran_FLAGS})
if (CMAKE_BUILD_TYPE STREQUAL "RELEASE")
    message("CMAKE_Fortran_FLAGS_RELEASE       : " ${CMAKE_Fortran_FLAGS_RELEASE})
else()
    message("CMAKE_Fortran_FLAGS_DEBUG         : " ${CMAKE_Fortran_FLAGS_DEBUG})
endif()
message("EXECUTABLE_OUTPUT_PATH                : " ${EXECUTABLE_OUTPUT_PATH})
message("CMAKE_BINARY_DIR                      : " ${CMAKE_BINARY_DIR})
message("CMAKE_CURRENT_BINARY_DIR              : " ${CMAKE_CURRENT_BINARY_DIR})
message("PROJECT_BINARY_DIR                    : " ${PROJECT_BINARY_DIR})

message("Souces" ${sources}) 
message("----------------------------------------------------------------")

# add_executable(matlib ${sources})
# target_link_libraries(matlib ${CMAKE_DL_LIBS})
add_library(matlib SHARED ${sources})
add_library(matlib_static STATIC ${sources})
# set_target_properties(matlab_static PROPERTIES OUTPUT_NAME MyLib)

# install(TARGETS matlib EXPORT "${CMAKE_PROJECT_NAME}Libraries"
#   RUNTIME DESTINATION bin
#   LIBRARY DESTINATION lib
#   ARCHIVE DESTINATION lib)

# make sure that the default is a RELEASE
# if (NOT CMAKE_BUILD_TYPE)
#   set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
#       "Choose the type of build, options are: None Debug Release."
#       FORCE)
# endif (NOT CMAKE_BUILD_TYPE)
# 
# # default installation
# get_filename_component (default_prefix ".." ABSOLUTE)
# set (CMAKE_INSTALL_PREFIX ${default_prefix} CACHE STRING
#       "Choose the installation directory; by default it installs in the NORMA directory."
#       FORCE)
# 
# # FFLAGS depend on the compiler
# get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
# 
# if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
#   # gfortran
#   set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
#   set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
# elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
#   # ifort (untested)
#   set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
#   set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
# elseif (Fortran_COMPILER_NAME MATCHES "g77")
#   # g77
#   set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
#   set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
# else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
#   message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
#   message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
#   message ("No optimized Fortran compiler flags are known, we just try -O2...")
#   set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
#   set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
# endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")
# 
# 
# # build executables
# set (NMPROGRAMS "diagstd" "diagrtb" "proj_modes_bin" "pdbmat")
# set (EXECUTABLES "NORMA.exe" ${NMPROGRAMS})
# set (SCRIPTS "gen_pert.sh" "pert_multi_mode.sh")
# 
# add_executable ("NORMA.exe" "NORMA.f")
# foreach (p ${NMPROGRAMS})
#   add_executable (${p} "${p}.f")
# endforeach (p)
# 
# # install executables and scripts
# install (TARGETS ${EXECUTABLES}
#          RUNTIME DESTINATION "bin")
# install (PROGRAMS ${SCRIPTS}
#          DESTINATION "bin")
