# CMake project file for Omnivor
# NOTE:
# on windows: cmake -G"NMake Makefiles"
# MKLROOT "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2017.2.187\windows\mkl\ "
# --- Small cmake documentation:
# include(GetGitRevisionDescription) # to include a cmake file
# add_definitions(-DGIT_VERSION_INFO="${GIT_DESCRIBE}") # to add compile time definitions

cmake_minimum_required (VERSION 2.6)
project (Omnivor)

# Project requirements
enable_language (Fortran)

# --- Default options
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING "Build options: None Debug Release." FORCE)
endif (NOT CMAKE_BUILD_TYPE)

# Adding cmake packages
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_inc") 

# --- Git info into the executable
include(GetGitRevisionDescription)
git_describe(GIT_DESCRIBE)
# add_definitions(-DGIT_VERSION_INFO="${GIT_DESCRIBE}")
# SET(LAPACK_LIBRARIES "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2017.2.187\windows\mkl\lib\intel64_lib")
# set(BLA_VENDOR Intel10_64lp_seq)
# find_package(BLAS REQUIRED)
# find_package(LAPACK)
# find_package(MKL) 
if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  find_package(MKL)
endif()
if (MKL_FOUND)
  include_directories(${MKL_INCLUDE_DIRS})
  # --- OPTION 1
#   set(BLAS_LIBRARIES ${MKL_LIBRARIES})
  set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
#   set(CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "ifport;ifcore;imf;svml;m;ipgo;intlc;c;irc_s;dl;c")
  # --- OPTION 2
  message("----------------------------------------------------------------")
  message("MKL_FOUND")
  message("MKL_LIBRARIES      : " ${MKL_LIBRARIES})
  message("MKL_INCLUDE_DIRS   : " ${MKL_INCLUDE_DIRS})
  message("LAPACK_LIBRARIES   : " ${LAPACK_LIBRARIES}) #<<<< We'll link with static libraries
  #  set(CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} -Qmkl:sequential")
  #  set(CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "ifport;ifcore;imf;svml;m;ipgo;intlc;c;irc_s;dl;c")
  #  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /qoffload-ldopts=-mkl=sequential mkl_sequential.lib mkl_lapack95_lp64.lib ")
  #  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")
    if(WIN32)
        #        SET(CMAKE_EXE_LINKER_FLAGS    "${CMAKE_EXE_LINKER_FLAGS} mkl_core.lib mkl_intel_lp64.lib  mkl_lapack95_lp64.lib mkl_sequential.lib mkl_rt.lib")
    endif()
  message("----------------------------------------------------------------")
else()
  find_package(BLAS REQUIRED)
  find_package(LAPACK REQUIRED)
endif()
# SET(Fortran_MKL_LINK_FLAGS    "")
#SET(Fortran_MKL_EXE_FLAGS    "mkl_lapack95_lp64.lib mkl_rt.lib")
# SET(Fortran_MKL_COMPILE_FLAGS "/Qmkl:sequential")
# SET(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} ${Fortran_MKL_COMPILE_FLAGS}" )
#SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${Fortran_MKL_COMPILE_FLAGS}" )
# SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${Fortran_MKL_LINK_FLAGS} ")
#SET(CMAKE_EXE_LINKER_FLAGS    "${CMAKE_EXE_LINKER_FLAGS} ${Fortran_MKL_EXE_FLAGS} ")


# --- Main directories
get_filename_component (parent_prefix ".." ABSOLUTE)
# set(CMAKE_CURRENT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/current_binary_dir") 
# set(CMAKE_BINARY_DIR "${CMAKE_BINARY_DIR}/cmake_binary_dir") 
# set(PROJECT_BINARY_DIR "${PROJECT_BINARY_DIR}/project_binary_dir") 
# set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/fortran_modules") 





# --- Compiler dependent flags
if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -DEXTRA -Wno-attributes")
endif()




message("----------------------------------------------------------------")
message("GIT VERSION                       : " ${GIT_DESCRIBE})
message("CMAKE_PROJECT_NAME                : " ${CMAKE_PROJECT_NAME})
# message("CMAKE_Fortran_COMPILER           : " ${CMAKE_Fortran_COMPILER})
message("CMAKE_Fortran_COMPILER_ID MATCHES : " ${CMAKE_Fortran_COMPILER_ID})

message("CMAKE_Fortran_FLAGS               : " ${CMAKE_Fortran_FLAGS})
message("CMAKE_BUILD_TYPE                  : " ${CMAKE_BUILD_TYPE})
if (CMAKE_BUILD_TYPE STREQUAL "RELEASE")
    message("CMAKE_Fortran_FLAGS_RELEASE       : " ${CMAKE_Fortran_FLAGS_RELEASE})
else()
    message("CMAKE_Fortran_FLAGS_DEBUG         : " ${CMAKE_Fortran_FLAGS_DEBUG})
endif()

message("  ")
message("CMAKE_SHARED_LINKER_FLAGS             : " ${CMAKE_SHARED_LINKER_FLAGS})
message("CMAKE_EXE_LINKER_FLAGS                : " ${CMAKE_EXE_LINKER_FLAGS})
message("  ")

message("CMAKE_Fortran_MODULE_DIRECTORY    : " ${CMAKE_Fortran_MODULE_DIRECTORY})
message("EXECUTABLE_OUTPUT_PATH            : " ${EXECUTABLE_OUTPUT_PATH})
message("PROJECT_BINARY_DIR                : " ${PROJECT_BINARY_DIR})
message("CMAKE_BINARY_DIR                  : " ${CMAKE_BINARY_DIR})
message("CMAKE_SOURCE_DIR                  : " ${CMAKE_SOURCE_DIR})
message("CMAKE_CURRENT_SOURCE_DIR          : " ${CMAKE_CURRENT_SOURCE_DIR})
message("CMAKE_CURRENT_BINARY_DIR          : " ${CMAKE_CURRENT_BINARY_DIR})
message("CMAKE_INSTALL_PREFIX              : " ${CMAKE_INSTALL_PREFIX})
message("parent_prefix                     : " ${parent_prefix})
message("----------------------------------------------------------------")



# --- Version
configure_file(CompileVars_Template.f90 CompileVars.f90)

# --- Purelib
file(GLOB spurelib purelib/*.f90 )
set(spurelib ${spurelib} CompileVars.f90)
add_library(purelib SHARED ${spurelib})
add_library(purelib_static STATIC ${spurelib})

## --- Matlib
file(GLOB smatlib matlib/*.f90)
add_library(matlib_shared SHARED ${smatlib})
target_link_libraries(matlib_shared ${LAPACK_LIBRARIES})

add_library(matlib_static STATIC ${smatlib})

#

# --- Program test_lapack_min
add_executable(test_lapack_min  test_prog/test_lapack_min.f90 )
target_link_libraries(test_lapack_min ${LAPACK_LIBRARIES})


# --- Program test_lapack
add_executable(test_lapack  test_prog/test_lapack.f90)
target_link_libraries(test_lapack matlib_static ${LAPACK_LIBRARIES})

# --- Program test_matlib
add_executable(test_matlib  test_prog/test_matlib.f90)
target_link_libraries(test_matlib matlib_static ${LAPACK_LIBRARIES}) # NOTE: if adding shared library here, nmake will not create a .lib if there are no export statements in the shared library. Also only the exported symbols will be accessible

# set_target_properties(matlab_static PROPERTIES OUTPUT_NAME MyLib)

#install(TARGETS purelib EXPORT "${CMAKE_PROJECT_NAME}Libraries"
#   RUNTIME DESTINATION bin
#   LIBRARY DESTINATION lib
#   ARCHIVE DESTINATION lib)

# set (CMAKE_INSTALL_PREFIX ${default_prefix} CACHE STRING "Choose the installation directory; by default it installs in the  directory." FORCE)
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
