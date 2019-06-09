# clear&&echo&&rm -rf CMakeCache.txt CMakeFiles Makefile&&echo&&cmake .&&echo&&make&&echo&&mpiexec -n 3 bin/CMakeForPlus&&echo
# h5cc -showconfig
# dpkg -l | grep hdf5
# echo ${HDF5_ROOT}

CMAKE_MINIMUM_REQUIRED( VERSION 3.10 )
PROJECT( test C Fortran )

ENABLE_LANGUAGE( Fortran )

SET( CMAKE_VERBOSE_MAKEFILE "On" )

SET( CMAKE_Fortran_COMPILER mpif90 )
SET( CMAKE_C_COMPILER mpicc )

SET( HDF5_USE_STATIC_LIBRARIES "ON" )
SET( HDF5_FIND_DEBUG "ON" )

FIND_PACKAGE( MPI REQUIRED )
FIND_PACKAGE( OpenMP REQUIRED )
FIND_PACKAGE( HDF5 REQUIRED COMPONENTS Fortran Fortran_HL )

STRING( TIMESTAMP CONFIG_DATE "%Y-%m-%d" )
MESSAGE( STATUS "    --> CONFIG_DATE [${CONFIG_DATE}]" )

MESSAGE( STATUS "    -->  [${}]" )
MESSAGE( STATUS "    -->  [${}]" )
MESSAGE( STATUS "    --> HDF5_FOUND [${HDF5_FOUND}]" )
MESSAGE( STATUS "    --> HDF5_IS_PARALLEL [${HDF5_IS_PARALLEL}]" )
MESSAGE( STATUS "    --> HDF5_VERSION [${HDF5_VERSION}]" )
MESSAGE( STATUS "    -->  [${}]" )
MESSAGE( STATUS "    --> HDF5_INCLUDE_DIRS [${HDF5_INCLUDE_DIRS}]" )
MESSAGE( STATUS "    --> HDF5_C_INCLUDE_DIRS [${HDF5_C_INCLUDE_DIRS}]" )
MESSAGE( STATUS "    --> HDF5_Fortran_INCLUDE_DIRS [${HDF5_Fortran_INCLUDE_DIRS}]" )
MESSAGE( STATUS "    -->  [${}]" )
MESSAGE( STATUS "    --> HDF5_DEFINITIONS [${HDF5_DEFINITIONS}]" )
MESSAGE( STATUS "    --> HDF5_C_DEFINITIONS [${HDF5_C_DEFINITIONS}]" )
MESSAGE( STATUS "    --> HDF5_Fortran_DEFINITIONS [${HDF5_Fortran_DEFINITIONS}]" )
MESSAGE( STATUS "    -->  [${}]" )
MESSAGE( STATUS "    --> HDF5_LIBRARIES [${HDF5_LIBRARIES}]" )
MESSAGE( STATUS "    --> HDF5_HL_LIBRARIES [${HDF5_HL_LIBRARIES}]" )
MESSAGE( STATUS "    --> HDF5_C_LIBRARIES [${HDF5_C_LIBRARIES}]" )
MESSAGE( STATUS "    --> HDF5_Fortran_LIBRARIES [${HDF5_Fortran_LIBRARIES}]" )
MESSAGE( STATUS "    --> HDF5_C_HL_LIBRARIES [${HDF5_C_HL_LIBRARIES}]" )
MESSAGE( STATUS "    --> HDF5_Fortran_HL_LIBRARIES [${HDF5_Fortran_HL_LIBRARIES}]" )
MESSAGE( STATUS "    --> HDF5_C_COMPILER_EXECUTABLE_NO_INTERROGATE [${HDF5_C_COMPILER_EXECUTABLE_NO_INTERROGATE}]" )
MESSAGE( STATUS "    --> HDF5_Fortran_COMPILER_EXECUTABLE_NO_INTERROGATE [${HDF5_Fortran_COMPILER_EXECUTABLE_NO_INTERROGATE}]" )
MESSAGE( STATUS "    -->  [${}]" )
MESSAGE( STATUS "    -->  [${}]" )

SET( CMAKE_BUILD_TYPE Debug )
SET( CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -cpp -std=f2008 -m64 -fcheck=all -Wall -fmax-errors=5" )
SET( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
SET( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
SET( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
SET( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod )

INCLUDE_DIRECTORIES( ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS} )

ADD_SUBDIRECTORY( code )



# me: extremely high intelligence
#     +moderately low agreeableness (don't care about offending)
#     +moderately low consciousness (think outside the box, break the rules)
#     +highly sexually repressed
#     +K strategist (r/K K=nurturing )



#CMAKE_FORCE_Fortran_COMPILER( mpif90 ${CMAKE_Fortran_COMPILER_ID} )
#CMAKE_FORCE_C_COMPILER( mpicc ${CMAKE_C_COMPILER_ID} )
#MESSAGE( STATUS "    --> : ${}" )
#GET_FILENAME_COMPONENT( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )
#MESSAGE( STATUS "    --> :" ${} )
#
#INCLUDE( CMakeForceCompiler )
#
#SET( PACKAGE_VERSION 1.0 )
#
#
#SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fopenmp )
#
#find_package(MPI REQUIRED)
#
#ADD_DEFINITIONS( ${MPI_Fortran_COMPILE_FLAGS} )
#ADD_DEFINITIONS( -DOMPI_SKIP_MPICXX ) # something to do with c++ bindings for OpenMPI
#
#INCLUDE_DIRECTORIES( SYSTEM ${MPI_INCLUDE_PATH} )
#INCLUDE_DIRECTORIES( ${MPI_Fortran_INCLUDE_PATH} )
#
#ADD_EXECUTABLE( main main.cpp )
#
#LINK_DIRECTORIES( ${MPI_Fortran_LIBRARIES} )
#TARGET_LINK_LIBRARIES( ${exe_name} otherlibs ${MPI_Fortran_LIBRARIES} )
#TARGET_LINK_LIBRARIES( my_mpi_target ${MPI_C_LIBRARIES} )
#TARGET_LINK_LIBRARIES( target gomp )
#TARGET_LINK_LIBRARIES( main PRIVATE MPI::MPI_C )
#
#
## Old way.
##target_link_libraries(main
## PRIVATE
## ${MPI_C_LIBRARIES})
#
##target_include_directories(main
## PRIVATE
## ${MPI_C_INCLUDE_PATH}) 
#
#
#