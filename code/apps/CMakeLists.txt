MESSAGE( STATUS "    --> in 'apps'" )

ADD_EXECUTABLE( TestMpiIocHdf5 TestMpiIocHdf5.f90 MsgTest.f90 )
TARGET_LINK_LIBRARIES( TestMpiIocHdf5 LibHdf5Static LibMpiStatic ${HDF5_Fortran_LIBRARIES} )

#ADD_EXECUTABLE( arts2017 arts2017.f90 )
#TARGET_LINK_LIBRARIES( arts2017 FaultsStatic )
