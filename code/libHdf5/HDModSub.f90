
SUBMODULE( HDMod ) HDModSub
CONTAINS

!--------|---------|---------|---------|---------|---------|---------|---------|---------|---------|--READ---|---INT---|---32----|-|

    MODULE PROCEDURE HD_read1Int32i
        CALL self%HDB_showLine( __LINE__, 'HD_read1Int32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read1Int32
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        INTEGER( HSIZE_T ) :: rowNumber
        self%dType_id = H5T_NATIVE_INTEGER
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read1Int32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_read2Int32i
        CALL self%HDB_showLine( __LINE__, 'HD_read2Int32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read2Int32
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        INTEGER( HSIZE_T ) :: rowNumber
        self%dType_id = H5T_NATIVE_INTEGER
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read2Int32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_read3Int32i
        CALL self%HDB_showLine( __LINE__, 'HD_read3Int32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read3Int32
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        INTEGER( HSIZE_T ) :: rowNumber
        self%dType_id = H5T_NATIVE_INTEGER
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read3Int32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

!--------|---------|---------|---------|---------|---------|---------|---------|---------|---------|--WRITE--|---INT---|---32----|-|

    MODULE PROCEDURE HD_write1Int32i
        CALL self%HDB_showLine( __LINE__, 'HD_write1Int32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write1Int32
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_INTEGER
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write1Int32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_write2Int32i
        CALL self%HDB_showLine( __LINE__, 'HD_write2Int32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write2Int32
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_INTEGER
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write2Int32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_write3Int32i
        CALL self%HDB_showLine( __LINE__, 'HD_write3Int32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write3Int32
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_INTEGER
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write3Int32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

!--------|---------|---------|---------|---------|---------|---------|---------|---------|---------|--READ---|---INT---|---64----|-|

    MODULE PROCEDURE HD_read1Int64i
        CALL self%HDB_showLine( __LINE__, 'HD_read1Int64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read1Int64
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        INTEGER( HSIZE_T ) :: rowNumber
        self%dType_id = H5T_STD_I64LE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read1Int64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_read2Int64i
        CALL self%HDB_showLine( __LINE__, 'HD_read2Int64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read2Int64
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        INTEGER( HSIZE_T ) :: rowNumber
        self%dType_id = H5T_STD_I64LE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read2Int64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_read3Int64i
        CALL self%HDB_showLine( __LINE__, 'HD_read3Int64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read3Int64
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        INTEGER( HSIZE_T ) :: rowNumber
        self%dType_id = H5T_STD_I64LE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read3Int64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

!--------|---------|---------|---------|---------|---------|---------|---------|---------|---------|--WRITE--|---INT---|---64----|-|

    MODULE PROCEDURE HD_write1Int64i
        CALL self%HDB_showLine( __LINE__, 'HD_write1Int64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write1Int64
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_STD_I64LE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write1Int64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_write2Int64i
        CALL self%HDB_showLine( __LINE__, 'HD_write2Int64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write2Int64
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_STD_I64LE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write2Int64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_write3Int64i
        CALL self%HDB_showLine( __LINE__, 'HD_write3Int64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write3Int64
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_STD_I64LE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write3Int64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

!--------|---------|---------|---------|---------|---------|---------|---------|---------|---------|--READ---|---REAL--|---32----|-|

    MODULE PROCEDURE HD_read1Real32i
        CALL self%HDB_showLine( __LINE__, 'HD_read1Real32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read1Real32
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_REAL
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read1Real32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_read2Real32i
        CALL self%HDB_showLine( __LINE__, 'HD_read2Real32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read2Real32
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_REAL
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read2Real32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_read3Real32i
        CALL self%HDB_showLine( __LINE__, 'HD_read3Real32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read3Real32
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_REAL
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read3Real32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

!--------|---------|---------|---------|---------|---------|---------|---------|---------|---------|--WRITE--|---REAL--|---32----|-|

    MODULE PROCEDURE HD_write1Real32i
        CALL self%HDB_showLine( __LINE__, 'HD_write1Real32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write1Real32
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_REAL
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write1Real32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_write2Real32i
        CALL self%HDB_showLine( __LINE__, 'HD_write2Real32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write2Real32
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_REAL
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write2Real32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_write3Real32i
        CALL self%HDB_showLine( __LINE__, 'HD_write3Real32i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write3Real32
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_REAL
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write3Real32', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

!--------|---------|---------|---------|---------|---------|---------|---------|---------|---------|--READ---|---REAL--|---64----|-|

    MODULE PROCEDURE HD_read1Real64i
        CALL self%HDB_showLine( __LINE__, 'HD_read1Real64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read1Real64
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_DOUBLE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read1Real64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_read2Real64i
        CALL self%HDB_showLine( __LINE__, 'HD_read2Real64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read2Real64
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_DOUBLE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_read2Real64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_read3Real64i
        CALL self%HDB_showLine( __LINE__, 'HD_read3Real64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%read( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_read3Real64
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_DOUBLE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL self%HDB_showLine( __LINE__, 'HD_read3Real64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL H5Dread_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

!--------|---------|---------|---------|---------|---------|---------|---------|---------|---------|--WRITE--|---REAL--|---64----|-|

    MODULE PROCEDURE HD_write1Real64i
        CALL self%HDB_showLine( __LINE__, 'HD_write1Real64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write1Real64
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_DOUBLE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write1Real64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_write2Real64i
        CALL self%HDB_showLine( __LINE__, 'HD_write2Real64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=HSIZE_T ) )
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write2Real64
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_DOUBLE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write2Real64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE

    MODULE PROCEDURE HD_write3Real64i
        CALL self%HDB_showLine( __LINE__, 'HD_write3Real64i', SM_DEBUG, 'lineNumber=', int( lineNumber, kind=real64 ))
        CALL self%write( dSetName, buf, int( lineNumber, kind=HSIZE_T ))
    END PROCEDURE
    MODULE PROCEDURE HD_write3Real64
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: mhsCount
        INTEGER :: fileRank
        self%dType_id = H5T_NATIVE_DOUBLE
        rowNumber = self%HDB_getOpenLength( dSetName, shape( buf ))
        rowNumber = self%HDB_getExtendRow( rowNumber, lineNumber )
        CALL self%HDB_showLine( __LINE__, 'HD_write3Real64', SM_DEBUG, 'rowNumber', rowNumber )
        CALL self%HDB_loadHyperslabs( rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL self%HDB_allocDSetDims( mhsCount )
        CALL H5Dwrite_f( self%dSet_id, self%dType_id, buf, mhsCount(2:fileRank), self%iError, self%mSpace_id, self%dSpace_id )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( mhsCount )) DEALLOCATE( mhsCount )
    END PROCEDURE
END SUBMODULE
