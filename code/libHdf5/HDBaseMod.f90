! Written by Rand Huso

MODULE HDBaseMod
    USE, INTRINSIC :: iso_fortran_env ! , ONLY : output_unit, error_unit, int64, int32, real64, real32
    USE :: HDF5
    USE :: SimpleMessagesMod
    USE :: LMathMod
    IMPLICIT NONE

    TYPE, ABSTRACT :: HDBaseType ! PRIVATE - PROTECTED would be nice, if we were there
        CHARACTER( len=: ), ALLOCATABLE :: filename
        INTEGER :: iError
        INTEGER( HID_T ) :: file_id = 0
        INTEGER( HID_T ) :: group_id = 0
        INTEGER( HID_T ) :: dSet_id = 0
        INTEGER( HID_T ) :: dSpace_id = 0
        INTEGER( HID_T ) :: mSpace_id = 0
        INTEGER( HID_T ) :: prp_id = 0
        INTEGER( HID_T ) :: dType_id = 0
        INTEGER :: myRank = -1
    CONTAINS
        PROCEDURE :: HDB_Open
        PROCEDURE :: HDB_Close
        PROCEDURE :: HDB_errorCheck
        PROCEDURE :: HDB_fOpen
        PROCEDURE :: HDB_showLine
        PROCEDURE :: HDB_fClose
        PROCEDURE :: HDB_gCreate
        PROCEDURE :: HDB_createGroup
        PROCEDURE :: HDB_namedItemExists
        PROCEDURE :: HDB_getOpenLength
        PROCEDURE :: HDB_pathExists
        PROCEDURE :: HDB_getDatasetItems
        PROCEDURE :: HDB_allocDSetDims
        PROCEDURE :: HDB_getDatasetRank
        PROCEDURE, NOPASS :: HDB_extendArray
        PROCEDURE :: HDB_getExtendRow
        PROCEDURE :: HDB_isExtendable
        PROCEDURE :: HDB_extendDataset
        PROCEDURE :: HDB_loadHyperslabs
        PROCEDURE :: HDB_setRank
    END TYPE
CONTAINS

    SUBROUTINE HDB_errorCheck( self, lineNumber, message )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER, INTENT( in ) :: lineNumber
        CHARACTER( len=* ), INTENT( in ), OPTIONAL :: message
        CHARACTER( len=* ), PARAMETER :: fmt = "( I0, ':', A, TR4, 'Error:', I0, /, :, 'msg:', A, / )"
        CHARACTER( len=1024 ) :: wrappedMsg
        IF( self%iError < 0 ) THEN
            IF( present( message ) ) THEN
                WRITE( wrappedMsg, fmt=fmt ) lineNumber, self%filename, self%iError, message
            ELSE
                WRITE( wrappedMsg, fmt=fmt ) lineNumber, self%filename, self%iError
            END IF
            WRITE( *, "('%-E- ', A)" ) trim( wrappedMsg )
            CALL SM_logMessage( SM_ERROR, trim( wrappedMsg ))
            STOP self%iError
        END IF
    END SUBROUTINE

    SUBROUTINE HDB_showLine( self, lineNumber, routine, msgLevel, message, number )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER, INTENT( in ) :: lineNumber
        CHARACTER( len=* ), INTENT( in ) :: routine
        INTEGER( kind( SM_TYPE )), INTENT( IN ), VALUE :: msgLevel
        CHARACTER( len=* ), INTENT( in ), OPTIONAL :: message
        INTEGER( kind=HSIZE_T ), INTENT( in ), OPTIONAL :: number
        CHARACTER( len=* ), PARAMETER :: fmt = "( '[', I0, ']', TR4, I0, ': ', A, :TR4, '==> ', A, :TR2, I0 )"
        CHARACTER( len=1024 ) :: wrappedMsg
        IF( present( message ) .and. present( number )) THEN
            WRITE( wrappedMsg, fmt=fmt ) self%myRank, lineNumber, routine, trim( message ), number
        ELSE IF( present( message )) THEN
            WRITE( wrappedMsg, fmt=fmt ) self%myRank, lineNumber, routine, trim( message )
        ELSE
            WRITE( wrappedMsg, fmt=fmt ) self%myRank, lineNumber, routine
        END IF
        CALL SM_logMessage( msgLevel, trim( wrappedMsg ))
    END SUBROUTINE

    SUBROUTINE HDB_Open( self )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        CALL self%HDB_showLine( __LINE__, 'HDB_Open', SM_DEBUG )
        CALL H5open_f( self%iError )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( self%iError < 0 ) PRINT *,'HDB_Open: ERROR - Failed to initialize the HDF5 library.', self%iError
    END SUBROUTINE

    SUBROUTINE HDB_Close( self )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        CALL self%HDB_showLine( __LINE__, 'HDB_Close', SM_DEBUG )
        CALL H5close_f( self%iError ) ! Flushes all data to disk, closes all open identifiers, and cleans up memory.
        CALL self%HDB_errorCheck( __LINE__ )
    END SUBROUTINE

    SUBROUTINE HDB_fClose( self )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        CALL self%HDB_showLine( __LINE__, 'HDB_fClose', SM_DEBUG )
        !PRINT *, self%myRank, 'HDMod::HDB_fClose filename', self%filename
        CALL self%HDB_showLine( __LINE__, 'HDB_fClose', SM_DEBUG, 'closing filename: ' // self%filename )
        CALL H5Fclose_f( self%file_id, self%iError )
        CALL self%HDB_errorCheck( __LINE__ )
    END SUBROUTINE

    SUBROUTINE HDB_fOpen( self )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        LOGICAL :: ex
        INQUIRE( file=self%filename, exist=ex, IOSTAT=self%iError )
        IF( .not. ex ) THEN
            CALL self%HDB_showLine( __LINE__, 'HDB_constructor', SM_DEBUG, 'creating filename: ' // self%filename )
            CALL H5Fcreate_f( self%filename, H5F_ACC_TRUNC_F, self%file_id, self%iError )
            CALL self%HDB_errorCheck( __LINE__, 'Can''t create file' )
        ELSE
            CALL self%HDB_showLine( __LINE__, 'HDB_constructor', SM_DEBUG, 'opening filename: ' // self%filename )
            CALL H5Fopen_f( self%filename, H5F_ACC_RDWR_F, self%file_id, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
        END IF
    END SUBROUTINE

    SUBROUTINE HDB_gCreate( self, groupName )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        CHARACTER( len=* ), INTENT( in ) :: groupName
        CHARACTER, PARAMETER :: solidus = '/'
        CHARACTER( len=1+len( groupName )) :: gnPlus
        INTEGER :: currentPos
        INTEGER :: scanStart
        CALL self%HDB_showLine( __LINE__, 'HDB_gCreate', SM_DEBUG )
        gnPlus = trim( groupName ) // solidus
        scanStart = 1
        currentPos = 1
        self%group_id = 0
        IF( solidus == gnPlus(1:1)) scanStart = 2
        DO
            currentPos = index( gnPlus( scanStart:), solidus )
            IF( 0 == currentPos ) EXIT
            scanStart = scanStart +currentPos
            CALL self%HDB_createGroup( gnPlus(:scanStart-2))
        END DO
    END SUBROUTINE

    SUBROUTINE HDB_createGroup( self, groupPathName )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        CHARACTER( len=* ), INTENT( in ) :: groupPathName
        CALL self%HDB_showLine( __LINE__, 'HDB_createGroup', SM_DEBUG )
        IF( .not. self%HDB_namedItemExists( groupPathName )) THEN
            CALL H5Gcreate_f( self%file_id, trim( groupPathName ), self%group_id, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
        ELSE
            CALL H5Gopen_f( self%file_id, trim( groupPathName ), self%group_id, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
        END IF
    END SUBROUTINE

    FUNCTION HDB_namedItemExists( self, groupPathName ) RESULT( response )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        CHARACTER( len=* ), INTENT( in ) :: groupPathName
        LOGICAL :: response
        CALL self%HDB_showLine( __LINE__, 'HDB_namedItemExists', SM_DEBUG, groupPathName )
        response = .false.
        IF( 0 < len( trim( groupPathname ))) THEN
            CALL H5Lexists_f( self%file_id, trim( groupPathName ), response, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
        END IF
    END FUNCTION

    FUNCTION HDB_getOpenLength( self, dSetName, dimShape ) RESULT( response )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        CHARACTER( len=* ), INTENT( in ) :: dSetName
        INTEGER, INTENT( in ), DIMENSION( : ) :: dimShape
        INTEGER( HSIZE_T ) :: response

        CHARACTER, PARAMETER :: solidus = '/'
        INTEGER :: solidusIndex
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: fileDims
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: fileDimsMax
        INTEGER :: dimSize

        CALL self%HDB_showLine( __LINE__, 'HDB_getOpenLength', SM_DEBUG, dSetName )
        solidusIndex = index( dSetName, solidus, back=.true. )
        IF( 1 < solidusIndex ) CALL self%HDB_gCreate( dSetName(:solidusIndex-1))
        IF( .not. self%HDB_pathExists( dSetName )) THEN
            fileDims = HDB_extendArray( dimShape, int( 1, kind=HSIZE_T ))
            fileDimsMax = HDB_extendArray( dimShape, H5S_UNLIMITED_F )
!            CALL rDump( fileDimsMax, 'HDB_getOpenLength fileDimsMax' )
            dimSize = size( fileDims )
            CALL H5Screate_simple_f( dimSize, fileDims, self%dSpace_id, self%iError, fileDimsMax )
            CALL self%HDB_errorCheck( __LINE__ )
            CALL H5Pcreate_f( H5P_DATASET_CREATE_F, self%prp_id, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
            CALL H5Pset_chunk_f( self%prp_id, dimSize, fileDims, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
            CALL h5pset_deflate_f( self%prp_id, 6, self%iError)
            CALL self%HDB_errorCheck( __LINE__ )
            CALL H5Dcreate_f( self%file_id, dSetName, self%dType_id, self%dSpace_id, self%dSet_id, self%iError, self%prp_id )
            CALL self%HDB_errorCheck( __LINE__ )
            response = 0
            IF( allocated( fileDims )) DEALLOCATE( fileDims )
            IF( allocated( fileDimsMax )) DEALLOCATE( fileDimsMax )
        ELSE
            CALL H5Dopen_f( self%file_id, dSetName, self%dSet_id, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
            CALL H5Dget_space_f( self%dSet_id, self%dSpace_id, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
            response = self%HDB_getDatasetItems()
        END IF
        CALL self%HDB_showLine( __LINE__, 'HDB_getOpenLength', SM_DEBUG, 'returning', response )
    END FUNCTION

    FUNCTION HDB_pathExists( self, itemPathName ) RESULT( response )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        CHARACTER( len=* ), INTENT( in ) :: itemPathName
        LOGICAL :: response
        CHARACTER, PARAMETER :: solidus = '/'
        CHARACTER( len=1+len( itemPathName )) :: fpnPath
        INTEGER :: currentPos
        INTEGER :: scanStart
        CALL self%HDB_showLine( __LINE__, 'HDB_pathExists', SM_DEBUG, itemPathName )
        response = .false.
        IF( itemPathName( 1:1 ) == solidus ) THEN ! all paths are to be absolute
            fpnPath = itemPathName
        ELSE
            fpnPath = '/' // trim( itemPathName )
        END IF
        scanStart = 2
        DO
            currentPos = index( fpnPath( scanStart:), solidus )
            scanStart = scanStart +currentPos
            IF( 0 /= currentPos ) THEN
                response = self%HDB_namedItemExists( fpnPath(:scanStart-2))
                IF( .not. response ) EXIT
            ELSE
                response = self%HDB_namedItemExists( fpnPath )
                EXIT
            END IF
        END DO
    END FUNCTION

    FUNCTION HDB_getDatasetItems( self ) RESULT( response )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER( HSIZE_T ) :: response
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: datasetDims
        CALL self%HDB_allocDSetDims( datasetDims )
        IF( 0 < size( shape( datasetDims ))) THEN
            response = datasetDims( size( shape( datasetDims ))) ! get last element in array
        ELSE
            response = 0_HSIZE_T
        END IF
        IF( allocated( datasetDims )) DEALLOCATE( datasetDims )
        CALL self%HDB_showLine( __LINE__, 'HDB_getDatasetItems', SM_DEBUG, 'returning', response )
    END FUNCTION

    SUBROUTINE HDB_allocDSetDims( self, response )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER( HSIZE_T ), INTENT( out ), DIMENSION( : ), ALLOCATABLE :: response
        INTEGER :: fileRank
        CALL self%HDB_showLine( __LINE__, 'HDB_allocDSetDims', SM_DEBUG )
        fileRank = self%HDB_getDatasetRank()
        BLOCK
            INTEGER( HSIZE_T ), DIMENSION( fileRank ) :: tDims, tMaxdims
            CALL H5Sget_simple_extent_dims_f( self%dSpace_id, tDims, tMaxdims, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
            ALLOCATE( response, source=tDims )
        END BLOCK
    END SUBROUTINE

    FUNCTION HDB_getDatasetRank( self ) RESULT( response )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER :: response
        INTEGER :: fileRank
        CALL H5Sget_simple_extent_ndims_f( self%dSpace_id, fileRank, self%iError )
        CALL self%HDB_errorCheck( __LINE__ )
        response = fileRank
        CALL self%HDB_showLine( __LINE__, 'HDB_getDatasetRank', SM_DEBUG, 'returning', int( response, kind=real64 ))
    END FUNCTION

    FUNCTION HDB_extendArray( x, v ) RESULT( y )
        INTEGER, INTENT( in ) :: x( : )
        INTEGER( HSIZE_T ), INTENT( in ) :: v
        INTEGER( HSIZE_T ), ALLOCATABLE :: y( : )
        INTEGER :: i
        i = size( x )
        ALLOCATE( y( 1 +i ))
        y( 2: ) = x
        y( 1 ) = v
    END FUNCTION

    FUNCTION HDB_getExtendRow( self, rowNumber, lineNumber ) RESULT( response )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER( HSIZE_T ), INTENT( in ) :: rowNumber
        INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        INTEGER( HSIZE_T ) :: response
        IF( present( lineNumber )) THEN ! rowNumber is now the extisting (old) number of lines in the dataset
            CALL self%HDB_showLine( __LINE__, 'HDB_getExtendRow', SM_DEBUG, 'lineNumber', lineNumber )
            response = lineNumber -1
            IF( rowNumber < lineNumber ) THEN
                IF( self%HDB_isExtendable()) THEN
                    CALL self%HDB_extendDataset( lineNumber ) ! extend the file by the requested amount.
                ELSE
                    PRINT *, 'HDB_getExtendRow Can''t extend the file. Sorry. Replacing the last line of the file. (rowNumber)'
                END IF
            END IF
        ELSE
            CALL self%HDB_showLine( __LINE__, 'HDB_getExtendRow', SM_DEBUG, 'no lineNumber supplied' )
            IF( 0 < rowNumber ) THEN
                CALL self%HDB_extendDataset( 1+rowNumber )
                response = rowNumber
            ELSE
                response = 0
            END IF
        END IF
    END FUNCTION

    FUNCTION HDB_isExtendable( self ) RESULT( response )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER( HID_T ) :: dcpl
        INTEGER :: layout
        LOGICAL :: response
        CALL self%HDB_showLine( __LINE__, 'HDB_isExtendable', SM_DEBUG )
        CALL H5Dget_create_plist_f( self%dSet_id, dcpl, self%iError )
        CALL self%HDB_errorCheck( __LINE__ )
        CALL H5Pget_layout_f( dcpl, layout, self%iError )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( layout == H5D_CHUNKED_F ) THEN
            response = .true.
        ELSE
            response = .false.
        END IF
    END FUNCTION

    SUBROUTINE HDB_extendDataset( self, newLength )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER( HSIZE_T ), INTENT( in ) :: newLength
        INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: newDims
        CALL self%HDB_showLine( __LINE__, 'HDB_extendDataset', SM_DEBUG, 'newLength', newLength )
        CALL self%HDB_allocDSetDims( newDims )
        newDims( 1 ) = newLength
        CALL H5Dset_extent_f( self%dSet_id, newDims, self%iError )
        CALL self%HDB_errorCheck( __LINE__ )
        IF( allocated( newDims )) DEALLOCATE( newDims )
    END SUBROUTINE

    SUBROUTINE HDB_loadHyperslabs( self, rowNumber )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER( HSIZE_T ) :: rowNumber
        INTEGER :: fileRank
        CALL self%HDB_showLine( __LINE__, 'HDB_loadHyperslabs', SM_DEBUG, 'rowNumber', rowNumber )
        fileRank = self%HDB_getDatasetRank()
        CALL H5Dget_space_f( self%dSet_id, self%dSpace_id, self%iError ) ! need a fresh copy of the dataspace !?!?
        CALL self%HDB_errorCheck( __LINE__ )
        BLOCK
            INTEGER( HSIZE_T ), DIMENSION( fileRank ) :: hsOffset ! hyperslab offset in the file
            INTEGER( HSIZE_T ), DIMENSION( fileRank ) :: hsCount ! Size of the hyperslab in the file and in memory
            INTEGER( HSIZE_T ), DIMENSION( : ), ALLOCATABLE :: datasetDims

            CALL self%HDB_allocDSetDims( datasetDims )

            hsOffset = 0_HSIZE_T
            hsOffset( 1 ) = rowNumber
            hsCount = datasetDims
            hsCount( 1 ) = 1_HSIZE_T

            CALL H5Sselect_hyperslab_f( self%dSpace_id, H5S_SELECT_SET_F, hsOffset, hsCount, self%iError ) !, hsStride, hsBlock )
            CALL self%HDB_errorCheck( __LINE__ )

            CALL H5Screate_simple_f( fileRank, hsCount, self%mSpace_id, self%iError )
            CALL self%HDB_errorCheck( __LINE__ )
            IF( allocated( datasetDims )) DEALLOCATE( datasetDims )
        END BLOCK
    END SUBROUTINE

    SUBROUTINE HDB_setRank( self, myRank )
        CLASS( HDBaseType ), INTENT( inout ) :: self
        INTEGER, INTENT( in ) :: myRank
        CALL self%HDB_showLine( __LINE__, 'HDB_setRank', SM_DEBUG )
        self%myRank = myRank
    END SUBROUTINE

END MODULE
