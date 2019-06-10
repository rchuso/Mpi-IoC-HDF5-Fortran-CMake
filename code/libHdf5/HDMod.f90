! Written by Rand Huso

MODULE HDMod
    USE, INTRINSIC :: iso_fortran_env, ONLY : int64, int32, real64, real32
    USE :: HDBaseMod
    IMPLICIT NONE

    TYPE, EXTENDS( HDBaseType ) :: HDType
    CONTAINS
        PROCEDURE, PRIVATE :: HD_read1Int32i, HD_read1Int32, HD_read2Int32, HD_read2Int32i, HD_read3Int32, HD_read3Int32i
        PROCEDURE, PRIVATE :: HD_write1Int32, HD_write1Int32i, HD_write2Int32, HD_write2Int32i, HD_write3Int32, HD_write3Int32i
        PROCEDURE, PRIVATE :: HD_read1Int64, HD_read1Int64i, HD_read2Int64, HD_read2Int64i, HD_read3Int64, HD_read3Int64i
        PROCEDURE, PRIVATE :: HD_write1Int64, HD_write1Int64i, HD_write2Int64, HD_write2Int64i, HD_write3Int64, HD_write3Int64i
        PROCEDURE, PRIVATE :: HD_read1Real32, HD_read1Real32i, HD_read2Real32, HD_read2Real32i, HD_read3Real32, HD_read3Real32i
        PROCEDURE, PRIVATE :: HD_write1Real32, HD_write1Real32i, HD_write2Real32, HD_write2Real32i, HD_write3Real32,HD_write3Real32i
        PROCEDURE, PRIVATE :: HD_read1Real64, HD_read1Real64i, HD_read2Real64, HD_read2Real64i, HD_read3Real64, HD_read3Real64i
        PROCEDURE, PRIVATE :: HD_write1Real64, HD_write1Real64i, HD_write2Real64, HD_write2Real64i, HD_write3Real64,HD_write3Real64i

        GENERIC :: write => &
            HD_write1Int32, HD_write1Int32i, HD_write2Int32, HD_write2Int32i, HD_write3Int32, HD_write3Int32i, &
            HD_write1Int64, HD_write1Int64i, HD_write2Int64, HD_write2Int64i, HD_write3Int64, HD_write3Int64i, &
            HD_write1Real32, HD_write1Real32i, HD_write2Real32, HD_write2Real32i, HD_write3Real32, HD_write3Real32i, &
            HD_write1Real64, HD_write1Real64i, HD_write2Real64, HD_write2Real64i, HD_write3Real64, HD_write3Real64i

        GENERIC :: read => &
            HD_read1Int32, HD_read1Int32i, HD_read2Int32, HD_read2Int32i, HD_read3Int32, HD_read3Int32i, &
            HD_read1Int64, HD_read1Int64i, HD_read2Int64, HD_read2Int64i, HD_read3Int64, HD_read3Int64i, &
            HD_read1Real32, HD_read1Real32i, HD_read2Real32, HD_read2Real32i, HD_read3Real32, HD_read3Real32i, &
            HD_read1Real64, HD_read1Real64i, HD_read2Real64, HD_read2Real64i, HD_read3Real64, HD_read3Real64i

        FINAL :: HD_dsestructor
    END TYPE

    INTERFACE HDType
        MODULE PROCEDURE HD_constructor
    END INTERFACE

    INTERFACE
        MODULE SUBROUTINE HD_read1Int32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( : ), INTENT( inout ) :: buf
            INTEGER, INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE  SUBROUTINE HD_read1Int32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( : ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ) :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_read2Int32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( :, : ), INTENT( inout ) :: buf
            INTEGER, INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read2Int32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( :, : ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ) :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_read3Int32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( :, :, : ), INTENT( inout ) :: buf
            INTEGER, INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read3Int32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( :, :, : ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ) :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write1Int32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( : ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write1Int32( self, dSetName, buf, lineNumber ) ! TEMPLATES would be so much simpler, or CLASS(*)
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( : ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write2Int32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( :,: ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write2Int32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( :,: ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write3Int32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( :,:,: ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write3Int32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int32 ), DIMENSION( :,:,: ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_read1Int64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( : ), INTENT( inout ) :: buf
            INTEGER, INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read1Int64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( : ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ) :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_read2Int64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( :, : ), INTENT( inout ) :: buf
            INTEGER, INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read2Int64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( :, : ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ) :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_read3Int64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( :, :, : ), INTENT( inout ) :: buf
            INTEGER, INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read3Int64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( :, :, : ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ) :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write1Int64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( : ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write1Int64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( : ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write2Int64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( :,: ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write2Int64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( :,: ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write3Int64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( :,:,: ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write3Int64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            INTEGER( kind=int64 ), DIMENSION( :,:,: ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_read1Real32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( : ), INTENT( inout ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read1Real32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( : ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read2Real32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( :,: ), INTENT( inout ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read2Real32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( :,: ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read3Real32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( :,:,: ), INTENT( inout ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read3Real32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( :,:,: ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write1Real32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( : ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write1Real32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( : ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write2Real32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( :,: ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write2Real32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( :,: ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write3Real32i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( :,:,: ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write3Real32( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real32 ), DIMENSION( :,:,: ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_read1Real64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( : ), INTENT( inout ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read1Real64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( : ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_read2Real64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( :,: ), INTENT( inout ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read2Real64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( :,: ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_read3Real64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( :,:,: ), INTENT( inout ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_read3Real64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( :,:,: ), INTENT( inout ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write1Real64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( : ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write1Real64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( : ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write2Real64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( :,: ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write2Real64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( :,: ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE

        MODULE SUBROUTINE HD_write3Real64i( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( :,:,: ), INTENT( in ) :: buf
            INTEGER( kind=int32 ), INTENT( in ) :: lineNumber
        END SUBROUTINE
        MODULE SUBROUTINE HD_write3Real64( self, dSetName, buf, lineNumber )
            CLASS( HDType ), INTENT( inout ) :: self
            CHARACTER( len=* ), INTENT( in ) :: dSetName
            REAL( kind=real64 ), DIMENSION( :,:,: ), INTENT( in ) :: buf
            INTEGER( HSIZE_T ), INTENT( in ), OPTIONAL :: lineNumber
        END SUBROUTINE
    END INTERFACE

CONTAINS

    FUNCTION HD_constructor( filename, thisRank ) RESULT( self )
        TYPE( HDType ) :: self
        CHARACTER( len=*), INTENT( in ) :: filename
        INTEGER, OPTIONAL :: thisRank
        CALL self%HDB_showLine( __LINE__, 'HD_constructor', SM_INFO, 'filename: ' // filename )
        IF( present( thisRank )) CALL self%HDB_setRank( thisRank )
        ALLOCATE( self%filename, source=filename )
        CALL self%HDB_Open()
        CALL self%HDB_fOpen()
    END FUNCTION

    ! IMPURE ELEMENTAL
    SUBROUTINE HD_dsestructor( self )
        TYPE( HDType ), INTENT( inout ) :: self
        CALL self%HDB_showLine( __LINE__, 'HD_dsestructor', SM_INFO, 'closing: ' // self%filename )
        IF( self%file_id /= 0) CALL self%HDB_fClose()
        CALL self%HDB_Close()
        IF( allocated( self%filename )) DEALLOCATE( self%filename )
    END SUBROUTINE

END MODULE
