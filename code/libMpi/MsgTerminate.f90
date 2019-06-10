! Written by Rand Huso

MODULE MsgTerminate
    USE :: MsgBase
    USE :: mpi
    IMPLICIT NONE

    CHARACTER( len=* ), PARAMETER :: MTT_msgName     = 'Terminate'
    INTEGER, PARAMETER            :: MTT_msgTag      = 61
    INTEGER                       :: MTT_mpiDatatype = MPI_DATATYPE_NULL

    TYPE, EXTENDS( MsgBaseType ) :: MsgTerminateType
    CONTAINS
        PROCEDURE :: getMpiDatatype => MTT_getMpiDatatype
        PROCEDURE :: getMsgName => MTT_getName
        PROCEDURE :: str => MTT_getStr
        PROCEDURE :: getMsgTag => MTT_getMsgTag
        PROCEDURE, PRIVATE :: MTT_loadMpiDatatype
    END TYPE

CONTAINS

    FUNCTION MTT_getMsgTag( self ) RESULT( response )
        CLASS( MsgTerminateType ) :: self
        INTEGER :: response
        response = MTT_msgTag
    END FUNCTION

    FUNCTION MTT_getName( self ) RESULT( iName )
        CLASS( MsgTerminateType ), INTENT( in ) :: self
        CHARACTER( len=132 ) :: iName
        iName = MTT_msgName
    END FUNCTION

    SUBROUTINE MTT_loadMpiDatatype( self )
        CLASS( MsgTerminateType ) :: self
        TYPE( MpiAssistType ) :: maType
        maType = MpiAssistType( 1 )
        CALL maType%MA_loadComponent( thisItem=self%baseId, itemNumber=1, itemCount=1, itemType=MPI_INTEGER )
        MTT_mpiDatatype = maType%MA_getType()
    END SUBROUTINE

    FUNCTION MTT_getMpiDatatype( self )
        CLASS( MsgTerminateType ) :: self
        INTEGER :: MTT_getMpiDatatype
        IF( MPI_DATATYPE_NULL == MTT_mpiDatatype ) CALL self%MTT_loadMpiDatatype()
        MTT_getMpiDatatype = MTT_mpiDatatype
    END FUNCTION

    FUNCTION MTT_getStr( self )
        CLASS( MsgTerminateType ), INTENT( in ) :: self
        CHARACTER( len=132 ) :: MTT_getStr
        IF( MPI_DATATYPE_NULL == MTT_mpiDatatype ) MTT_mpiDatatype = self%getMpiDatatype()
        WRITE( MTT_getStr, '("MTT:: datatype=", I0)' ) MTT_mpiDatatype
    END FUNCTION
END MODULE
