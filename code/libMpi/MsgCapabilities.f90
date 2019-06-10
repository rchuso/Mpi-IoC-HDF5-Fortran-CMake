! Written by Rand Huso

MODULE MsgCapabilities
    USE :: mpi
    USE :: MsgBase
    USE :: Capabilities
    IMPLICIT NONE

    CHARACTER( len=* ), PARAMETER :: MCT_msgName     = 'Capabilities'
    INTEGER, PARAMETER            :: MCT_msgTag      = 97
    INTEGER                       :: MCT_mpiDatatype = MPI_DATATYPE_NULL

    TYPE, EXTENDS( MsgBaseType ) :: MsgCapabilitiesType
        INTEGER :: mRank
        INTEGER :: mSize
        CHARACTER( len=MPI_MAX_PROCESSOR_NAME ) :: hostname
        INTEGER :: freeRam
        INTEGER :: totalRam
        INTEGER :: cores
    CONTAINS
        PROCEDURE :: MCT_loadSysInfo
        PROCEDURE :: str => MCT_getStr
        PROCEDURE :: getMsgName => MCT_getName
        PROCEDURE :: getMsgTag => MCT_getTag
        PROCEDURE :: getMpiDatatype => MCT_getMpiDatatype
        PROCEDURE, PRIVATE :: MCT_loadMpiDatatype
    END TYPE MsgCapabilitiesType

    INTERFACE MsgCapabilitiesType
        PROCEDURE MCT_constructor
    END INTERFACE

CONTAINS
    FUNCTION MCT_getName( self ) RESULT( iName )
        CLASS( MsgCapabilitiesType ), INTENT( in ) :: self
        CHARACTER( len=132 ) :: iName
        iName = MCT_msgName
    END FUNCTION

    FUNCTION MCT_getMpiDatatype( self )
        CLASS( MsgCapabilitiesType ) :: self
        INTEGER :: MCT_getMpiDatatype
        IF( MPI_DATATYPE_NULL == MCT_mpiDatatype ) CALL self%MCT_loadMpiDatatype()
        MCT_getMpiDatatype = MCT_mpiDatatype
    END FUNCTION

    FUNCTION MCT_getTag( self ) RESULT( response )
        CLASS( MsgCapabilitiesType ) :: self
        INTEGER :: response
        response = MCT_msgTag
    END FUNCTION

    SUBROUTINE MCT_loadMpiDatatype( self )
        CLASS( MsgCapabilitiesType ) :: self
        TYPE( MpiAssistType ) :: maType
        maType = MpiAssistType( 7 )
        CALL maType%MA_loadComponent( thisItem=self%baseId, itemNumber=1, itemCount=1, itemType=MPI_INTEGER )
        CALL maType%MA_loadComponent( thisItem=self%mRank, itemNumber=2, itemCount=1, itemType=MPI_INTEGER )
        CALL maType%MA_loadComponent( thisItem=self%mSize, itemNumber=3, itemCount=1, itemType=MPI_INTEGER )
        CALL maType%MA_loadComponent( thisItem=self%hostname, itemNumber=4, itemCount=MPI_MAX_PROCESSOR_NAME, &
            itemType=MPI_CHARACTER )
        CALL maType%MA_loadComponent( thisItem=self%freeRam, itemNumber=5, itemCount=1, itemType=MPI_LONG )
        CALL maType%MA_loadComponent( thisItem=self%totalRam, itemNumber=6, itemCount=1, itemType=MPI_LONG )
        CALL maType%MA_loadComponent( thisItem=self%cores, itemNumber=7, itemCount=1, itemType=MPI_INTEGER )
        MCT_mpiDatatype = maType%MA_getType()
    END SUBROUTINE

    FUNCTION MCT_getStr( self )
        CLASS( MsgCapabilitiesType ), INTENT( in ) :: self
        CHARACTER( len=132 ) :: MCT_getStr
        IF( MPI_DATATYPE_NULL == MCT_mpiDatatype ) MCT_mpiDatatype = self%getMpiDatatype()
        WRITE( MCT_getStr, &
            '("MCT:: rank=", I0, " size=", I0, " datatype=", I0, " RAM free:", I0, " total:", I0, " cores:", I0, 1X, A)' ) &
            self%mRank, self%mSize, MCT_mpiDatatype, self%freeRam, self%totalRam, self%cores, trim( self%hostname )
    END FUNCTION

    FUNCTION MCT_constructor() RESULT( self )
        TYPE( MsgCapabilitiesType ) :: self
        INTEGER :: iErr
        CALL MPI_COMM_RANK( MPI_COMM_WORLD, self%mRank, iErr )
        CALL MPI_COMM_SIZE( MPI_COMM_WORLD, self%mSize, iErr )
        CALL self%MCT_loadSysInfo()
    END FUNCTION

    SUBROUTINE MCT_loadSysInfo( self )
        CLASS( MsgCapabilitiesType ), INTENT( inout ) :: self
        INTEGER :: freeRam
        INTEGER :: totalRam
        INTEGER :: cores
        CHARACTER( len=MPI_MAX_PROCESSOR_NAME ) :: hostname
        CALL C_loadSysInfo( self%freeRam, self%totalRam, self%cores, self%hostname )
    END SUBROUTINE
END MODULE
