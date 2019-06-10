! Written by Rand Huso

MODULE MsgTest
    USE :: MsgBase
    USE :: mpi
    IMPLICIT NONE

    INTEGER :: MXT_mpiDatatype = MPI_DATATYPE_NULL
    CHARACTER( len=* ), PARAMETER :: MXT_msgName = 'Test'
    INTEGER, PARAMETER :: MXT_msgTag = 37

    TYPE, EXTENDS( MsgBaseType ) :: MsgTestType
        REAL :: myPi = 3.14159
    CONTAINS
        PROCEDURE :: getMpiDatatype => MXT_getMpiDatatype
        PROCEDURE :: str => MXT_getStr
        PROCEDURE :: getMsgName => MXT_getName
        PROCEDURE :: getMsgTag => MXT_getMsgTag
        PROCEDURE, PRIVATE :: MXT_loadMpiDatatype
    END TYPE

    INTERFACE MsgTestType
        PROCEDURE MXT_constructor
    END INTERFACE

CONTAINS

    FUNCTION MXT_constructor() RESULT( self )
        TYPE( MsgTestType ) :: self
        INTEGER :: mpiDatatype
         mpiDatatype = self%getMpiDatatype()
        self%myPi = 2.718281828
    END FUNCTION

    FUNCTION MXT_getMsgTag( self ) RESULT( response )
        CLASS( MsgTestType ) :: self
        INTEGER :: response
        response = MXT_msgTag
    END FUNCTION

    FUNCTION MXT_getName( self ) RESULT( iName )
        CLASS( MsgTestType ), INTENT( in ) :: self
        CHARACTER( len=132 ) :: iName
        iName = MXT_msgName
    END FUNCTION

    SUBROUTINE MXT_loadMpiDatatype( self )
        CLASS( MsgTestType ) :: self
        TYPE( MpiAssistType ) :: maType
        maType = MpiAssistType( 2 )
        CALL maType%MA_loadComponent( thisItem=self%baseId, itemNumber=1, itemCount=1, itemType=MPI_INTEGER )
        CALL maType%MA_loadComponent( thisItem=self%myPi, itemNumber=2, itemCount=1, itemType=MPI_REAL )
        MXT_mpiDatatype = maType%MA_getType()
    END SUBROUTINE

    FUNCTION MXT_getMpiDatatype( self )
        CLASS( MsgTestType ) :: self
        INTEGER :: MXT_getMpiDatatype
        IF( MPI_DATATYPE_NULL == MXT_mpiDatatype ) CALL self%MXT_loadMpiDatatype()
        MXT_getMpiDatatype = MXT_mpiDatatype
    END FUNCTION

    FUNCTION MXT_getStr( self )
        CLASS( MsgTestType ), INTENT( in ) :: self
        CHARACTER( len=132 ) :: MXT_getStr
        IF( MPI_DATATYPE_NULL == MXT_mpiDatatype ) MXT_mpiDatatype = self%getMpiDatatype()
        WRITE( MXT_getStr, '("MXT:: myPi=", F5.3, " datatype=", I0)' ) self%myPi, MXT_mpiDatatype
    END FUNCTION
END MODULE
