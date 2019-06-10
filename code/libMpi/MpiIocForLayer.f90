! Written by Rand Huso

MODULE MpiIocForLayer
    USE :: MpiIocForAppCallback
    USE :: MPI
    USE :: MsgBase
    USE :: MsgCapabilities
    USE :: MsgTerminate
    USE :: MpiIocForBinaryTree
    IMPLICIT NONE

    TYPE :: MpiIocForLayerType
        INTEGER :: mRank = 0
        INTEGER :: mSize = 0
        CLASS( MpiIocForAppCallbackType), POINTER :: appObject
        TYPE( MpiIocForBinaryTreeType ) :: binaryTree
        LOGICAL :: doLoop = .true.
    CONTAINS
        PROCEDURE :: start => MIFLT_start
        PROCEDURE :: txMessage => MIFLT_txMessage
        PROCEDURE :: txTerminate => MIFLT_txTerminate
        PROCEDURE :: loadType => MIFLT_loadType
        PROCEDURE, PRIVATE :: MIFLT_initMpi
        PROCEDURE, PRIVATE :: MIFLT_loadInitialObjects
        PROCEDURE, PRIVATE :: MIFLT_sendCapabilities
        PROCEDURE, PRIVATE :: MIFLT_rxMessage
        PROCEDURE, PRIVATE :: MIFLT_getStoredObject
        PROCEDURE, PRIVATE :: MIFLT_finalizeMpi
    END TYPE

    INTERFACE MpiIocForLayerType
        PROCEDURE MIFLT_constructor
    END INTERFACE
CONTAINS

    FUNCTION MIFLT_constructor( appObject ) RESULT( self )
        TYPE( MpiIocForLayerType ) :: self
        CLASS( MpiIocForAppCallbackType ), POINTER :: appObject
        self%appObject => appObject
        CALL self%MIFLT_initMpi()
        CALL self%MIFLT_loadInitialObjects()
    END FUNCTION

    SUBROUTINE MIFLT_initMpi( self )
        CLASS( MpiIocForLayerType ) :: self
        INTEGER :: iErr
        LOGICAL :: initialized
        CALL MPI_Initialized( initialized, iErr )
        IF( .NOT. initialized ) THEN
            CALL MPI_INIT( iErr )
            CALL MPI_COMM_RANK( MPI_COMM_WORLD, self%mRank, iErr )
            CALL MPI_COMM_SIZE( MPI_COMM_WORLD, self%mSize, iErr )
        ELSE
            PRINT *, '        MIFLT_initMpi:: - should not be called again - already initialized.'
        END IF
    END SUBROUTINE

    SUBROUTINE MIFLT_loadType( self, message )
        CLASS( MpiIocForLayerType ) :: self
        CLASS( MsgBaseType ) :: message
        INTEGER :: tag
        INTEGER :: mpiDatatype
        tag = message%getMsgTag()
        mpiDatatype = message%getMpiDatatype()
        CALL self%binaryTree%addItem( message, tag )
    END SUBROUTINE

    SUBROUTINE MIFLT_loadInitialObjects( self )
        CLASS( MpiIocForLayerType ) :: self
        BLOCK
            TYPE( MsgTerminateType ) :: msgTerminate
            CALL self%loadType( msgTerminate )
        END BLOCK
        BLOCK
            TYPE( MsgCapabilitiesType ) :: msgCapabilities
            msgCapabilities = MsgCapabilitiesType()
            CALL self%loadType( msgCapabilities )
        END BLOCK
    END SUBROUTINE

    SUBROUTINE MIFLT_start( self )
        CLASS( MpiIocForLayerType ) :: self
        CALL self%MIFLT_sendCapabilities()
        DO WHILE ( self%doLoop )
            CALL self%MIFLT_rxMessage()
        END DO
        CALL self%MIFLT_finalizeMpi()
    END SUBROUTINE

    SUBROUTINE MIFLT_txMessage( self, message, destination)
        CLASS( MpiIocForLayerType ) :: self
        CLASS( MsgBaseType ), INTENT( inout ) :: message
        INTEGER, INTENT( in ) :: destination
        CLASS( MsgBaseType ), POINTER :: storedObjectPtr
        INTEGER :: mpiDatatype
        INTEGER :: tag
        INTEGER :: iErr
        tag = message%getMsgTag()
        CALL self%MIFLT_getStoredObject( tag, storedObjectPtr )
        IF( associated( storedObjectPtr )) THEN
            mpiDatatype = storedObjectPtr%getMpiDatatype()
            CALL MPI_SEND( message%baseId, 1, mpiDatatype, destination, tag, MPI_COMM_WORLD, iErr )
        END IF
    END SUBROUTINE

    SUBROUTINE MIFLT_txTerminate( self )
        CLASS( MpiIocForLayerType ) :: self
        TYPE( MsgTerminateType ) :: terminate
        INTEGER :: node
        DO node=0, self%mSize-1
            CALL self%txMessage( terminate, node ) ! goes to everyone
        END DO
    END SUBROUTINE

    SUBROUTINE MIFLT_sendCapabilities( self )
        CLASS( MpiIocForLayerType ) :: self
        TYPE( MsgCapabilitiesType ) :: capabilities
        capabilities = MsgCapabilitiesType()
        CALL self%txMessage( capabilities, 0 ) ! these all go to rank=0
    END SUBROUTINE

    SUBROUTINE MIFLT_rxMessage( self )
        CLASS( MpiIocForLayerType ) :: self
        INTEGER :: iErr
        INTEGER, DIMENSION( MPI_STATUS_SIZE ) :: iStat
        INTEGER :: mpiDatatype
        CLASS( MsgBaseType ), POINTER :: storedObjectPtr

        CALL MPI_probe( MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, iStat, iErr )
        CALL self%MIFLT_getStoredObject( iStat( MPI_TAG ), storedObjectPtr )
        IF( associated( storedObjectPtr )) THEN
            mpiDatatype = storedObjectPtr%getMpiDatatype()
            CALL MPI_Recv( storedObjectPtr%baseId, 1, mpiDatatype, iStat( MPI_SOURCE ),iStat( MPI_TAG ),MPI_COMM_WORLD, iStat, iErr)
            SELECT TYPE( storedObjectPtr )
                TYPE is( MsgTerminateType )
                    self%doLoop = .false.
                TYPE is( MsgCapabilitiesType )
                    CALL self%appObject%storeCapabilities( storedObjectPtr )
                CLASS DEFAULT
                    CALL self%appObject%receiveMsg( storedObjectPtr, iStat( MPI_SOURCE ), iStat( MPI_TAG))
            END SELECT
        ELSE
            PRINT *, '        MIFLT_rxMessage:: FAILED to get stored object'
        END IF
    END SUBROUTINE

    SUBROUTINE MIFLT_getStoredObject( self, tag, storedObjectPtr )
        CLASS( MpiIocForLayerType ) :: self
        INTEGER, INTENT( in ) :: tag
        CLASS( MsgBaseType ), INTENT( out ), POINTER :: storedObjectPtr
        CALL self%binaryTree%getItem( storedObjectPtr, tag )
    END SUBROUTINE

    SUBROUTINE MIFLT_finalizeMpi( self )
        CLASS( MpiIocForLayerType ) :: self
        INTEGER :: iErr
        CALL MPI_FINALIZE( iErr )
    END SUBROUTINE
END MODULE
