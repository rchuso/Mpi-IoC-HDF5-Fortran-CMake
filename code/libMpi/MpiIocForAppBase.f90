! Written by Rand Huso

MODULE MpiIocForAppBase
    USE :: mpi
    USE :: MsgBase
    USE :: MsgCapabilities
    USE :: MpiIocForAppCallback
    USE :: MpiIocForLayer
    IMPLICIT NONE

    TYPE :: CapabilitiesList
        CLASS( MsgCapabilitiesType ), POINTER :: capability => Null()
    END TYPE

    TYPE, ABSTRACT, EXTENDS( MpiIocForAppCallbackType ) :: MpiIocForAppBaseType
        TYPE( MpiIocForLayerType ) :: mpiLayer
        INTEGER :: capabilitiesCount = 0
        TYPE( CapabilitiesList ), ALLOCATABLE, DIMENSION(:) :: capabilities
    CONTAINS
        PROCEDURE( iRxMessage ), DEFERRED :: receiveMsg
        PROCEDURE( iLoadUserTypes ), DEFERRED :: loadUserTypes
        PROCEDURE( iStartApp ), DEFERRED :: startApp
        PROCEDURE :: storeCapabilities => MIFABT_storeCapabilities
        PROCEDURE :: initMpiLayer => MIFABT_initMpiLayer
        PROCEDURE :: loadType => MIFABT_loadType
        PROCEDURE :: start => MIFABT_start
        PROCEDURE :: stop => MIFABT_stop
        PROCEDURE :: send => MIFABT_txMessage
        PROCEDURE :: bcast => MIFABT_txAllMessage
        PROCEDURE :: rank => MIFABT_rank
        PROCEDURE :: size => MIFABT_size
        PROCEDURE :: MIFABT_storeCapabilities
        PROCEDURE :: showCapabilities => MIFABT_showCapabilities
    END TYPE

    ABSTRACT INTERFACE
        SUBROUTINE iRxMessage( self, message, sourceNode, tag )
            IMPORT
            CLASS( MpiIocForAppBaseType ), INTENT( inout ) :: self
            CLASS( MsgBaseType ), INTENT( inout ) :: message
            INTEGER, INTENT( in ) :: sourceNode
            INTEGER, INTENT( in ) :: tag
        END SUBROUTINE

        SUBROUTINE iLoadUserTypes( self )
            IMPORT
            CLASS( MpiIocForAppBaseType ), INTENT( inout ) :: self
        END SUBROUTINE

        SUBROUTINE iStartApp( self )
            IMPORT
            CLASS( MpiIocForAppBaseType ), INTENT( inout ) :: self
        END SUBROUTINE
    END INTERFACE

CONTAINS

    SUBROUTINE MIFABT_showCapabilities( self )
        CLASS( MpiIocForAppBaseType ), INTENT( inout ) :: self
        INTEGER :: i
        PRINT *, 'Capabilities (to examine, process, use):'
        DO i=1, self%size()
            PRINT *, i, trim( self%capabilities( i )%capability%str())
        END DO
    END SUBROUTINE

    SUBROUTINE MIFABT_storeCapabilities( self, message )
        CLASS( MpiIocForAppBaseType ), INTENT( inout ) :: self
        CLASS( MsgBaseType ), INTENT( inout ) :: message
        INTEGER :: mSize
        mSize = self%size()
        IF( .not. allocated( self%capabilities )) THEN
            ALLOCATE( self%capabilities( mSize ))
        END IF
        SELECT TYPE( message )
            CLASS is( MsgCapabilitiesType )
                self%capabilitiesCount = self%capabilitiesCount + 1
                ALLOCATE( self%capabilities( self%capabilitiesCount )%capability, SOURCE=message )
            CLASS DEFAULT
                PRINT *, 'wrong capabilities?'
        END SELECT
        IF( self%capabilitiesCount == mSize ) THEN
            CALL self%startApp()
        END IF
    END SUBROUTINE

    SUBROUTINE MIFABT_loadType( self, message )
        CLASS( MpiIocForAppBaseType ) :: self
        CLASS( MsgBaseType ) :: message
        CALL self%mpiLayer%loadType( message )
    END SUBROUTINE

    FUNCTION MIFABT_rank( self ) RESULT( myRank )
        CLASS( MpiIocForAppBaseType ) :: self
        INTEGER :: myRank
        myRank = self%mpiLayer%mRank
    END FUNCTION

    FUNCTION MIFABT_size( self ) RESULT( mySize )
        CLASS( MpiIocForAppBaseType ) :: self
        INTEGER :: mySize
        mySize = self%mpiLayer%mSize
    END FUNCTION

    SUBROUTINE MIFABT_initMpiLayer( self, ptrSelf )
        CLASS( MpiIocForAppBaseType ) :: self
        CLASS( MpiIocForAppCallbackType ), POINTER :: ptrSelf
        self%mpiLayer = MpiIocForLayerType( ptrSelf )
        CALL self%loadUserTypes()
    END SUBROUTINE

    SUBROUTINE MIFABT_start( self )
        CLASS( MpiIocForAppBaseType ) :: self
        CALL self%mpiLayer%start()
    END SUBROUTINE

    SUBROUTINE MIFABT_txMessage( self, message, destinationNode )
        CLASS( MpiIocForAppBaseType ), INTENT( inout ) :: self
        CLASS( MsgBaseType ), INTENT( inout ) :: message
        INTEGER, INTENT( in ) :: destinationNode
        CALL self%mpiLayer%txMessage( message, destinationNode )
    END SUBROUTINE

    SUBROUTINE MIFABT_txAllMessage( self, message )
        CLASS( MpiIocForAppBaseType ), INTENT( inout ) :: self
        CLASS( MsgBaseType ), INTENT( inout ) :: message
        INTEGER rank
        DO rank=0, self%size()-1
            IF( rank == self%rank()) CYCLE
            CALL self%mpiLayer%txMessage( message, rank )
        END DO
    END SUBROUTINE

    SUBROUTINE MIFABT_stop( self )
        CLASS( MpiIocForAppBaseType ) :: self
        CALL self%mpiLayer%txTerminate()
    END SUBROUTINE
END MODULE
