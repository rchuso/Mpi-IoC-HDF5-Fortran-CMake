! Written by Rand Huso

MODULE MpiAssist
    USE :: mpi
    IMPLICIT NONE

    TYPE MpiAssistType
        INTEGER :: MA_elements
        INTEGER( kind=MPI_ADDRESS_KIND ), ALLOCATABLE :: MA_address(:)
        INTEGER, ALLOCATABLE :: MA_len(:)
        INTEGER, ALLOCATABLE :: MA_type(:)
        INTEGER( kind=MPI_ADDRESS_KIND ) :: MA_baseAddress
    CONTAINS
        PROCEDURE :: MA_loadComponent
        PROCEDURE :: MA_getType
        FINAL :: MA_destructor
    END TYPE

    INTERFACE MpiAssistType
        PROCEDURE MA_constructor
    END INTERFACE
CONTAINS

    FUNCTION MA_constructor( elements ) RESULT( self )
        TYPE( MpiAssistType ) :: self
        INTEGER, INTENT( in ) :: elements

        self%MA_elements = elements
        ALLOCATE( self%MA_address( self%MA_elements ))
        ALLOCATE( self%MA_len( self%MA_elements ), SOURCE=0)
        ALLOCATE( self%MA_type( self%MA_elements ), SOURCE=0)
    END FUNCTION

    SUBROUTINE MA_destructor( self )
        TYPE( MpiAssistType ) :: self
        IF( allocated( self%MA_address )) DEALLOCATE( self%MA_address )
        IF( allocated( self%MA_len )) DEALLOCATE( self%MA_len )
        IF( allocated( self%MA_type )) DEALLOCATE( self%MA_type )
    END SUBROUTINE

    SUBROUTINE MA_loadComponent( self, thisItem, itemNumber, itemCount, itemType )
        CLASS( MpiAssistType ), INTENT( inout ) :: self
        CLASS( * ), INTENT( in ) :: thisItem
        INTEGER, INTENT( in ) :: itemNumber, itemCount, itemType
        INTEGER :: iErr

        CALL MPI_GET_ADDRESS( thisItem, self%MA_address( itemNumber ), iErr )
        IF( 1 == itemNumber ) self%MA_baseAddress = self%MA_address( itemNumber )
        self%MA_address( itemNumber ) = ( self%MA_address( itemNumber ) -self%MA_baseAddress )
        self%MA_len( itemNumber ) = itemCount
        self%MA_type( itemNumber ) = itemType
    END SUBROUTINE

    FUNCTION MA_getType( self )
        CLASS( MpiAssistType ), INTENT( inout ) :: self
        INTEGER :: MA_getType
        INTEGER :: iErr
        CALL MPI_TYPE_CREATE_STRUCT( self%MA_elements, self%MA_len, self%MA_address, self%MA_type, MA_getType, iErr )
        CALL MPI_TYPE_COMMIT( MA_getType, iErr )
    END FUNCTION
END MODULE
