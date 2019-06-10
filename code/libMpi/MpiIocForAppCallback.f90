! Written by Rand Huso

MODULE MpiIocForAppCallback
    USE :: MsgBase
    IMPLICIT NONE

    TYPE, ABSTRACT :: MpiIocForAppCallbackType
    CONTAINS
        PROCEDURE( jRxMessage ), DEFERRED :: receiveMsg
        PROCEDURE( jStoreCapabilities ), DEFERRED :: storeCapabilities
    END TYPE

    ABSTRACT INTERFACE
        SUBROUTINE jRxMessage( self, message, sourceNode, tag )
            IMPORT
            CLASS( MpiIocForAppCallbackType ), INTENT( inout ) :: self
            CLASS( MsgBaseType ), INTENT( inout ) :: message
            INTEGER, INTENT( in ) :: sourceNode
            INTEGER, INTENT( in ) :: tag
        END SUBROUTINE

        SUBROUTINE jStoreCapabilities( self, message )
            IMPORT
            CLASS( MpiIocForAppCallbackType ), INTENT( inout ) :: self
            CLASS( MsgBaseType ), INTENT( inout ) :: message
        END SUBROUTINE
    END INTERFACE
END MODULE
