! Written by Rand Huso

MODULE MsgBase
    USE :: MpiAssist
    IMPLICIT NONE

    TYPE, ABSTRACT :: MsgBaseType
        INTEGER :: baseId = 0 ! required in all message objects for referencing in MPI_SEND under OpenMPI
    CONTAINS
        PROCEDURE( iGetMpiDatatype ), DEFERRED :: getMpiDatatype
        PROCEDURE( iGetStr ),         DEFERRED :: str
        PROCEDURE( iGetMsgTag ),      DEFERRED :: getMsgTag

        PROCEDURE :: MBT_writeFormatted
        GENERIC :: WRITE( formatted ) => MBT_writeFormatted
    END TYPE

    ABSTRACT INTERFACE
        FUNCTION iGetMpiDatatype( self ) RESULT( response )
            IMPORT :: MsgBaseType
            CLASS( MsgBaseType ) :: self
            INTEGER :: response
        END FUNCTION

        FUNCTION iGetStr( self ) RESULT( identityStr )
            IMPORT :: MsgBaseType
            CLASS( MsgBaseType ), INTENT( in ) :: self
            CHARACTER( len=132 ) :: identityStr
        END FUNCTION

        FUNCTION iGetMsgTag( self ) RESULT( response )
            IMPORT :: MsgBaseType
            CLASS( MsgBaseType ) :: self
            INTEGER :: response
        END FUNCTION
    END INTERFACE
CONTAINS

    SUBROUTINE MBT_writeFormatted( self, unit, iotype, v_list, iostat, iomsg )
        !! iotype has a value as follows:
        !! (https://www.ibm.com/support/knowledgecenter/en/SS2MB5_14.1.0/com.ibm.xlf141.bg.doc/language_ref/uddtioprocedures.jsp)
        !! "LISTDIRECTED" if the parent data transfer statement specified list directed formatting
        !! "NAMELIST" if the parent data transfer statement specified namelist formatting
        !! "DT"
        CLASS( MsgBaseType ), INTENT( in ) :: self
        INTEGER, INTENT( in ) :: unit
        CHARACTER( len=* ), INTENT( in ) :: iotype
        INTEGER, INTENT( in ) :: v_list(:)
        INTEGER, INTENT( out ) :: iostat
        CHARACTER( len=* ), INTENT( inout ) :: iomsg
        WRITE( unit, '(A)', IOSTAT=iostat, IOMSG=iomsg ) trim( self%str() )
        !! borrow ideas from: https://software.intel.com/en-us/node/692243
    END SUBROUTINE
END MODULE
