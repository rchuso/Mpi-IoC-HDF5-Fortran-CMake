! Written by Rand Huso
! This is an example of writing an MPI application using basic Inversion of
! Control (IoC) software pattern. The AppBase is in charge of the execution, and supplies objects (based on
! the MsgBase class) to this application when they are received at this node.
! Example application - Extend the AppBase, implement the three required methods.

MODULE MyApp
    USE :: MpiIocForAppBase
    USE :: MsgCapabilities
    USE :: MsgTest
    IMPLICIT NONE
    TYPE, EXTENDS( MpiIocForAppBaseType ) :: MyAppType
    CONTAINS
        PROCEDURE :: receiveMsg    => MA_rxMessage ! called when a message arrives
        PROCEDURE :: loadUserTypes => MA_loadUserTypes ! called by the layer to register your message types
        PROCEDURE :: startApp      => MA_startApp ! called when the application is ready to start
    END TYPE

CONTAINS

    ! this is called on rank=0 by the layer after all the nodes have reported their "capabilities". Use this
    ! to start the application doing whatever it has to do. In this case we show the capabilities, broadcast
    ! the test message, and wait.
    SUBROUTINE MA_startApp( self )
        CLASS( MyAppType ), INTENT( inout ) :: self
        TYPE( MsgTestType ) :: testMessage
        testMessage = MsgTestType()
        CALL self%showCapabilities()
        CALL self%bcast( testMessage )
    END SUBROUTINE

    ! just to register this single message type to the layer - which it keeps (a copy of) in its storage tree
    ! Whatever messages are registered here should also be listed in the SELECT TYPE in the next method.
    SUBROUTINE MA_loadUserTypes( self )
        CLASS( MyAppType ), INTENT( inout ) :: self
        TYPE( MsgTestType ) :: msgTest ! &c with all the other messages we need
        CALL self%loadType( msgTest )  ! &c with all the other messages we need
    END SUBROUTINE

    ! when a message is received, it comes here. Check to see if it's one of the test messages, and if it is,
    ! and if this is not rank=0, send it back to 0 ("sourceNode", otherwise, if it's rank=0, stop the layer.
    SUBROUTINE MA_rxMessage( self, message, sourceNode, tag )
        CLASS( MyAppType ), INTENT( inout ) :: self
        CLASS( MsgBaseType ), INTENT( inout ) :: message ! The message is owned by the layer - do not delete
        INTEGER, INTENT( in ) :: sourceNode
        INTEGER, INTENT( in ) :: tag
        WRITE( *, "(I0,':',I0,' rx:',A,' from:',I0,' tag:',I0)" ) &
                self%rank(), self%size(), trim( message%str()), sourceNode, tag
        SELECT TYPE( message )
            TYPE is( MsgTestType )
                IF( 0 /= self%rank()) THEN
                    CALL self%send( message, sourceNode )
                ELSE
                    CALL self%stop()
                END IF
            CLASS DEFAULT
                WRITE( *, * ) 'MA_rxMessage - default unknown message'
        END SELECT
    END SUBROUTINE
END MODULE

! This test application tests the two libraries, separately.
! The first (MPI) test may include licensing or other requirements, which may be checked between the initMpiLayer
! and the start methods.
! The second part just creates a compressed extensible HDF5 output file.

PROGRAM TestMpiIocHdf5
    USE :: MyApp
    USE :: HDMod
    IMPLICIT NONE

    ! The block is just to see that the memory is free'd when testing with valgrind.
    ! Start the application, and it will return when the code above calls the "stop" method of the layer.

    CHARACTER( LEN=* ), PARAMETER :: fileName = 'hyper.h5'
    INTEGER :: myRank

    ! MPI section
    BLOCK
        TYPE( MyAppType ), TARGET :: myApp
        CLASS( MpiIocForAppCallbackType ), POINTER :: myAppPtr
        myAppPtr => myApp ! the layer needs the pointer for its "callback" processing. The layer implements
                          ! the AppCallback type.
        CALL myApp%initMpiLayer( myAppPtr ) ! initialization is done before starting the layer - this allows
                                            ! this program to check for licnsing or similar based on the rank.
        CALL myApp%start()
        ! the start() ^^^^^ routine does not return until the application is finished and returns control

        myRank = myApp%rank()
    END BLOCK

    ! HDF5 section
    IF( 0 == myRank ) THEN
        BLOCK ! only showing this for one rank
            INTEGER, PARAMETER :: cols1 = 2
            INTEGER, PARAMETER :: rows1 = 2
            INTEGER, PARAMETER :: cols2 = 3
            INTEGER, PARAMETER :: rows2 = 1
            INTEGER :: row, col
            TYPE( HDType ) :: hObj

            hObj = HDType( fileName )

            BLOCK
                CHARACTER( LEN=* ), PARAMETER :: dSetName = '/Barney/Betty'
                REAL( kind=real64 ), DIMENSION( cols1, rows1 ) :: buf ! reverse-odometer
                DO row=1, rows1
                    DO col=1, cols1
                        buf( col, row ) = row*2.718281828 +col*10
                    END DO
                END DO
                CALL hObj%write( dSetName, buf )
            END BLOCK
            BLOCK
                CHARACTER( LEN=* ), PARAMETER :: dSetName = '/Fred/Wilma'
                INTEGER( kind=int32 ), DIMENSION( cols2, rows2 ) :: buf
                DO row=1, rows2
                    DO col=1, cols2
                        buf( col, row ) = row +col*10
                    END DO
                END DO
                CALL hObj%write( dSetName, buf, 5 ) ! force write to item 5
            END BLOCK
        END BLOCK
    END IF

END PROGRAM
