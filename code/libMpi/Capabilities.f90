!--------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
MODULE Capabilities
    USE, INTRINSIC :: iso_c_binding
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE getMem( freeRam, totalRam ) BIND( C )
            IMPORT :: C_long
            INTEGER( KIND=C_long ) :: freeRam, totalRam
        END SUBROUTINE

        SUBROUTINE getCores( cores ) BIND( C )
            IMPORT :: C_int
            INTEGER( KIND=C_int ) :: cores
        END SUBROUTINE
    END INTERFACE

CONTAINS
    !----!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
    SUBROUTINE C_loadSysInfo( freeRamMB, totalRamMB, nCores )
        INTEGER, INTENT( OUT ) :: freeRamMB, totalRamMB, nCores
!        CLASS( MsgCapabilitiesType ), INTENT( inout ) :: self
        INTEGER( KIND=C_long ) :: freeRam
        INTEGER( KIND=C_long ) :: totalRam
        INTEGER( KIND=C_int ) :: cores
        CALL getMem( freeRam, totalRam )
!        PRINT *, freeRam, totalRam
        CALL getcores( cores )
!        PRINT *, cores
        freeRamMB = INT( freeRam )
        totalRamMb = INT( totalRam )
        nCores = INT( cores )
    END SUBROUTINE
END MODULE
!--------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
