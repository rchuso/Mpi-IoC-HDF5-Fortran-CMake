! Written by Rand Huso

MODULE Capabilities
    USE, INTRINSIC :: iso_c_binding
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE getmem( freeRam, totalRam ) BIND( C )
            IMPORT :: C_long
            INTEGER( KIND=C_long ) :: freeRam, totalRam
        END SUBROUTINE

        SUBROUTINE getcores( cores ) BIND( C )
            IMPORT :: C_int
            INTEGER( KIND=C_int ) :: cores
        END SUBROUTINE
    END INTERFACE

CONTAINS
    SUBROUTINE C_loadSysInfo( freeRamMB, totalRamMB, nCores, hostname )
        USE :: mpi
        INTEGER, INTENT( out ) :: freeRamMB, totalRamMB, nCores
        CHARACTER( len=MPI_MAX_PROCESSOR_NAME ), INTENT( out ) :: hostname
        INTEGER( KIND=C_long ) :: freeRam
        INTEGER( KIND=C_long ) :: totalRam
        INTEGER( KIND=C_int ) :: cores
        INTEGER :: hostnameLength, iErr

        CALL getmem( freeRam, totalRam )
        CALL getcores( cores )
        CALL MPI_Get_processor_name( hostname, hostnameLength, iErr )

        freeRamMB = INT( freeRam ) ! because of the type conversion
        totalRamMb = INT( totalRam )
        nCores = INT( cores )
    END SUBROUTINE
END MODULE
