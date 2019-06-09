!--------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
! Display messages that may be picked-up by the UI for proper display
! This includes progress messages, ERROR, WARNING, INFO, and DEBUG messages as well as simple text lines

MODULE SimpleMessagesMod
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : Output_Unit, Input_Unit
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_int, c_float
    IMPLICIT NONE

    ENUM, BIND( C ) ! :: SM_LEVEL - see https://rosettacode.org/wiki/Enumerations#Fortran about this
        ENUMERATOR :: SM_TYPE = 0
        ENUMERATOR :: SM_NONE = 0
        ENUMERATOR :: SM_ERROR
        ENUMERATOR :: SM_WARNING
        ENUMERATOR :: SM_INFO
        ENUMERATOR :: SM_DEBUG
    END ENUM

    INTEGER( kind( SM_TYPE )) :: SM_CurrentLevel = SM_INFO
#ifdef NotWorkingYet
    LOGICAL :: SM_showBacktrace = .True.
#endif

    CHARACTER( LEN=* ), PARAMETER, PRIVATE :: SM_PREFIX_PERCENT = '%-P-'
    CHARACTER( LEN=* ), PARAMETER, PRIVATE :: SM_PREFIX_ERROR   = '%-E-'
    CHARACTER( LEN=* ), PARAMETER, PRIVATE :: SM_PREFIX_WARNING = '%-W-'
    CHARACTER( LEN=* ), PARAMETER, PRIVATE :: SM_PREFIX_INFO    = '%-I-'
    CHARACTER( LEN=* ), PARAMETER, PRIVATE :: SM_PREFIX_DEBUG   = '%-D-'

CONTAINS

    !----!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
    SUBROUTINE SM_logMessage( msgLevel, msg )
        INTEGER( kind( SM_TYPE )), INTENT( IN ), VALUE :: msgLevel
        CHARACTER( LEN=* ), INTENT( IN ) :: msg
        IF( msgLevel <= SM_CurrentLevel ) THEN
            SELECT CASE( msgLevel )
                CASE( SM_NONE );    WRITE( Output_Unit, '(A)' ) msg
                CASE( SM_DEBUG );   WRITE( Output_Unit, '(A, 1X, A)' ) SM_PREFIX_DEBUG, msg
                CASE( SM_INFO );    WRITE( Output_Unit, '(A, 1X, A)' ) SM_PREFIX_INFO, msg
                CASE( SM_WARNING ); WRITE( Output_Unit, '(A, 1X, A)' ) SM_PREFIX_WARNING, msg
                CASE( SM_ERROR );   WRITE( Output_Unit, '(A, 1X, A)' ) SM_PREFIX_ERROR, msg
            END SELECT
            FLUSH( Output_Unit )
#ifdef NotWorkingYet
            PRINT *, 'XXXXXXXXXXXXXXXXXXXX Checking for backtrace() XXXXXXXXXXXXXXXXXXXXXXX'
            IF( SM_showBacktrace .and. ( msgLevel == SM_ERROR )) THEN
                WRITE( Output_Unit, '(A)' ) '|--------+---------+---------+---------+---------+---------+---------+---------+-|'
                WRITE( Output_Unit, '(A)' ) 'SM_BACKTRACE:'
                CALL BACKTRACE()
                WRITE( Output_Unit, '(A)' ) '|--------+---------+---------+---------+---------+---------+---------+---------+-|'
            END IF
#endif
        END IF
    END SUBROUTINE

    !----!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
    SUBROUTINE SM_showProgress( percentCompleted, msg )
        IMPLICIT NONE
        INTEGER :: percentCompleted
        CHARACTER( LEN=* ), INTENT( IN ), OPTIONAL :: msg
        INTEGER, SAVE :: previousPercentage = -1
        IF( previousPercentage < percentCompleted ) THEN
            previousPercentage = percentCompleted
            IF( present( msg )) THEN
                WRITE( Output_Unit, '(A, " [", I0, "] ",  A)' ) SM_PREFIX_PERCENT, percentCompleted, msg
            else
                WRITE( Output_Unit, '(A, " [", I0, "] ",  A)' ) SM_PREFIX_PERCENT, percentCompleted
            END IF
            FLUSH(Output_Unit)
        END IF
    END SUBROUTINE
END MODULE
!--------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
