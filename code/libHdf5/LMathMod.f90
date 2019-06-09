!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-
! here's an example ( after the fact ): http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap06/trigon.html
MODULE LMathMod
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : real32, real64
    IMPLICIT NONE
    PRIVATE
    REAL( KIND=real64 ), PARAMETER, PUBLIC :: LMathPI = 4*ATAN( 1.E0_real64 )
    REAL( KIND=real64 ), PARAMETER, PUBLIC :: LMathPI2 = LMathPI/2
    REAL( KIND=real64 ), PARAMETER, PUBLIC :: LMathDegToRad = LMathPI / 180._real64
    REAL( KIND=real32 ), PARAMETER, PUBLIC :: LMathPI32 = 4*ATAN( 1.E0_real32 )
    REAL( KIND=real32 ), PARAMETER, PUBLIC :: LMathDegToRad32 = LMathPI32 / 180._real32

    INTERFACE toRadians
        MODULE PROCEDURE toRadians32, toRadians64
    END INTERFACE

    INTERFACE toDegrees
        MODULE PROCEDURE toDegrees32, toDegrees64
    END INTERFACE

    PUBLIC SIND
    INTERFACE SIND
        MODULE PROCEDURE SIND32, SIND64
    END INTERFACE

    PUBLIC COSD
    INTERFACE COSD
        MODULE PROCEDURE COSD32, COSD64
    END INTERFACE

    PUBLIC TAND
    INTERFACE TAND
        MODULE PROCEDURE TAND32, TAND64
    END INTERFACE

    PUBLIC ATAND
    INTERFACE ATAND
        MODULE PROCEDURE ATAND32, ATAND64
    END INTERFACE

CONTAINS

    ELEMENTAL REAL( KIND=real64 ) FUNCTION toRadians64( degrees ) ! ELEMENTAL implies PURE
        REAL( KIND=real64 ), INTENT( in ) :: degrees
        toRadians64 = degrees * LMathDegToRad
    END FUNCTION

    ELEMENTAL REAL( KIND=real32 ) FUNCTION toRadians32( degrees )
        REAL( KIND=real32 ), INTENT( in ) :: degrees
        toRadians32 = degrees * LMathDegToRad32
    END FUNCTION

    ELEMENTAL REAL( KIND=real64 ) FUNCTION toDegrees64( radians )
        REAL( KIND=real64 ), INTENT( in ) :: radians
        toDegrees64 = radians / LMathDegToRad
    END FUNCTION

    ELEMENTAL REAL( KIND=real32 ) FUNCTION toDegrees32( radians )
        REAL( KIND=real32 ), INTENT( in ) :: radians
        toDegrees32 = radians / LMathDegToRad32
    END FUNCTION

    ELEMENTAL REAL( KIND=real64 ) FUNCTION SIND64( degrees )
        IMPLICIT NONE
        REAL( KIND=real64 ), INTENT( in ) :: degrees
        SIND64 = sin( toRadians( degrees ) )
    END FUNCTION
!
    ELEMENTAL REAL( KIND=real32 ) FUNCTION SIND32( degrees )
        IMPLICIT NONE
        REAL( KIND=real32 ), INTENT( in ) :: degrees
        SIND32 = sin( toRadians( degrees ) )
    END FUNCTION
!
    ELEMENTAL REAL( KIND=real64 ) FUNCTION COSD64( degrees )
        IMPLICIT NONE
        REAL( KIND=real64 ), INTENT( in ) :: degrees
        COSD64 = cos( toRadians( degrees ) )
    END FUNCTION
!
    ELEMENTAL REAL( KIND=real32 ) FUNCTION COSD32( degrees )
        IMPLICIT NONE
        REAL( KIND=real32 ), INTENT( in ) :: degrees
        COSD32 = cos( toRadians( degrees ) )
    END FUNCTION
!
    ELEMENTAL REAL( KIND=real64 ) FUNCTION TAND64( degrees )
        IMPLICIT NONE
        REAL( KIND=real64 ), INTENT( in ) :: degrees
        TAND64 = tan( toRadians( degrees ) )
    END FUNCTION
!
    ELEMENTAL REAL( KIND=real32 ) FUNCTION TAND32( degrees )
        IMPLICIT NONE
        REAL( KIND=real32 ), INTENT( in ) :: degrees
        TAND32 = tan( toRadians( degrees ) )
    END FUNCTION
!
    ELEMENTAL REAL( KIND=real64 ) FUNCTION ATAND64( value )
        IMPLICIT NONE
        REAL( KIND=real64 ), INTENT( in ) :: value
        ATAND64 = toDegrees( atan( value ) )
    END FUNCTION
!
    ELEMENTAL REAL( KIND=real32 ) FUNCTION ATAND32( value )
        IMPLICIT NONE
        REAL( KIND=real32 ), INTENT( in ) :: value
        ATAND32 = toDegrees( atan( value ) )
    END FUNCTION
END MODULE
!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-
