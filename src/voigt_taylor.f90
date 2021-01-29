SUBROUTINE VOIGT_TAYLOR(DAM,FREC,PHI,PSI)
  !
  ! J M Borrero
  ! Apr 24, 2008
  ! HAO-NCAR for HMI-Stanford
  !
  USE CONS_PARAM
  USE VOIGT_DATA
  USE LINE_PARAM
  !
  IMPLICIT NONE
  REAL(DP),   DIMENSION(NUMW)  :: FREC, PHI, PSI
  REAL(DP)                     :: U, DAM, COEF1, COEF2, COEF3, COEF4, COEF5, COEF6, DELTAU
  INTEGER                      :: I, HERE
  !

  DO I=1,NUMW
     U=FREC(I)
     IF (U.LT.0) U=ABS(U)
     !
!     CALL VOIGT(1,DAM,FREC(I),PHI(I),PSI(I))
     IF (U.GE.29.99D0.OR.U.LT.1D0) CALL VOIGT(1,DAM,FREC(I),PHI(I),PSI(I))
     IF (U.LT.29.99D0.AND.U.GE.1D0) THEN
        ! By RCE: bug commented out in next line. Caused regular 
        ! jumps in area/amplitude of Voigt function with increasing
        ! field strength, every 10 gauss or so.
    !    HERE=ANINT(3001D0*U/30D0)
        HERE = ANINT(U*1D2+1D0)
        DELTAU=U-UU(HERE)
        COEF1=HH(HERE)
        COEF2=FF(HERE)
        COEF3=4D0*DAM*COEF2-2D0*U*COEF1
        COEF4=1D0/SQRT(DPI)-2D0*U*COEF2-DAM*COEF1
        !COEF5=4D0*DAM*COEF4-2D0*COEF1-2D0*U*COEF3
        !COEF6=-2D0*COEF2-2D0*U*COEF4-DAM*COEF3
        PSI(I)=COEF2+COEF4*DELTAU!+COEF6*DELTAU**2D0
        PHI(I)=COEF1+COEF3*DELTAU!+COEF5*DELTAU**2D0
	PSI(I)=2D0*PSI(I)
        IF (FREC(I).LT.0) PSI(I)=-PSI(I)
     ENDIF 
  ENDDO

END SUBROUTINE VOIGT_TAYLOR
!CVSVERSIONINFO "$Id: voigt_taylor.f90,v 1.6 2012/04/10 22:17:51 keiji Exp $"
