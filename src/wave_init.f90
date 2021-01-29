SUBROUTINE WAVE_INIT (LAMBDA_MIN, DELTA_LAMBDA, NUM_LAMBDA)
  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  USE CONS_PARAM
  USE LINE_PARAM
  IMPLICIT NONE
  REAL(DP), INTENT(IN) :: LAMBDA_MIN, DELTA_LAMBDA
  INTEGER,  INTENT(IN) :: NUM_LAMBDA
  INTEGER              :: I
  !
  NUMW=NUM_LAMBDA
  STEPW=DELTA_LAMBDA
  !
  DO I=1,NUMW
     WAVE(I)=LAMBDA_MIN+DBLE(I-1)*STEPW
  ENDDO
  !
END SUBROUTINE WAVE_INIT
!CVSVERSIONINFO "$Id: wave_init.f90,v 1.5 2012/04/10 22:17:56 keiji Exp $"