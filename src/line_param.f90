MODULE LINE_PARAM
  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  !
  USE CONS_PARAM
  !
  REAL(DP)                                   :: LANDA0, SHIFT, STEPW
  REAL(DP),       DIMENSION(4)               :: NOISE
  INTEGER                                    :: NUMW
  REAL(DP),         ALLOCATABLE              :: WAVE(:)
  !
END MODULE LINE_PARAM
!CVSVERSIONINFO "$Id: line_param.f90,v 1.5 2012/04/10 22:17:18 keiji Exp $"
