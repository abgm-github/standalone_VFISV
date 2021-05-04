PROGRAM VFISV

  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  !
  ! -----------------------------------------------------------------------------
  !
  ! VFISV
  ! Date: October 2010
  !
  ! September 2020
  ! ABGM modified ASD program
  ! This program was created by Ana Belen Grinon Marin in order to invert the
  ! Stokes parameters observed by HMI@SDO. 
  !
  !  It is based on the Alberto Sainz Dalda program, which in turn was based on
  !  code developed by Borrero et al. 2011 and Centeno et al. 2014
  ! -----------------------------------------------------------------------------



  USE FILT_PARAM
  USE INV_UTILS
  USE FORWARD

  INTEGER    :: NUM_ITER, NUM_LAMBDA
  REAL(8)    :: LAMBDA_MIN
  INTEGER    :: NUM_LAMBDA_FILTER, NUM_TUNNING, NUM_LAMBDA_SYNTH
  REAL(8)    :: LAMBDA_MIN_SYNTH, SVD_TOLERANCE, CHI2_STOPI
  REAL(8)    :: POLARIZATION_THRESHOLD, PERCENTAGE_JUMP, LAMBDA_0, LAMBDA_B
  REAL(8)    :: DELTA_LAMBDA, LYOTFWHM, WNARROW, WSPACING, INTENSITY_THRESHOLD 
  REAL(8)    :: NOISE_LEVELFI, NOISE_LEVELFQ, NOISE_LEVELFU, NOISE_LEVELFV
  INTEGER    :: CONTINUUM
  REAL(8), DIMENSION(4)    :: WEIGHTS, NOISE_LEVEL
  REAL(8)                  :: IVALMAX, IVALAVE, CONT, TMPVAL
  INTEGER                  :: I 
  !-----------------------
  REAL(8), DIMENSION(:), ALLOCATABLE           :: OBS, SCAT, SCAT_LONG
  !!REAL(8), DIMENSION(:), ALLOCATABLE           :: OBS, SCAT_LONG, INTEG_SCAT
  !!REAL(8), DIMENSION(:,:), ALLOCATABLE         :: SCAT
  REAL(8), DIMENSION(:), ALLOCATABLE           :: GUESS, RES
  REAL(8), DIMENSION(:), ALLOCATABLE           :: ERR
  INTEGER(kind=2), DIMENSION(:), ALLOCATABLE   :: LIST_FREE_PARAMS
  REAL(DP), DIMENSION(:,:), ALLOCATABLE        :: FILTERS
  REAL(DP), DIMENSION(:,:,:), ALLOCATABLE      :: DSYN
  REAL(DP), DIMENSION(:), ALLOCATABLE          :: INTEG_FILTERS_NEW

  !-----------------------abgm
  REAL(8), DIMENSION(:), ALLOCATABLE           :: SYN
  INTEGER(kind=2) :: NTHINGS
  INTEGER(kind=2) :: MODE
  INTEGER(kind=2) :: NUM_LAMBDAS
  INTEGER(kind=2) :: NDIMS1
  INTEGER(kind=2) :: NDIMS2
  INTEGER(kind=2) :: NDIMS3
  INTEGER(kind=2) :: NDIMS4
  INTEGER(kind=2) :: NDIMS5
  INTEGER(kind=2) :: NDIMS6
  INTEGER(kind=2) :: NDIMS7
  INTEGER(kind=2) :: NDIMS8
  INTEGER(kind=2) :: NDIMS9

  INTEGER(kind=2), DIMENSION(2) :: DIMS1
  INTEGER(kind=2), DIMENSION(2) :: DIMS2
  INTEGER(kind=2), DIMENSION(2) :: DIMS3
  INTEGER(kind=2), DIMENSION(1) :: DIMS4
  INTEGER(kind=2), DIMENSION(1) :: DIMS5
  INTEGER(kind=2), DIMENSION(1) :: DIMS6
  INTEGER(kind=2), DIMENSION(2) :: DIMS7
  INTEGER(kind=2), DIMENSION(1) :: DIMS8
  INTEGER(kind=2), DIMENSION(1) :: DIMS9

  REAL(8), DIMENSION(:), ALLOCATABLE   :: POLARIZATION
  !-----------------------abgmm


!! Default parameters

  NUM_ITERATIONS = 200              !! "number of iterations(default: 30)"
!!  NUM_ITERATIONS = 10              !! "number of iterations(default: 30)"
  LAMBDA_MIN = -1998.0D0              !! "Intensity threshold (default: -432)"
  NUM_TUNNING = 6                   !! "Number of ??(default: 6)"
  NUM_LAMBDA_SYNTH = 49             !! "Number of synthetic filters (default: 6)"
  LAMBDA_MIN_SYNTH = -648.0D0         !! "Intensity threshold (default: -432)"
  SVD_TOLERANCE = 1.0D-32           !! "svd tolerance (default: 1.0e-32)"
  CHI2_STOPI = 1.0D-15              !! "chisq-stop (default: 1.0e-6)"
  POLARIZATION_THRESHOLD = 1.0D-2   !! "polarization threshold (default: 0.01)"
  PERCENTAGE_JUMP = 10.0D0            !! "Percentage Jump (default: 10%)"
  LAMBDA_0 = 6173.3433D0              !! "Wavelength(default:6173.3433 Angstrom )"
  LAMBDA_B = 0.044475775D0            !! "FWHM?? (default:0.044475775)"
  DELTA_LAMBDA = 27.0D0               !! "Delta Lambda(default: 27.0)"
  LYOTFWHM = 424.0D0                  !! "Lyot filter FWHM (default: 424.0)"
  WNARROW = 172.0D0                   !! "FSR (full spectral range) of the Narrow-Band Michelson"
  WSPACING = 69.0D0                   !! "wavelength spacing between the HMI filters"
  INTENSITY_THRESHOLD = 1.0D2       !! "Intensity threshold (default: 0.8)"
  NOISE_LEVELFI = 0.118D0             !! "Noise Sigma factor for I"
  NOISE_LEVELFQ = 0.204D0             !! "Noise Sigma factor for Q"
  NOISE_LEVELFU = 0.204D0             !! "Noise Sigma factor for U"
  NOISE_LEVELFV = 0.204D0             !! "Noise Sigma factor for V"
  CONTINUUM = 0                     !! "Intensity threshold (default: 0)"

  WEIGHTS(1) = 1.0D0
  WEIGHTS(2) = 5.0D0
  WEIGHTS(3) = 5.0D0
  WEIGHTS(4) = 3.5D0

!!!  WEIGHTS(1) = 1.0D0
!!!  WEIGHTS(2) = 0.0D0
!!!  WEIGHTS(3) = 0.0D0
!!!  WEIGHTS(4) = 0.0D0

!!!***********************************


  ALLOCATE(ERR(12))

  OPEN(UNIT=1, FILE='input.bin', FORM='UNFORMATTED' &
       , ACCESS='STREAM', CONVERT='LITTLE_ENDIAN')
    READ(1) NTHINGS
!!!print*,NTHINGS

    READ(1) MODE
!!!print*,'Mode: ',MODE

    READ(1) NDIMS1
    READ(1) DIMS1(:)
    ALLOCATE(FILTERS(DIMS1(1),DIMS1(2)))
    FILTERS(:,:) = 0.0D0
    READ(1) FILTERS(:,:)
!!!print*,NDIMS1
!!!print*,DIMS1(:)
!!!print*,SIZE(FILTERS(:,:))

    READ(1) NDIMS2
    READ(1) DIMS2(:)
    ALLOCATE(SCAT_LONG(PRODUCT(DIMS2)))
    SCAT_LONG(:) = 0.0D0
    READ(1) SCAT_LONG(:)
    !!!ALLOCATE(SCAT(PRODUCT(DIMS2)))
    !!!SCAT(:) = 0.0D0
    !!!READ(1) SCAT(:)
!!!print*,NDIMS2
!!!print*,DIMS2(:)
!!!print*,'Scat: ',SCAT_LONG(:)

    READ(1) NDIMS3
    READ(1) DIMS3(:)
    ALLOCATE(OBS(PRODUCT(DIMS3)))
    OBS(:) = 0.0D0
    READ(1) OBS(:)
!!!print*,NDIMS3
!!!print*,DIMS3(:)
!!!print*,'Obs: ',OBS(:)

    READ(1) NDIMS4
    READ(1) DIMS4
    ALLOCATE(GUESS(DIMS4(1)))
    GUESS(:) = 0.0D0
    READ(1) GUESS(:)
!!!print*,NDIMS4
!!!print*,DIMS4
!!!print*,'Guess: ',GUESS(:)

    READ(1) NDIMS5
    READ(1) DIMS5
    ALLOCATE(LIST_FREE_PARAMS(DIMS5(1)))
    LIST_FREE_PARAMS(:) = 0
    READ(1) LIST_FREE_PARAMS(:)
!!!print*,NDIMS5
!!!print*,DIMS5
!!!print*,'List Free Params: ',LIST_FREE_PARAMS(:)

    READ(1) NDIMS6
    READ(1) DIMS6
    ALLOCATE(POLARIZATION(DIMS6(1)))
    POLARIZATION(:) = 0.0D0
    READ(1) POLARIZATION(:)
!!!print*,NDIMS6
!!!print*,DIMS6
!!!print*,'Polarization: ',POLARIZATION(:)

  CLOSE(1)


  NUM_LAMBDA = DIMS1(1)          !149
  NUM_LAMBDA_FILTER = DIMS1(2)   !5,6,8,10


!------------------------------


!!!!!  ALLOCATE(SCAT(NUM_LAMBDA_SYNTH,NUM_LAMBDA_FILTER))
!!!!!  ALLOCATE(INTEG_SCAT(NUM_LAMBDA_FILTER))
!!!!!
!!!!!!!!  NPOINTS = (DIMS2(0) - NUM_LAMBDA_SYNTH)/2
!!!!!!!!
!!!!!!!!  SCAT(:,:) = SCAT_LONG(NPOINTS+1:NUM_LAMBDA_SYNTH+NPOINTS,:)
!!!!!!!!  DO I = 1, DIMS2(1)
!!!!!!!!     INTEG_SCAT(I) = SUM(SCAT_LONG(1:NPOINTS,I)) +  &
!!!!!!!!          SUM(SCAT_LONG(DIMS2(0)-NPOINTS+1:,I))
!!!!!!!!  ENDDO
!!!!!print*,'+++++++++++++++++++++'
!!!!!print*,'size scat long',size(scat_long)
!!!!!print*,'size scat',size(scat)
!!!!!print*,'size integ scat',size(integ_scat)
!!!!!print*,'FILTERS_LONG',NUMW_LONG, NBINS
!!!!!print*,'FILTERS',NUMW,NBINS
!!!!!print*,'INTEG_FILTERS',NBINS
!!!!!print*,'NPOINTS',NPOINTS
!!!!!print*,'pol',POLARIZATION
!!!!!stop


!------------------------------


  !Only if mode input is 1, do inversion
  IF (MODE.NE.1) MODE = 0

  ALLOCATE(RES(DIMS5(1)))
  RES(:) = 0.0d0

  IF (DIMS1(1).NE.NUM_LAMBDA) THEN
    PRINT*,"*******************"
    PRINT*,"First dimension of filter is different to the number of lambdas...Ending.."
    PRINT*,"*******************"
    STOP
  ENDIF

  IF (DIMS1(2).NE.NUM_LAMBDA_FILTER) THEN
    PRINT*,"*******************"
    PRINT*,"Second dimension of filter is different to the number of values of the filter...Ending.."
    PRINT*,"*******************"
    STOP
  ENDIF

!!!***********************************

  NUMW = NUM_LAMBDA_SYNTH
  CALL VFISVALLOC(NUM_LAMBDA_FILTER,NUM_LAMBDA,NUM_LAMBDA_SYNTH)

  !! ME inversion initialization 
  CALL INV_INIT(NUM_ITERATIONS,SVD_TOLERANCE,CHI2_STOP,POLARIZATION_THRESHOLD,INTENSITY_THRESHOLD)
  CALL FREE_INIT(INT(LIST_FREE_PARAMS))

  !! Changed index of list_free_params to refer to Damping*/
  IF (LIST_FREE_PARAMS(4).EQ.0) CALL VOIGT_INIT()

  IVALAVE = SUM(OBS(1:NUM_LAMBDA_FILTER)) / DBLE(NUM_LAMBDA_FILTER)
  IVALMAX = MAXVAL(OBS(1:NUM_LAMBDA_FILTER))
  CONT = IVALMAX

  IVALAVE = DSQRT(IVALAVE)
  IVALMAX = DSQRT(IVALMAX)
  NOISE_LEVEL(1) = NOISE_LEVELFI * IVALAVE
  NOISE_LEVEL(2) = NOISE_LEVELFQ * IVALAVE
  NOISE_LEVEL(3) = NOISE_LEVELFU * IVALAVE
  NOISE_LEVEL(4) = NOISE_LEVELFV * IVALAVE

  CALL LIM_INIT(CONT)
  CALL LINE_INIT(LAMBDA_0,LAMBDA_B,NOISE_LEVEL)
  CALL WAVE_INIT(LAMBDA_MIN_SYNTH,DELTA_LAMBDA,NUM_LAMBDA_SYNTH)
  CALL FILT_INIT(NUM_LAMBDA_FILTER,WSPACING,NUM_LAMBDA)

  !!! Test
  IF (DIMS3(1).NE.DIMS2(1)) THEN
    IF (DIMS2(1).NE.NUMW) THEN
      PRINT*,'Scat no tiene las dimesiones de obs o de numw'
      STOP
    ENDIF
  
    ALLOCATE(INTEG_FILTERS_NEW(NBINS))
    INTEG_FILTERS_NEW(:) = 0.0D0 
    NPOINTS = (NUMW_LONG - NUMW)/2
    DO I = 1, NBINS
       INTEG_FILTERS_NEW(I) = SUM(FILTERS(1:NPOINTS,I)) +  &
            SUM(FILTERS(NUMW_LONG-NPOINTS+1:,I))
    ENDDO
    IF (NPOINTS .EQ. 0) THEN
       INTEG_FILTERS_NEW(:) = 0D0
    ENDIF

    ALLOCATE(SCAT(PRODUCT(DIMS3)))
    DO K=1,4
       DO J=1,NBINS
          SCAT((K-1)*NBINS+J)=SUM(FILTERS(NPOINTS+1:NUMW_LONG-NPOINTS,J)*SCAT_LONG((K-1)* &
                    dims2(1)+1:k*dims2(1))) / (1.-INTEG_FILTERS_NEW(J))
       ENDDO
    ENDDO
  ELSE 
    
    ALLOCATE(SCAT(PRODUCT(DIMS2)))
    SCAT(:) = SCAT_LONG(:)
      ALLOCATE(INTEG_FILTERS_NEW(NBINS))
      INTEG_FILTERS_NEW(:) = 0.0D0 
      NPOINTS = (NUMW_LONG - NUMW)/2
      DO I = 1, NBINS
         INTEG_FILTERS_NEW(I) = SUM(FILTERS(1:NPOINTS,I)) +  &
              SUM(FILTERS(NUMW_LONG-NPOINTS+1:,I))
      ENDDO
      IF (NPOINTS .EQ. 0) THEN
         INTEG_FILTERS_NEW(:) = 0D0
      ENDIF
  ENDIF  

  IF (MODE.EQ.1) THEN 
    CALL INVERT(OBS, SCAT, GUESS, RES, ERR, FILTERS, ICONVERGE_FLAG, WEIGHTS, &
                POLARIZATION)
  ELSE 
    PRINT*,'++++++++++++' 
    PRINT*,'Synthesis...'
    PRINT*,'++++++++++++' 
    RES(:) = GUESS(:)
  ENDIF


  ALLOCATE(SYN(PRODUCT(DIMS3)))
  SYN(:) = 0.0D0
  
  ALLOCATE(DSYN(SIZE(RES),NBINS,4))
  DSYN(:,:,:) = 0.0D0

  RES(3) = RES(3) - 90D0
  CALL SYNTHESIS2(RES,SCAT,.FALSE.,SYN,DSYN, FILTERS(NPOINTS+1:NUMW+NPOINTS,:), INTEG_FILTERS_NEW)
  RES(3) = RES(3) + 90D0



  IF (MODE.EQ.0) THEN 
    OBS(1:NUM_LAMBDA_FILTER) = (SYN(1:NUM_LAMBDA_FILTER) - (1-RES(10))*SCAT(1:NUM_LAMBDA_FILTER)) / RES(10)
  ENDIF

  !abgm
  NTHINGS = 5
  DIMS7 = DIMS3
  NDIMS7 = SIZE(DIMS7)
  DIMS8 = SHAPE(RES)
  NDIMS8 = SIZE(DIMS8)
  DIMS9 = SHAPE(ERR)
  NDIMS9 = SIZE(DIMS9)

  OPEN (UNIT=97, FILE = "output.bin", ACCESS='STREAM' &
                        , FORM="unformatted",CONVERT='LITTLE_ENDIAN')
  WRITE(97) NTHINGS
  WRITE(97) NDIMS7
  WRITE(97) DIMS7(:)
  WRITE(97) SYN
  WRITE(97) NDIMS2
  WRITE(97) DIMS3
  WRITE(97) SCAT
  WRITE(97) NDIMS3
  WRITE(97) DIMS3(:)
  WRITE(97) OBS
  WRITE(97) NDIMS8
  WRITE(97) DIMS8
  WRITE(97) RES
  WRITE(97) NDIMS9
  WRITE(97) DIMS9
  WRITE(97) ERR
  CLOSE(97)
  !abgm
!!print*,'Finishing...'



  CALL FREE_MEMORY()

  IF (ALLOCATED(SCAT)) DEALLOCATE(SCAT)
  IF (ALLOCATED(OBS)) DEALLOCATE(OBS)
  IF (ALLOCATED(FILTERS)) DEALLOCATE(FILTERS)
  IF (ALLOCATED(GUESS)) DEALLOCATE(GUESS)
  IF (ALLOCATED(LIST_FREE_PARAMS)) DEALLOCATE(LIST_FREE_PARAMS)
  IF (ALLOCATED(ERR)) DEALLOCATE(ERR)
  IF (ALLOCATED(RES)) DEALLOCATE(RES)
  IF (ALLOCATED(SYN)) DEALLOCATE(SYN)

END PROGRAM VFISV
