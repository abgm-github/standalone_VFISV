;;/*-------------------------------------------------------------------------------------------------------*/
;;/* Function to compute the HMI-filter profiles                                                           */
;;/*                                                                                                       */
;;/* OUTPUT:                                                                                               */
;;/* filters are the filter profiles, in the format double filters[Num_lambda_filter][Num_lambda]          */
;;/*                                                                                                       */
;;/* INPUTS:                                                                                               */
;;/* Num_lambda is the number of wavelengths                                                               */
;;/* Lambda_Min is the minimum value of the wavelength                                                     */
;;/* Delta_Lambda is the wavelength resolution (in milliAngstroms)                                         */
;;/* Num_lambda_filter is the number of filters/wavelengths used                                           */
;;/* frontwindowd is the spatially-averaged front window transmission profile                              */
;;/* wavelengthd is the wavelength grid for the front window profile                                       */
;;/* nfront is the number of points of the window profile                                                  */
;;/* blockerd is the spatially-averaged blocker filter transmission profile                                */
;;/* wavelengthbd is the wavelength grid for the blocker filter profile                                    */
;;/* nblocker is the number of points of the blocker filter profile                                        */
;;/* centerblocker is the location of the center of the blocker filter                                     */
;;/* phaseNT are the phases of the 4 non-tunable elements                                                  */
;;/* phaseT are the phases of the 3 tunable elements                                                       */
;;/* contrastNT are the contrasts of the 4 non-tunable elements                                            */
;;/* contrastT are the contrasts of the 3 tunable elements                                                 */
;;/* FSR are the full spectral ranges of the 7 HMI optical-filter elements                                 */
;;/* lineref is the reference Fe I line profile at disk center                                             */
;;/* wavelengthref is the wavelength grid for the Fe I line profile                                        */
;;/* referencenlam is the number of wavelengths in the reference Fe I profile                              */
;;/* distance is the angular distance from disk center for the pixel studied (distance=cos(theta): 1 at    */
;;/* disk center, 0 at the limb)                                                                           */
;;/* HCME1 is the hollow-core motor position to center the profile on the Fe I line for the Lyot element E1*/
;;/* HCMWB is for the wide-band Michelson                                                                  */
;;/* HCMNB is for the NB Michelson                                                                         */
;;/* HCMPOL is for the tuning polarizer                                                                    */
;;/*-------------------------------------------------------------------------------------------------------*/

FUNCTION lininterp1f,xv,yv,x,ydefault,m,minterp

nrowsinterp = minterp
nrowsdata = m
yinterp = DBLARR(nrowsinterp)
FOR i=0,nrowsinterp-1 DO BEGIN
  IF (x[i] LT xv[0]) OR (x[i] GT xv[nrowsdata-1]) THEN BEGIN
    yinterp[i] = ydefault
  ENDIF ELSE BEGIN
    FOR j=1,nrowsdata-1 DO BEGIN
       IF x[i] LE xv[j] THEN BEGIN
         yinterp[i] = (x[i]-xv[j-1]) / (xv[j]-xv[j-1]) * (yv[j]-yv[j-1]) + yv[j-1]
         BREAK
       ENDIF
     ENDFOR
  ENDELSE
ENDFOR

RETURN,yinterp

END




FUNCTION vfisv_filter,NUM_LAMBDA_FILTER,NUM_LAMBDA,LAMBDA_MIN,DELTA_LAMBDA, $
                      contrastNTi,contrastTi,phaseNTi,phaseTi,nfront,hcme1,hcmwb,hcmnb,hcmpol

nblocker = 201
centerblocker = 2.d0

lam0 = 6173.3433d0 ;WAVELENGTH AT REST OF THE SOLAR FeI LINE (THIS IS THE REFERENCE WAVELENGTH THAT IS USED TO CALCULATE THE PHASES OF THE TUNABLE ELEMENTS)
ydefault = 1.d0 ;default value for the intensity of the solar line OUTSIDE a small range around 6173.3433 A (SHOULD BE 1)
ydefault2 = 0.d0 ;default value for the transmittance of the blocker filter and front window outside the range considered (SHOULD BE 0)

;WAVELENGTH GRID
;By RCE April 22, 2010: divide Lambda_Min by 1d3 to put it in Angstroems
wavelength = DBLARR(num_lambda)
FOR i=0,num_lambda-1 DO wavelength[i] = lambda_min/1000.0d0 + i*delta_lambda/1000.0d0 ;wavelength is the wavelength grid in Angstroms

values = take_values_wd_fd(nfront)

wavelengthd = values.wavelengthd
frontwindowd = values.frontwindowd
wavelengthdtmp = DBLARR(nfront)
frontwindowdtmp = DBLARR(nfront)
FOR i=0,nfront-1 DO BEGIN
  wavelengthdtmp[i] = wavelengthd[i]*10.0d0 - lam0
  frontwindowdtmp[i] = frontwindowd[i]/100.0d0
ENDFOR

wavelengthbd = values.wavelengthbd
blockerbd = values.blockerbd
wavelengthbdtmp = DBLARR(nblocker)
blockerbdtmp = DBLARR(nblocker)
FOR i=0,nblocker-1 DO BEGIN
  wavelengthbdtmp[i] = wavelengthbd[i] + centerblocker - lam0
  blockerbdtmp[i] = blockerbd[i]/100.0d0
ENDFOR

frontwindowint = lininterp1f(wavelengthdtmp,frontwindowdtmp,wavelength,ydefault2,nfront,num_lambda) ;Interpolation on the same wavelength grid
blockerint = lininterp1f(wavelengthbdtmp,blockerbdtmp,wavelength,ydefault2,nblocker,num_lambda)

FOR j=0,num_lambda-1 DO blockerint[j] = blockerint[j]*frontwindowint[j]

calculate_phase_parameters,num_lambda_filter,HCME1,HCMWB,HCMNB,HCME1phase,HCMWBphase,HCMNBphase

FSR = DBLARR(7)
FSR[0] = 0.1689d0;      //FSR in Angstroms, NB Michelson
FSR[1] = 0.33685d0;     //WB Michelson
FSR[2] = 0.695d0;       //Lyot element E1
FSR[3] = 1.417d0;       //E2
FSR[4] = 2.779d0;       //E3
FSR[5] = 5.682d0;       //E4
FSR[6] = 11.354d0;      //E5

;NON-TUNABLE TRANSMISSION PROFILE
lyot = DBLARR(num_lambda)
FOR j=0,num_lambda-1 DO BEGIN
  lyot[j] = blockerint[j]* $
            (1.d0+contrastNTi[0]*COS(2.0d0*!DPI/FSR[3]*wavelength[j]+phaseNTi[0]))/2.d0 * $
            (1.d0+contrastNTi[1]*COS(2.0d0*!DPI/FSR[4]*wavelength[j]+phaseNTi[1]))/2.d0 * $
            (1.d0+contrastNTi[2]*COS(2.0d0*!DPI/FSR[5]*wavelength[j]+phaseNTi[2]))/2.d0 * $
            (1.d0+contrastNTi[3]*COS(2.0d0*!DPI/FSR[6]*wavelength[j]+phaseNTi[3]))/2.d0
ENDFOR

;TUNABLE TRANSMISSION PROFILE (NB: FILTERS ARE CALCULATED FROM I0 TO I9 WHICH IS NOT BY ORDER OF INCREASING WAVELENGTH FOR Num_lambda_filter > 6)
filters = DBLARR(num_lambda_filter,num_lambda)
FOR i=0,num_lambda_filter-1 DO BEGIN
  FOR j=0,num_lambda-1 DO BEGIN
    filters[i,j] = lyot[j]* $
           (1.d0+contrastTi[0]*COS(2.0d0*!DPI/FSR[0]*wavelength[j]+HCMNBphase[i]+phaseTi[0]))/2.d0 * $
           (1.d0+contrastTi[1]*COS(2.0d0*!DPI/FSR[1]*wavelength[j]+HCMWBphase[i]+phaseTi[1]))/2.d0 * $
           (1.d0+contrastTi[2]*COS(2.0d0*!DPI/FSR[2]*wavelength[j]-HCME1phase[i]+phaseTi[2]))/2.d0
  ENDFOR
ENDFOR

RETURN,filters 

END

