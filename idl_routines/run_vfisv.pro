
; ------------------------------------------------------------------------------
;
; RUN_VFISV
; Date: October 2010
;
; PURPOSE: Runs VFISV for Alberto's Hinode filtered data /local/d/rce/marc/hinode-vfisv
; and reads and plots results
;
; March 2020
; ABGM modified ASD program
; ------------------------------------------------------------------------------



PRO draw,obs2,syn2,scat2,alpha,mode

  WINDOW,0,xsize=1200,ysize=1000

  sio = REFORM(obs2[*,0])
  sqo = REFORM(obs2[*,1])
  suo = REFORM(obs2[*,2])
  svo = REFORM(obs2[*,3])

  sis = REFORM(syn2[*,0])
  sqs = REFORM(syn2[*,1])
  sus = REFORM(syn2[*,2])
  svs = REFORM(syn2[*,3])

  col = [255,250,200,150]
  linestyle = [0,2,3,4]
 
  !P.MULTI = [0,2,2]
  LOADCT,39,/sil

  obs = sio
  nomag = scat2
  synt = sis
  IF mode EQ 1 THEN BEGIN
    mag = (synt - (1-alpha)*nomag) / alpha
    ymin = MIN([obs,synt,nomag,mag]) * 0.9
    ymax = MAX([obs,synt,nomag,mag]) * 1.2
    items = ['Observed','Inverted','NonMag','Mag']
  ENDIF ELSE BEGIN
    ymin = MIN([obs,synt,nomag]) * 0.9
    ymax = MAX([obs,synt,nomag]) * 1.2
    ;mag = synt*alpha + (1-alpha)*nomag
    items = ['Synthesize','Mag','NonMag'] 
  ENDELSE

  xmin = -4
  xmax = N_ELEMENTS(obs) + 1
  xx = FINDGEN(N_ELEMENTS(obs)) + 1
  PLOT,xx,obs,col=col[0],psym=-1,xran=[xmin,xmax],yran=[ymin,ymax], $
       /xst,/yst,linestyle=linestyle[0]
  OPLOT,xx,synt,col=col[1],psym=-1,linestyle=linestyle[1]
  OPLOT,xx,nomag,col=col[2],psym=-1,linestyle=linestyle[2]
  IF mode EQ 1 THEN OPLOT,xx,mag,col=col[3],psym=-1,linestyle=linestyle[3]

  chars = 2
  XYOUTS,3,ymax*0.9,'!4a!6 = ' + STRTRIM(alpha,2),chars=chars
  pos = [-3.5,ymax*0.75]
  FOR i=0l,N_ELEMENTS(items)-1 DO BEGIN
    dx = (xmax - xmin) / 10.
    dy = (ymax - ymin) / 15.
    yit = pos[1] - i*dy
    OPLOT,[pos[0],pos[0]+dx],[yit,yit] + dy/3.,col=col[i],linestyle=linestyle[i]
    XYOUTS,pos[0]+dx*1.3,yit,items[i],chars=chars
  ENDFOR

  xmin = 0
  xmax = N_ELEMENTS(obs) + 1

  min = MIN([-1000,sqo,sqs]) * 1.2
  max = MAX([1000,sqo,sqs]) * 1.2
  PLOT,xx,sqo,col=col[0],psym=-1,xran=[xmin,xmax],yran=[min,max],/xst,/yst,linestyle=linestyle[0]
  IF mode EQ 1 THEN OPLOT,xx,sqs,col=col[1],psym=-1,linestyle=linestyle[1]

  min = MIN([-1000,suo,sus]) * 1.2
  max = MAX([1000,suo,sus]) * 1.2
  PLOT,xx,suo,col=col[0],psym=-1,xran=[xmin,xmax],yran=[min,max],/xst,/yst,linestyle=linestyle[0]
  IF mode EQ 1 THEN OPLOT,xx,sus,col=col[1],psym=-1,linestyle=linestyle[1]

  min = MIN([-1000,svo,svs]) * 1.2
  max = MAX([1000,svo,svs]) * 1.2
  PLOT,xx,svo,col=col[0],psym=-1,xran=[xmin,xmax],yran=[min,max],/xst,/yst,linestyle=linestyle[0]
  IF mode EQ 1 THEN OPLOT,xx,svs,col=col[1],psym=-1,linestyle=linestyle[1]

  !P.MULTI = 0
  LOADCT,0,/sil

RETURN
END




PRO print_screen,params,errors

  PRINT,'         '
  PRINT,'         Eta0: ' + STRTRIM(params[0],2)
  PRINT,'         Inclination (deg): ' + STRTRIM(params[1],2)
  PRINT,'         Azimuth (deg): ' + STRTRIM(params[2],2)
  PRINT,'         Damping (Length Units): ' + STRTRIM(params[3],2)
  PRINT,'         Doppler Width (mA): ' + STRTRIM(params[4],2)
  PRINT,'         Strength (G): ' + STRTRIM(params[5],2)
  PRINT,'         LOS velocity (cm/s): ' + STRTRIM(params[6],2)
  PRINT,'         s0 (DN/s): ' + STRTRIM(params[7],2)
  PRINT,'         s1 (DN/s): ' + STRTRIM(params[8],2)
  PRINT,'         Filling Factor: ' + STRTRIM(params[9],2)
  PRINT,'         Chi2: ' + STRTRIM(errors[11],2)
  PRINT,'         '

RETURN

END


;;;;;;;;;;;;;;; MAIN PROGRAM: 

PRO run_vfisv,file_mult,savename,in_path,out_path,vfisv_path, $
              xini,yini,date,header, $
              stokes_data=stokes_data, $
              see=see,print_parameters=print_parameters, $
              list_free_params=list_free_params,guess=guess, $
              num_lambdas=num_lambdas,synthesis=synthesis

RESTORE, in_path + file_mult

PRINT,'----'
SPAWN,'ls -l ' + vfisv_path + 'vfisv.x',ls
parts = STRSPLIt(ls,' ',/extra)
PRINT,'Compiled the ' + parts[6] + ' of ' + parts[5] + ' at ' + parts[7]
PRINT,'----'


dims = SIZE(hmi_data,/dim)
IF N_ELEMENTS(dims) LT 4 THEN BEGIN
  IF N_ELEMENTS(dims) EQ 2 THEN dims = [dims,1,1]
  IF N_ELEMENTS(dims) EQ 3 THEN dims = [dims,1]
ENDIF

IF NOT KEYWORD_SET(synthesis) THEN BEGIN
  xpix = FIX(dims[2])
  stokes = FIX(dims[1])
  ypix = FIX(dims[3])
  atmos = DBLARR(N_ELEMENTS(guess),xpix,ypix)
  syn = DBLARR(num_lambdas,stokes,xpix,ypix)
  obs = DBLARR(num_lambdas,stokes,xpix,ypix)
  scat = DBLARR(num_lambdas,xpix,ypix)
  error = DBLARR(12,xpix,ypix)
ENDIF ELSE BEGIN
  xpix = 1
  stokes = 4
  ypix = 1
  syn = DBLARR(num_lambdas,stokes)
  obs = DBLARR(num_lambdas,stokes)
  scat = DBLARR(num_lambdas)
ENDELSE

;;;mode = synthesis ; synthesis = 1 -> sintetiza   /    synthesis = 0 -> invierte
IF KEYWORD_SET(synthesis) THEN mode = 0 ELSE mode = 1

num_lambdas_short = FIX(49)
num_lambdas_long = FIX(149)

;+++++++++++++++++++++++++++++
;Calculate filters
save_filters_hmi,rsun_ref,dsun_obs,cdeltx,crpixx,crpixy,num_lambdas,num_lambdas_long,out_path,date=date
filters_in = DBLARR(xpix,ypix,num_lambdas_long,num_lambdas)

aux = 1l
fact = 100. / FLOAT(xpix) / FLOAT(ypix)

PRINT,'----'
FOR ii=0,xpix-1 DO BEGIN
  FOR jj=0,ypix-1 DO BEGIN
    nccd = CEIL(4096.*(yini+jj)+(xini+ii))
    save_filters_hmi,rsun_ref,dsun_obs,cdeltx,crpixx,crpixy,num_lambdas, $
                     num_lambdas_long,out_path,nccd=nccd,filt
    filters_in[ii,jj,*,*] = TRANSPOSE(filt)
    PRINT, STRING(13B), "Calculating filters...", $
           FLOAT(aux)*fact, " %",format='(a,a,f6.2,a,$)'
    aux++
  ENDFOR
ENDFOR
PRINT,''
PRINT,'----'
;+++++++++++++++++++++++++++++


nthings = 6
aux = 1l
fact = 100. / FLOAT(xpix) / FLOAT(ypix)

FOR ii=0,xpix-1 DO BEGIN
  FOR jj=0,ypix-1 DO BEGIN

    filters = DOUBLE(REFORM(filters_in[ii,jj,*,*]))
    IF mode EQ 1 THEN obs_in = DOUBLE(REFORM(hmi_data[*,*,ii,jj])) $ 
    ELSE obs_in = DBLARR(num_lambdas,stokes)
    scat_in = DBLARR(num_lambdas_short,stokes)

    IF aux EQ 1 THEN BEGIN
      ndims1 = FIX((SIZE(filters))[0])
      ndims2 = FIX((SIZE(scat_in))[0])
      ndims3 = FIX((SIZE(obs_in))[0])
      ndims4 = FIX((SIZE(guess))[0])
      ndims5 = FIX((SIZE(list_free_params))[0])

      dims_guess = FIX((SIZE(guess,/dim))[0])
      dims_list = FIX((SIZE(list_free_params,/dim))[0])

      PRINT,' '
    ENDIF

    OPENW,wunit,'input.bin',/get_lun
      WRITEU, wunit, nthings       

      WRITEU, wunit, mode

      WRITEU, wunit, ndims1        
      WRITEU, wunit, num_lambdas_long  
      WRITEU, wunit, num_lambdas       
      WRITEU, wunit, filters       

      WRITEU, wunit, ndims2        
      WRITEU, wunit, num_lambdas_short
      WRITEU, wunit, stokes        
      WRITEU, wunit, scat_in       

      WRITEU, wunit, ndims3        
      WRITEU, wunit, num_lambdas       
      WRITEU, wunit, stokes       
      WRITEU, wunit, obs_in       

      WRITEU, wunit, ndims4        
      WRITEU, wunit, dims_guess
      WRITEU, wunit, guess        

      WRITEU, wunit, ndims5        
      WRITEU, wunit, dims_list
      WRITEU, wunit, list_free_params        

    FREE_LUN, wunit

    SPAWN,vfisv_path+'vfisv.x'

    nthings = FIX(0)
    ndims1 = FIX(0)
    ndims2 = FIX(0)
    ndims3 = FIX(0)
    ndims4 = FIX(0)
    ndims5 = FIX(0)
    ndims6 = FIX(0)
    dims1 = INTARR(2)
    dims2 = INTARR(2)
    dims3 = INTARR(2)
    dims4 = FIX(0)
    dims5 = FIX(0)
    dims6 = FIX(0)
    syn_tmp = DBLARR(stokes*num_lambdas)
    scat_tmp = DBLARR(stokes*num_lambdas)
    obs_tmp = DBLARR(stokes*num_lambdas)
    atmos_tmp = DBLARR(dims_list)
    error_tmp = DBLARR(12)

    OPENR,runit,'output.bin',/get_lun
      READU, runit,nthings
      READU, runit,ndims1
      READU, runit,dims1
      READU, runit,syn_tmp 
      READU, runit,ndims2
      READU, runit,dims2
      READU, runit,scat_tmp
      READU, runit,ndims3
      READU, runit,dims3
      READU, runit,obs_tmp
      READU, runit,ndims4
      READU, runit,dims4
      READU, runit,atmos_tmp
      READU, runit,ndims6
      READU, runit,dims6
      READU, runit,error_tmp
    FREE_LUN, runit

    IF mode EQ 1 THEN BEGIN
      FOR kk=0,3 DO BEGIN
        syn[*,kk,ii,jj] = syn_tmp[kk*num_lambdas:kk*num_lambdas+num_lambdas-1]
        obs[*,kk,ii,jj] = REVERSE(obs_tmp[kk*num_lambdas:kk*num_lambdas+num_lambdas-1])
      ENDFOR
      scat[*,ii,jj] = scat_tmp[0:num_lambdas-1]
      atmos[*,ii,jj] = atmos_tmp
      error[*,ii,jj] = error_tmp
      
      IF KEYWORD_SET(see) THEN $
        draw,REFORM(obs[*,*,ii,jj]),REFORM(syn[*,*,ii,jj]),scat_tmp(0:5),atmos_tmp[9],mode

      IF (aux EQ 1) THEN PRINT,''
      PRINT, STRING(13B), 'Running ', vfisv_path, 'vfisv.x ... ', $
             FLOAT(aux*fact), " %",format='(a,a,a,a,f6.1,a,$)'

      aux++
    ENDIF ELSE BEGIN
      atmos = atmos_tmp
      FOR kk=0,3 DO BEGIN
        syn[*,kk] = obs_tmp[kk*num_lambdas:kk*num_lambdas+num_lambdas-1]
        obs[*,kk] = syn_tmp[kk*num_lambdas:kk*num_lambdas+num_lambdas-1]
      ENDFOR
      scat = scat_tmp[0:num_lambdas-1]
      error = error_tmp
    ENDELSE

    IF KEYWORD_SET(print_parameters) THEN print_screen,atmos[*,ii,jj],error[*,ii,jj]

  ENDFOR
ENDFOR

PRINT,''
PRINT,''
PRINT,'Saving products in ' + STRTRIM(out_path,2) + ' file...'
PRINT,''
PRINT,''

; atmos variable cointains: 
; eta0, gamma, phi, damping, dopplerw, bfield, vlos, s0, s1, alpha_mag
;SAVE,filename=savename,atmos,syn,obs,error,scat
WRITEFITS,savename + '_atmosphere.fits',atmos
WRITEFITS,savename + '_observed_profiles.fits',obs
WRITEFITS,savename + '_magnetic_profiles.fits',syn
WRITEFITS,savename + '_nonmagnetic_profiles.fits',scat
WRITEFITS,savename + '_error.fits',error


answer = ''
WHILE STRUPCASE(answer) NE 'N' AND STRUPCASE(answer) NE 'Y' DO BEGIN
  READ,answer,prompt='Do you want to see the infered products? (y,Y / n,N): '
  PRINT,''
  PRINT,''
  
  IF answer EQ 'y' OR answer EQ 'Y' THEN BEGIN
    IF mode EQ 0 THEN BEGIN
      PRINT,''
      print_screen,atmos,error 
      PRINT,'     **********'
      draw,obs,syn,scat,atmos[9],mode
    ENDIF ELSE BEGIN 
      IF (dims[2] EQ 1 OR dims[3] EQ 1) OR (N_ELEMENTS(obs) EQ 2) THEN BEGIN
        FOR i=0,dims[2]-1 DO BEGIN
          FOR j=0,dims[3]-1 DO BEGIN
            PRINT,''
            PRINT,'     Pixel [' + STRTRIM(xini+i,2) + ',' + STRTRIM(yini+j,2) + ']:'
            print_screen,atmos[*,i,j],error[*,i,j] 
            PRINT,'     **********'
          ENDFOR
        ENDFOR
      ENDIF ELSE show_infered_hmi_products,savename 
    ENDELSE
  ENDIF
ENDWHILE

PRINT,''
PRINT,''
PRINT,'Inversion process is finished...'
PRINT,''
PRINT,''
stop


END

