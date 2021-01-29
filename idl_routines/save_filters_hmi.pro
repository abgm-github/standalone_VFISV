PRO save_filters_hmi,rsun_ref,dsun_obs,cdeltx,crpixx,crpixy, $
            num_lambda_filter,num_lambda,out_path,date=date,nccd=nccd,filters

nfront = 401l
nx2 = 128l
ny2 = 128l

lambda_min= -1998.000000d0
delta_lambda= 27.000000d0
  
IF KEYWORD_SET(date) THEN BEGIN

  out = initialize_vfisv_filters(date,nx2,ny2,nfront)
  phaseT = out.phaseT
  phaseNT = out.phaseNT
  contrastT = out.contrastT
  contrastNT = out.contrastNT
  hcme1 = out.hcme1
  hcmwb = out.hcmwb
  hcmnb = out.hcmnb
  hcmpol = out.hcmpol
  
  column = 2048l ;MIND these two assume the case input be 4k x 4k, but no need to modify even when the input is not of 4k x 4k:
  row = 2048l ;This part just calculate the filter-normalization factor at the center.
  
  values = take_values(nx2,ny2,column,row,phaseT,phaseNT,contrastT,contrastNT)
  contrastNTi = values.contrastNTi
  contrastTi = values.contrastTi
  phaseNTi = values.phaseNTi
  phaseTi = values.phaseTi
  
  filters = vfisv_filter(num_lambda_filter,num_lambda,lambda_min,delta_lambda, $
            contrastNTi,contrastTi,phaseNTi,phaseTi,nfront,hcme1,hcmwb,hcmnb,hcmpol)
  
  sum_filt = DBLARR(num_lambda_filter)
  FOR i=0,num_lambda_filter-1 DO BEGIN
    sum_filt[i] = 0.0d0
    FOR j=0,num_lambda-1 DO BEGIN
      sum_filt[i] = sum_filt[i] + filters[i,j]
    ENDFOR
  ENDFOR
  
  sumsum = 0
  FOR i=0,num_lambda_filter-1 DO sumsum = sumsum + sum_filt[i]
  
  norm_factor = sumsum / DOUBLE(num_lambda_filter)
  
  SAVE,filename=out_path + '/params_for_filters.sav',phaseT,phaseNT,contrastT,contrastNT, $
       hcme1,hcmwb,hcmnb,hcmpol,norm_factor

ENDIF ELSE BEGIN

  RESTORE,out_path + '/params_for_filters.sav'

  column = nccd MOD 4096 ;column from 0 to 4095
  column = FIX(column)
  row = nccd / 4096 ;row from 0 to 4095

  values = take_values(nx2,ny2,column,row,phaseT,phaseNT,contrastT,contrastNT)
  contrastNTi = values.contrastNTi
  contrastTi = values.contrastTi
  phaseNTi = values.phaseNTi
  phaseTi = values.phaseTi

  filters = vfisv_filter(num_lambda_filter,num_lambda,lambda_min,delta_lambda, $
            contrastNTi,contrastTi,phaseNTi,phaseTi,nfront,hcme1,hcmwb,hcmnb,hcmpol)
  
  FOR i=0,num_lambda_filter-1 DO $
    FOR j=0,num_lambda-1 DO filters[i,j] = filters[i,j] / norm_factor
  
ENDELSE

END 


