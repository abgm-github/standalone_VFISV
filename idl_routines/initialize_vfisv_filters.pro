FUNCTION initialize_vfisv_filters,t_rec,nx2,ny2,nfront

referencenlam = 7000
nblocker = 201

nelemPHASENT = 4l * LONG64(nx2) * LONG64(ny2)
nelemCONTRASTT = 3l * LONG64(nx2) * LONG64(ny2)

phaseNTf = FLTARR(nx2,ny2,4)
file = FILE_SEARCH(STRSPLIT(!PATH, ':', /extra), 'non_tunable_phases_710660_June09_cal_128_2.bin')
OPENR,runit,file,/get_lun
  READU,runit,phaseNTf
FREE_LUN,runit
phaseNTf = DOUBLE(phaseNTf)
phaseNT = DBLARR(nelemPHASENT)
FOR i=0,nelemPHASENT-1 DO phaseNT[i] = phaseNTf[i]*!DPI/180.d0 ;convert phases from degrees to radians

contrastNTf = FLTARR(nx2,ny2,4)
file = FILE_SEARCH(STRSPLIT(!PATH, ':', /extra), 'non_tunable_contrasts_710660_June09_cal_128_2.bin')
OPENR,runit,file,/get_lun
  READU,runit,contrastNTf
FREE_LUN,runit
contrastNTf = DOUBLE(contrastNTf)
contrastNT = DBLARR(nelemPHASENT)
FOR i=0,nelemPHASENT-1 DO contrastNT[i] = contrastNTf[i]

contrastTf = FLTARR(nelemCONTRASTT)
file = FILE_SEARCH(STRSPLIT(!PATH, ':', /extra), 'tunable_contrasts_710660_June09_cal_128.bin')
OPENR,runit,file,/get_lun
  READU,runit,contrastTf
FREE_LUN,runit
contrastTf = DOUBLE(contrastTf)
contrastT = DBLARR(nelemCONTRASTT)
FOR i=0,nelemCONTRASTT-1 DO contrastT[i] = contrastTf[i]

values = take_values_wd_fd(nfront)

date_split = get_date_hmi_vfisv(date=t_rec,split1='_',split2='.',split3=':')
year = DOUBLE(date_split.year)
month = DOUBLE(date_split.month)
day = DOUBLE(date_split.day)
hour = DOUBLE(date_split.hour)
minute = DOUBLE(date_split.minute)
second = DOUBLE(date_split.second)

ref_jul = JULDAY(4.,30.,2010.,0.,0.,0.)
ref_hmi_num = 1051660800.000000d0
cur_jul = JULDAY(month,day,year,hour,minute,second)
cur_hmi_num = ref_hmi_num + (cur_jul-ref_jul)*24.d0*60.d0*60.d0

file = FILE_SEARCH(STRSPLIT(!PATH, ':', /extra), 'phaseMaps/header_parameters.txt')
lines = file_lines(file)
nums = DBLARR(7,lines)
OPENR,runit,file,/get_lun
READF,runit,nums
FREE_LUN,runit

index = MAX(WHERE(nums[0,*]-cur_hmi_num LE 0))
FSNphasemaps = nums[1,index]
FSNphasemaps = LONG64(FSNphasemaps)

;;;;; +++++++++++++++++++++++++++++++
;;;;;abgm

;;;SPAWN,'show_info key=HCME1,HCMWB,HCMNB,HCMPOL,HCAMID ds=hmi.phasemaps_corrected"[' + $
;;;       STRTRIM(STRING(FSNphasemaps),2) + ']" -P',data
;;;data = data[1:*]
;;;camera = '2'
;;;;camera = '3'
;;;
;;;FOR i=0,N_ELEMENTS(data)-1 DO BEGIN
;;;  dum = STRSPLIT(data[i], /extra)
;;;  IF (STRTRIM(dum[4],2) EQ camera) THEN BEGIN
;;;    hcme1 = dum[0]
;;;    hcmwb = dum[1]
;;;    hcmnb = dum[2]
;;;    hcmpol = dum[3]
;;;    hcamid = dum[4]
;;;    path_phasemap = dum[5]
;;;    BREAK
;;; ENDIF
;;;ENDFOR
;;;tempphase = FITSIO_READ_IMAGE(path_phasemap + '/phases.fits')

;;;;; +++++++++++++++++++++++++++++++
;;;;; In case you want to use the show_info to find the data, comment the next lines until ;abgm
;;;;; and uncomment the previous from  
;;;;; SPAWN,'show_info key=HCME1,HCMWB,HCMNB,HCMPOL,HCAMID ds=hmi.phasemaps_corrected"[' + $...
;;;;; +++++++++++++++++++++++++++++++

index1 = WHERE(STRTRIM(FSNphasemaps,2) EQ nums[1,*])
camera = '2'
;;;;;camera = '3'
index2 = WHERE(nums[6,index1] EQ camera)
index = index1[index2]

hcme1 = FIX(nums[2,index])
hcmwb = FIX(nums[3,index])
hcmnb = FIX(nums[4,index])
hcmpol = FIX(nums[5,index])

file = FILE_SEARCH(STRSPLIT(!PATH, ':', /extra), 'phases' + STRTRIM(STRING(FSNphasemaps),2) + '_' + STRTRIM(camera,2) + '.fits')
tempphase = FITSIO_READ_IMAGE(file[0])

;;;;;abgm
;;;;; +++++++++++++++++++++++++++++++

phaseT = DBLARR(nelemCONTRASTT)
FOR i=0,nelemCONTRASTT-1 DO BEGIN
  ii = i / 3
  jj = i MOD 3
  phaseT[i] = DOUBLE(tempphase[ii*5+jj]) * !DPI / 180.d0 ;NB: PHASE MAPS ARE ASSUMED TO BE IN TYPE FLOAT AND IN DEGREES
ENDFOR

RETURN,{phaseT:phaseT,phaseNT:phaseNT,contrastT:contrastT,contrastNT:contrastNT, $
        hcme1:hcme1,hcmwb:hcmwb,hcmnb:hcmnb,hcmpol:hcmpol}

END

