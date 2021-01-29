
FUNCTION read_stokes_hmi,path,option, $
              cut=cut,points=points,single_pixel=single_pixel,date=date

i0 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*I0*.fits'))[0],hd)
i1 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*I1*.fits'))[0])
i2 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*I2*.fits'))[0])
i3 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*I3*.fits'))[0])
i4 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*I4*.fits'))[0])
i5 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*I5*.fits'))[0])
q0 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*Q0*.fits'))[0])
q1 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*Q1*.fits'))[0])
q2 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*Q2*.fits'))[0])
q3 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*Q3*.fits'))[0])
q4 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*Q4*.fits'))[0])
q5 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*Q5*.fits'))[0])
u0 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*U0*.fits'))[0])
u1 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*U1*.fits'))[0])
u2 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*U2*.fits'))[0])
u3 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*U3*.fits'))[0])
u4 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*U4*.fits'))[0])
u5 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*U5*.fits'))[0])
v0 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*V0*.fits'))[0])
v1 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*V1*.fits'))[0])
v2 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*V2*.fits'))[0])
v3 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*V3*.fits'))[0])
v4 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*V4*.fits'))[0])
v5 = FITSIO_READ_IMAGE((FILE_SEARCH(STRTRIM(path,2) + '/*V5*.fits'))[0])

IF option EQ 1 THEN BEGIN
  SPAWN,'show_info key=RSUN_REF,DSUN_OBS,CDELT1,CDELT2,CRPIX1,CRPIX2,CROTA2,CRVAL1,CRVAL2,OBS_VR,OBS_VW,OBS_VN,CRLT_OBS ds=hmi.S_720s"[' $
        + STRTRIM(date,2) + ']"',data
  dum = STRSPLIT(data[1], /extra)
  rsun_ref = DOUBLE(dum[0])
  dsun_obs = DOUBLE(dum[1])
  cdeltx = DOUBLE(dum[2])
  cdelty = DOUBLE(dum[3])
  crpixx = DOUBLE(dum[4])
  crota2 = DOUBLE(dum[6])
ENDIF ELSE BEGIN
  element = WHERE(STRPOS(hd, 'RSUN_REF') EQ 0)
  rsun_ref = DOUBLE(STRTRIM(STRMID(hd[element], STRPOS(hd[element],'=')+1),2))
  element = WHERE(STRPOS(hd, 'DSUN_OBS') EQ 0)
  dsun_obs = DOUBLE(STRTRIM(STRMID(hd[element], STRPOS(hd[element],'=')+1),2))
  element = WHERE(STRPOS(hd, 'CDELT1') EQ 0)
  cdeltx = DOUBLE(STRTRIM(STRMID(hd[element], STRPOS(hd[element],'=')+1),2))
  element = WHERE(STRPOS(hd, 'CDELT2') EQ 0)
  cdelty = DOUBLE(STRTRIM(STRMID(hd[element], STRPOS(hd[element],'=')+1),2))
  element = WHERE(STRPOS(hd, 'CRPIX1') EQ 0)
  crpixx = DOUBLE(STRTRIM(STRMID(hd[element], STRPOS(hd[element],'=')+1),2))
  element = WHERE(STRPOS(hd, 'CROTA2') EQ 0)
  crota2 = DOUBLE(STRTRIM(STRMID(hd[element], STRPOS(hd[element],'=')+1),2))
ENDELSE

header = [rsun_ref,dsun_obs,cdeltx,cdelty,crpixx,crota2]

IF KEYWORD_SET(cut) OR KEYWORD_SET(single_pixel) THEN BEGIN
  index = WHERE(i0 NE i0)
  i0[index] = 0
  dims = SIZE(i0,/dim)
  !P.MULTI = [0,2,1]
  WINDOW,0,xsi=dims[0]/2,ysi=dims[1]/4.
  ;WINDOW,0,xsi=dims[0]/4.,ysi=dims[1]/4.
  ABGM_TVFRAME,SQRT(q2^2+u2^2+v2^2)>(-500)<500,/as,/sa
  LOADCT,3,/sil
  ABGM_TVFRAME,i0>0<65000,/as,/sa
  LOADCT,0,/sil
  !P.MULTI = 0
  IF NOT KEYWORD_SET(single_pixel) THEN BEGIN
    PRINT,'Please, mark the lower left corner...'
    CURSOR,xini,yini,/down
    PRINT,'Please, mark the upper right corner...'
    CURSOR,xfin,yfin,/down
    ;coordinates to test
      ;;;xini = 1925
      ;;;yini = 1980
      ;;;xfin = 1975
      ;;;yfin = 2030
    ;END coordinates to test
  ENDIF ELSE BEGIN
    PRINT,'Please, select a pixel...'
    CURSOR,xini,yini,/down
    ;coordinates to test
      ;;;xini = 1893
      ;;;yini = 2090
    ;END coordinates to test
    xfin = xini
    yfin = yini
  ENDELSE
  WDELETE,0
  points = ROUND([xini,yini,xfin,yfin])
ENDIF

IF NOT KEYWORD_SET(cut) AND NOT KEYWORD_SET(points) THEN points = [0,0,4095,4095]
  
xini = points[0]
yini = points[1]
xfin = points[2]
yfin = points[3]
i0 = i0[xini:xfin,yini:yfin]
i1 = i1[xini:xfin,yini:yfin]
i2 = i2[xini:xfin,yini:yfin]
i3 = i3[xini:xfin,yini:yfin]
i4 = i4[xini:xfin,yini:yfin]
i5 = i5[xini:xfin,yini:yfin]
q0 = q0[xini:xfin,yini:yfin]
q1 = q1[xini:xfin,yini:yfin]
q2 = q2[xini:xfin,yini:yfin]
q3 = q3[xini:xfin,yini:yfin]
q4 = q4[xini:xfin,yini:yfin]
q5 = q5[xini:xfin,yini:yfin]
u0 = u0[xini:xfin,yini:yfin]
u1 = u1[xini:xfin,yini:yfin]
u2 = u2[xini:xfin,yini:yfin]
u3 = u3[xini:xfin,yini:yfin]
u4 = u4[xini:xfin,yini:yfin]
u5 = u5[xini:xfin,yini:yfin]
v0 = v0[xini:xfin,yini:yfin]
v1 = v1[xini:xfin,yini:yfin]
v2 = v2[xini:xfin,yini:yfin]
v3 = v3[xini:xfin,yini:yfin]
v4 = v4[xini:xfin,yini:yfin]
v5 = v5[xini:xfin,yini:yfin]

dims = SIZE(i0,/dim)

IF N_ELEMENTS(dims) EQ 1 THEN $
     hmi = REFORM(FLTARR(6,4,dims[0]),6,4,dims[0],1) $
ELSE hmi = FLTARR(6,4,dims[0],dims[1])

hmi[0,0,*,*] = i0
hmi[1,0,*,*] = i1
hmi[2,0,*,*] = i2
hmi[3,0,*,*] = i3
hmi[4,0,*,*] = i4
hmi[5,0,*,*] = i5
hmi[0,1,*,*] = q0
hmi[1,1,*,*] = q1
hmi[2,1,*,*] = q2
hmi[3,1,*,*] = q3
hmi[4,1,*,*] = q4
hmi[5,1,*,*] = q5
hmi[0,2,*,*] = u0
hmi[1,2,*,*] = u1
hmi[2,2,*,*] = u2
hmi[3,2,*,*] = u3
hmi[4,2,*,*] = u4
hmi[5,2,*,*] = u5
hmi[0,3,*,*] = v0
hmi[1,3,*,*] = v1
hmi[2,3,*,*] = v2
hmi[3,3,*,*] = v3
hmi[4,3,*,*] = v4
hmi[5,3,*,*] = v5

out = {hmi:hmi,header:header, $
       xini:xini,xfin:xfin,yini:yini,yfin:yfin}

RETURN,out

END
