PRO show_infered_hmi_products,out_path=out_path,prefix=prefix,file=file

IF KEYWORD_SET(file) THEN BEGIN
  atmos_file = FILE_SEARCH(file + '*atmospheres.fits')
  error_file = FILE_SEARCH(file + '*errors.fits')
ENDIF ELSE BEGIN
  IF NOT KEYWORD_SET(out_path) THEN out_path = 'out_data/'
  IF NOT KEYWORD_SET(prefix) THEN BEGIN
    atmos_file = FILE_SEARCH(out_path + '*atmospheres.fits')
    error_file = FILE_SEARCH(out_path + '*errors.fits')
  ENDIF ELSE BEGIN
    atmos_file = FILE_SEARCH(out_path + prefix + '*atmospheres.fits')
    error_file = FILE_SEARCH(out_path + prefix + '*errors.fits')
  ENDELSE
ENDELSE

atmos = READFITS(atmos_file)
error = READFITS(error_file)

;Parameter 1: eta0
eta0 = REFORM(atmos[0,*,*])

;Parameter 2: incli
incli = REFORM(atmos[1,*,*])

;Parameter 3: azi
azi = REFORM(atmos[2,*,*])

;Parameter 5: Doppler width
dop = REFORM(atmos[4,*,*])

;Parameter 6: field
field = REFORM(atmos[5,*,*])

;Parameter 7: vlos
vlos = REFORM(atmos[6,*,*]) / 1e5

;Parameter 8: s0
s0 = REFORM(atmos[7,*,*])

;Parameter 9: s1
s1 = REFORM(atmos[8,*,*])

;Parameter 10: ff
ff = REFORM(atmos[9,*,*])

;Parameter 11: chi2
chi2 = REFORM(error[-1,*,*])

;Parameter 12: inten
inten = s0 + s1


chars = 2

!P.MULTI = [0,3,3]
WINDOW,0,xsi=1500,ysi=1000

LOADCT,13,/sil
ABGM_TVFRAME,eta0,/as,/sa,/bar,chars=chars,bran=[0,50], $
            btit='Eta0'
LOADCT,16,/sil
ABGM_TVFRAME,incli,/as,/sa,/bar,chars=chars,bran=[0,180], $
            btit='Inclination (deg)'
LOADCT,13,/sil
ABGM_TVFRAME,azi,/as,/sa,/bar,chars=chars,bran=[0,180], $
            btit='Azimuth (deg)'
LOADCT,13,/sil
ABGM_TVFRAME,dop,/as,/sa,/bar,chars=chars,bran=[0,65], $
            btit='Doppler Width (Length Units)'
LOADCT,5,/sil
ABGM_TVFRAME,field*ff,/as,/sa,/bar,chars=chars,bran=[0,3000], $
            btit='Field * ff (G)'
LOADCT,16,/sil
ABGM_TVFRAME,vlos,/as,/sa,/bar,chars=chars,bran=[-3.5,3.5], $
            btit='Vlos (km/s)'
LOADCT,3,/sil
ABGM_TVFRAME,s0,/as,/sa,/bar,chars=chars,bran=[0,60000], $
            btit='S0'
LOADCT,3,/sil
ABGM_TVFRAME,s1,/as,/sa,/bar,chars=chars,bran=[0,60000], $
            btit='S1'
LOADCT,13,/sil
ABGM_TVFRAME,ff,/as,/sa,/bar,chars=chars,bran=[0,1], $
            btit='Filling Factor'




!P.MULTI = [0,2,2]
WINDOW,1,xsi=1500,ysi=1000

LOADCT,3,/sil
value = MAX(inten)
maximum = CEIL(value/10000)
ABGM_TVFRAME,inten,/as,/sa,/bar,chars=chars,bran=[0,maximum], $
            btit='S0+S1'
LOADCT,13,/sil
ABGM_TVFRAME,chi2,/as,/sa,/bar,chars=chars,bran=[0,100], $
            btit='Chi!u2!n'
LOADCT,5,/sil
ABGM_TVFRAME,field,/as,/sa,/bar,chars=chars,bran=[0,3000], $
            btit='Field (G)'

LOADCT,0,/sil

END

