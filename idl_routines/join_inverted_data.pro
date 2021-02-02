PRO join_inverted_data,out_path=out_path,parts=parts,show_products=show_products

IF NOT KEYWORD_SET(parts) THEN folders = FILE_SEARCH('part*') $
ELSE folders = FILE_SEARCH(STRTRIM(parts,2))

IF NOT KEYWORD_SET(out_path) THEN out_path = 'out_data/'
IF (NOT FILE_TEST(out_path)) THEN FILE_MKDIR,out_path

atmosphere = []
observations = []
magnetic = []
nonmagnetic = []
errors = []

FOR i=0,N_ELEMENTS(folders)-1 DO BEGIN
  atmos = FILE_SEARCH(folders[i] + '/*out*/*_atmosphere.fits')
  IF i EQ 0 THEN savename = atmos
  atmos = READFITS(atmos)
  observed_profiles = FILE_SEARCH(folders[i] + '/*out*/*_observed_profiles.fits')
  observed_profiles = READFITS(observed_profiles)
  magnetic_profiles = FILE_SEARCH(folders[i] + '/*out*/*_magnetic_profiles.fits')
  magnetic_profiles = READFITS(magnetic_profiles)
  nonmagnetic_profiles = FILE_SEARCH(folders[i] + '/*out*/*_nonmagnetic_profiles.fits')
  nonmagnetic_profiles = READFITS(nonmagnetic_profiles)
  error = FILE_SEARCH(folders[i] + '/*out*/*_error.fits')
  error = READFITS(error)

  atmosphere = [[[atmosphere]],[[atmos]]]
  errors = [[[errors]],[[error]]]
  IF i EQ 0 THEN BEGIN
    magnetic = magnetic_profiles
    observations = observed_profiles
    nonmagnetic = nonmagnetic_profiles
  ENDIF ELSE BEGIN
    magnetic = TRANSPOSE([TRANSPOSE(magnetic),TRANSPOSE(magnetic_profiles)])
    observations = TRANSPOSE([TRANSPOSE(observations),TRANSPOSE(observed_profiles)])
    nonmagnetic = TRANSPOSE([TRANSPOSE(nonmagnetic),TRANSPOSE(nonmagnetic_profiles)])
  ENDELSE
END

savename = (STRSPLIT(savename,'/',/extra))[-1]  
savename = out_path + STRJOIN((STRSPLIT(savename,'_',/extra))[0:-2],'_')  

WRITEFITS,savename + '_atmosphere.fits',atmosphere
WRITEFITS,savename + '_observed_profiles.fits',observations
WRITEFITS,savename + '_magnetic_profiles.fits',magnetic
WRITEFITS,savename + '_nonmagnetic_profiles.fits',nonmagnetic
WRITEFITS,savename + '_error.fits',errors


IF KEYWORD_SET(show_products) THEN show_infered_hmi_products,file=savename

END


