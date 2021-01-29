PRO join_inverted_data,out_path=out_path,parts=parts,show_products=show_products

IF NOT KEYWORD_SET(parts) THEN folders = FILE_SEARCH('part*') $
ELSE folders = FILE_SEARCH(STRTRIM(parts,2))

IF NOT KEYWORD_SET(out_path) THEN out_path = 'out_data/'
IF (NOT FILE_TEST(out_path)) THEN FILE_MKDIR,out_path

atmosphere = []
errores = []
synthetic = []
observations = []
scatter = []

FOR i=0,N_ELEMENTS(folders)-1 DO BEGIN
  data = FILE_SEARCH(folders[i] + '/*out*/*_products.sav')
  RESTORE,data
  atmosphere = [[[atmosphere]],[[atmos]]]
  errores = [[[errores]],[[error]]]
  IF i EQ 0 THEN BEGIN
    synthetic = syn
    observations = obs
    scatter = scat
  ENDIF ELSE BEGIN
    synthetic = TRANSPOSE([TRANSPOSE(synthetic),TRANSPOSE(syn)])
    observations = TRANSPOSE([TRANSPOSE(observations),TRANSPOSE(obs)])
    scatter = TRANSPOSE([TRANSPOSE(scatter),TRANSPOSE(scat)])
  ENDELSE
END

savename = STRJOIN((STRSPLIT(data,'/',/extract))[1:2],'/')
atmos = atmosphere
error = errores
syn = synthetic
obs = observations
scat = scatter
SAVE,filename=savename,atmos,syn,obs,error,scat

IF KEYWORD_SET(show_products) THEN show_infered_hmi_products,savename

END


