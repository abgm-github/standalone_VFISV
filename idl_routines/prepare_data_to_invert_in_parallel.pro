
PRO prepare_data_to_invert_in_parallel, $
            num_proc=num_proc,vfisv_path=vfisv_path, $
            data_folder=data_folder,info_date=info_date, $
            date_it=date_it, $
            cut=cut,points=points, $
            in_path=in_path,out_path=out_path, $
            ff_value=ff_value,invert_ff=invert_ff, $
            list_free_params=list_free_params,guess=guess, $ 
            num_lambdas=num_lambdas,deconv=deconv,suffix=suffix

IF NOT KEYWORD_SET(num_proc) THEN BEGIN
  PRINT,''
  PRINT,'*****************************************************'
  PRINT,''
  PRINT,'It seems you forgot to specify the number of processors to use'
  PRINT,''
  PRINT,'How many processors you want to use for the inversions?: '
  PRINT,'(Introduce a integer number)'
  PRINT,''
  num_proc = 1
  READ,num_proc,prompt='Number of processors: '
  PRINT,''
  PRINT,'*****************************************************'
  PRINT,''
  IF num_proc LE 1 THEN num_proc = 1 
ENDIF

IF NOT KEYWORD_SET(vfisv_path) THEN BEGIN
  PRINT,''
  PRINT,'****************************************************************'
  PRINT,''
  PRINT,'It is mandatory to specify where the code is located'
  PRINT,' using the keyword "vfisv_path"'
  PRINT,''
  vfisv_path = ''
  READ,vfisv_path,prompt="VFISV path (without quotes [ ' ' ]): "
  PRINT,''
  PRINT,''
  PRINT,'****************************************************************'

  IF (NOT FILE_TEST(vfisv_path+'vfisv.x')) THEN BEGIN
    PRINT,''
    PRINT,'********************************************'
    PRINT,''
    PRINT,'The vfisv.x file does not exist... '
    PRINT,''
    PRINT,'Ending...'
    PRINT,''
    PRINT,'********************************************'
    PRINT,''
    RETURN
  ENDIF
ENDIF


IF NOT KEYWORD_SET(date_it) AND NOT KEYWORD_SET(data_folder) THEN BEGIN
  PRINT,''
  PRINT,'****************************************************************'
  PRINT,''
  PRINT,'It is mandatory to active the keyword "date_it" or "data_folder"'
  PRINT,''
  PRINT,'Ending...'
  PRINT,''
  PRINT,'****************************************************************'
  RETURN
ENDIF

IF NOT KEYWORD_SET(cut) AND NOT KEYWORD_SET(points) THEN BEGIN
  PRINT,''
  PRINT,'*********************************************************'
  PRINT,''
  PRINT,'If you do not active the keywords "cut" or "points",'
  PRINT,'the code will invert the full map (4096 * 4096 pixels)'
  PRINT,''
  answer = ''
  READ, answer, PROMPT='Do you want to continue (y,Y / n,N): '
  PRINT,''
  IF STRUPCASE(answer) EQ 'N' THEN BEGIN
    PRINT,'Ending...'
    PRINT,''
    PRINT,'*********************************************************'
    PRINT,''
    RETURN
  ENDIF ELSE BEGIN
    PRINT,'*********************************************************'
    PRINT,''
    xini = 0
    xfin = 4095
    yini = 0
    yfin = 4095
  ENDELSE
ENDIF

IF KEYWORD_SET(points) THEN BEGIN
  xini = FIX(points[0])
  xfin = FIX(points[2])
  yini = FIX(points[1])
  yfin = FIX(points[3])  
ENDIF

IF KEYWORD_SET(cut) THEN BEGIN
  IF KEYWORD_SET(date_it) THEN BEGIN
    text_lookfor = STRING(STRTRIM(date_it[0],2) + '.' + $
                   STRTRIM(date_it[1],2) + '.' + $
                   STRTRIM(date_it[2],2) + '_' + $
                   STRTRIM(date_it[3],2) + ':' + $
                   STRTRIM(date_it[4],2) + ':00_TAI')
  
    SPAWN,'show_info ds=hmi.S_720s"[' + $
          STRTRIM(text_lookfor,2) + ']" -P',paths 
  
    index = WHERE(STRSPLIT(paths,'/') NE 0)
    paths = paths[index:*]
  ENDIF

  IF KEYWORD_SET(data_folder) THEN BEGIN
    IF NOT KEYWORD_SET(info_date) THEN BEGIN
      PRINT,''
      PRINT,'***************************************'
      PRINT,''
      PRINT,'Code needs the info_date keyword... '
      PRINT,''
      PRINT,"The format is 'YYYY.MM.DD_hh:mm:ss_TAI'"
      PRINT,'where YYYY is the year'
      print,'MM is the month'
      print,'DD is the day'
      print,'hh is the hour'
      print,'mm is the minutes'
      print,'ss is the seconds'
      PRINT,''
      PRINT,'Ending...'
      PRINT,''
      PRINT,'***************************************'
      PRINT,''
      RETURN
    ENDIF
    paths = data_folder
  ENDIF

  maps = read_stokes_hmi(paths,cut=cut,points=points)
  xini = FIX(maps.xini)
  xfin = FIX(maps.xfin)
  yini = FIX(maps.yini)
  yfin = FIX(maps.yfin)
ENDIF

ycoor = [0]
rest = (yfin-yini+1) MOD FIX(num_proc)
division = (yfin-yini+1) / FIX(num_proc)
FOR i=1,num_proc DO BEGIN
  IF i EQ num_proc THEN BEGIN
    ycoor = [ycoor,division*i+rest]
    BREAK
  ENDIF
  ycoor = [ycoor,division*i]
ENDFOR 

text1 = '"' + vfisv_path + '"'

IF NOT KEYWORD_SET(in_path) THEN in_path = 'in_data/'
IF NOT KEYWORD_SET(out_path) THEN out_path = 'out_data/'

;Prepare the date
IF KEYWORD_SET(date_it) THEN BEGIN
  text2 = "['" + date_it[0] + "','" + date_it[1] + "','" + date_it[2] + $
          "','" + date_it[3] + "','" + date_it[4] + "']" 
  data_folder = 0
  info_date = 0
ENDIF ELSE text2 = 0

IF NOT KEYWORD_SET(list_free_params) THEN list_free_params = FIX([1,1,1,0,1,1,1,1,1,0])

IF KEYWORD_SET(invert_ff) THEN list_free_params[9] = 1 $
ELSE invert_ff = 0

list_free_params = STRING(list_free_params,format='(i1)')
text4 = "['" + list_free_params[0] + "','" + list_free_params[1] + $
       "','" + list_free_params[2] + "','" + list_free_params[3] + $
       "','" + list_free_params[4] + "','" + list_free_params[5] + $
       "','" + list_free_params[6] + "','" + list_free_params[7] + $
       "','" + list_free_params[8] + "','" + list_free_params[9] + "']"

IF NOT KEYWORD_SET(guess) THEN guess = [15.,90.,45.,0.5,50.,150.,0.,'2400.','3600.',1.]
IF KEYWORD_SET(ff_value) THEN guess[9] = ff_value
guess = STRING(guess,format='(f8.3)')
text5 = "['" + guess[0] + "','" + guess[1] + "','" + guess[2] + "','" + guess[3] + $
       "','" + guess[4] + "','" + guess[5] + "','" + guess[6] + "','" + guess[7] + $
       "','" + guess[8] + "','" + guess[9] + "']"

IF KEYWORD_SET(deconv) THEN deconv = 1 ELSE deconv = 0
IF NOT KEYWORD_SET(num_lambdas) THEN num_lambdas = 6

FOR i=1,num_proc DO BEGIN
  IF num_proc EQ 1 THEN folder = '.' $
  ELSE BEGIN
    IF KEYWORD_SET(suffix) THEN folder = 'part' + STRTRIM(i,2) + '_' +  STRTRIM(suffix,2) $
    ELSE folder = 'part' + STRTRIM(i,2)
    FILE_MKDIR,folder
  ENDELSE

  value1 = yini + ycoor[i-1] 
  value2 = yini + ycoor[i] - 1 
  text3 = "['" + STRTRIM(xini,2) + "','" + STRTRIM(value1,2) + $
          "','" + STRTRIM(xfin,2) + "','" + STRTRIM(value2,2) + "']"

  OPENW,unit1,folder + '/run_inversions.pro', /GET_LUN
    PRINTF,unit1,'PRO run_inversions'

      PRINTF,unit1,'vfisv_invert,' + STRTRIM(text1,2) + $
             ',in_path="' + STRTRIM(in_path,2) + '"' +  $
             ',out_path="' + STRTRIM(out_path,2) + '"' +  $
             ',data_folder="' + STRTRIM(data_folder,2) + '"' + $
             ",info_date='" + STRTRIM(info_date,2) + "'" + $
             ',date_it=' + STRTRIM(text2,2) + $
             ',points=FIX(' + STRTRIM(text3,2) + ')' + $
             ',invert_ff=' + STRTRIM(invert_ff,2) + $
             ',list_free_params=FIX(' + STRTRIM(text4,2) + ')' + $
             ',guess=DOUBLE(' + STRTRIM(text5,2) + ')' + $
             ',num_lambdas=' + STRTRIM(num_lambdas,2) + $
             ',deconv=' + STRTRIM(deconv,2)
             
    PRINTF,unit1,'END'  
  CLOSE,unit1
  FREE_LUN,unit1

ENDFOR


END


