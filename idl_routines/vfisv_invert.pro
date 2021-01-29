PRO vfisv_invert,vfisv_path,in_path=in_path,out_path=out_path, $
                 data_folder=data_folder,info_date=info_date, $
                 date_it=date_it, $
                 cut=cut,single_pixel=single_pixel,points=points, $
                 ff_value=ff_value,invert_ff=invert_ff, $
                 list_free_params=list_free_params,guess=guess, $
                 num_lambdas=num_lambdas,synthesis=synthesis, $
                 deconv=deconv,see=see,print_parametes=print_parameters


; points is defined as: points = [x0,y0,x1,y1]

IF (NOT FILE_TEST(vfisv_path+'vfisv.x')) THEN BEGIN
  PRINT,''
  PRINT,'********************************************'
  PRINT,'The vfisv.x file does not exist... Ending...'
  PRINT,'********************************************'
  PRINT,''
  RETURN
ENDIF

IF NOT KEYWORD_SET(synthesis) THEN BEGIN
  IF KEYWORD_SET(cut) AND KEYWORD_SET(points) OR $
     KEYWORD_SET(cut) AND KEYWORD_SET(single_pixel) OR $
     KEYWORD_SET(points) AND KEYWORD_SET(single_pixel) THEN BEGIN
  
    PRINT,''
    PRINT,'*********************************************************************************'
    PRINT,''
    PRINT,'You have to choose ONLY one option between "cut", "points" and "single_pixel"'
    PRINT,''
    PRINT,'Ending...'
    PRINT,''
    PRINT,'*********************************************************************************'
    PRINT,''
    RETURN
  ENDIF
ENDIF ELSE BEGIN
  IF NOT KEYWORD_SET(points) OR NOT KEYWORD_SET(info_date) OR NOT KEYWORD_SET(guess) THEN BEGIN
    PRINT,''
    PRINT,'*********************************************************************************'
    PRINT,''
    PRINT,'The filters used to synthesize an atmosphere depend on the observation day and '
    PRINT,'on the location of the pixel in the disk, so the keywords "info_date" and "points" '
    PRINT,'are mandatory to synthesize'
    PRINT,''
    PRINT,'The "guess" keyword is mandatory, since it corresponds to the atmosphere to synthesize'
    PRINT,''
    PRINT,'Ending...'
    PRINT,''
    PRINT,'*********************************************************************************'
    PRINT,''
    RETURN
  ENDIF
ENDELSE

IF NOT KEYWORD_SET(in_path) THEN in_path = 'in_data/'
IF NOT KEYWORD_SET(out_path) THEN out_path = 'out_data/'
IF (NOT FILE_TEST(in_path)) THEN FILE_MKDIR,in_path
IF (NOT FILE_TEST(out_path)) THEN FILE_MKDIR,out_path

;;IF NOT KEYWORD_SET(synthesis) THEN synthesis = 0 

IF NOT KEYWORD_SET(num_lambdas) THEN num_lambdas = 6 

IF NOT KEYWORD_SET(list_free_params) THEN $
   list_free_params = FIX([1,1,1,0,1,1,1,1,1,0])

IF KEYWORD_SET(invert_ff) THEN list_free_params[9] = 1

IF KEYWORD_SET(ff_value) THEN guess[9] = ff_value


; THERE 2 OPTIONS:
; 1) LOOKING FOR THE DATA IN JSOC NETWORK: date_it keyword  /  option=1
; 2) GIVING THE FOLDER WHERE TO FIND THE DATA: data_folder  /  option=2

IF NOT KEYWORD_SET(synthesis) THEN BEGIN
IF KEYWORD_SET(date_it) THEN BEGIN
  option = 1

  text = STRING(STRTRIM(date_it[0],2) + $
                STRTRIM(date_it[1],2) + $ 
                STRTRIM(date_it[2],2) + '_' + $
                STRTRIM(date_it[3],2) + $
                STRTRIM(date_it[4],2) + '00_TAI')
  
  IF KEYWORD_SET(deconv) THEN text = text + '_dconS' 

  text_lookfor = STRING(STRTRIM(date_it[0],2) + '.' + $
                 STRTRIM(date_it[1],2) + '.' + $ 
                 STRTRIM(date_it[2],2) + '_' + $
                 STRTRIM(date_it[3],2) + ':' + $
                 STRTRIM(date_it[4],2) + ':00_TAI')
  
  IF KEYWORD_SET(deconv) THEN text_data = 'S_720s_dconS' $
  ELSE text_data = 'S_720s'

  SPAWN,'show_info ds=hmi.' + STRTRIM(text_data,2) + '"[' + $
        STRTRIM(text_lookfor,2) + ']" -P',path 
  path = path[1:*]

  date = text_lookfor

ENDIF ELSE BEGIN
  IF KEYWORD_SET(data_folder) THEN BEGIN
    option = 2

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
    path = data_folder 
    name = FILE_SEARCH(data_folder + '*I0*')
    image = FITSIO_READ_IMAGE(name[0],hd)
    text = (STRSPLIT(name,'.',/extract))[2]

    date = info_date

  ENDIF ELSE BEGIN
    PRINT,''
    PRINT,'****************************************************************'
    PRINT,''
    PRINT,'It is mandatory to active the keyword "date_it" or "data_folder"'
    PRINT,''
    PRINT,'Ending...'
    PRINT,''
    PRINT,'****************************************************************'
    RETURN

  ENDELSE
ENDELSE



  IF NOT KEYWORD_SET(cut) AND NOT KEYWORD_SET(points) AND NOT KEYWORD_SET(single_pixel) THEN BEGIN
    PRINT,''
    PRINT,'*********************************************************************************'
    PRINT,''
    PRINT,'If you do not active the keywords "cut", "points" or "single_pixel",'
    PRINT,'the code will invert the full map (4096 * 4096 pixels)'
    PRINT,''
    answer = ''
    WHILE STRUPCASE(answer) NE 'N' AND STRUPCASE(answer) NE 'Y' DO BEGIN
      READ, answer, PROMPT='Do you want to continue (y,Y / n,N): '
      PRINT,''
      IF STRUPCASE(answer) EQ 'N' THEN BEGIN
        PRINT,'Ending...'
        PRINT,''
        PRINT,'*********************************************************************************'
        PRINT,''
        RETURN
      ENDIF
    ENDWHILE
    PRINT,'*********************************************************************************'
    PRINT,''
  ENDIF

  params = prepare_data(path,in_path,out_path,text,option, $
                     cut=cut,points=points,single_pixel=single_pixel,date=text_lookfor)
  xini = params.xini
  xfin = params.xfin
  yini = params.yini
  yfin = params.yfin
  savename = params.savename
  file = params.file

  IF NOT KEYWORD_SET(guess) THEN $
     guess = DOUBLE([15.d0,90.d0,45.d0,0.5d0,50.d0,150.d0,0.d0,2400.d0,3600.d0,1.d0])

  PRINT,''
  PRINT,'==================================================================='
  PRINT,''
  IF option EQ 1 THEN BEGIN
    PRINT,'Looking for the data in JSOC  database'
    PRINT,'Date: ',text_lookfor 
  ENDIF ELSE BEGIN
    IF option EQ 2 THEN BEGIN
    PRINT,'Looking for the data in the computer'
    PRINT,'Data Folder: ',data_folder
    ENDIF
  ENDELSE
  PRINT,'Points [ x0 : x1 , y0 : y1 ]:         [ ' + STRTRIM(points[0],2) + ' : ' + STRTRIM(points[2],2) + ' , ' + $
        STRTRIM(points[1],2) + ' : ' + STRTRIM(points[3],2) + ' ]'
  PRINT,''
  num_pix = (FLOAT(points[2])-FLOAT(points[0])+1) * (FLOAT(points[3])-FLOAT(points[1])+1)
  PRINT,'Number of pixel to be processed: ',STRTRIM(STRING(num_pix,format='(i)') ,2)
  PRINT,''
  PRINT,'==================================================================='
  PRINT,''

ENDIF ELSE BEGIN

  file = 'synthesis_' + STRTRIM(num_lambdas,2) + '_filters.sav'
  savename = STRTRIM(out_path,2) + 'synthesis_' + STRTRIM(num_lambdas,2) + '_filters.sav'

  date = info_date
  xini = points[0]
  yini = points[1]
  xfin = xini
  yfin = yini
  hmi_data = 0
  SAVE,file=STRTRIM(in_path,2) + file,hmi_data

ENDELSE

run_vfisv,file,savename,in_path,out_path,vfisv_path, $
          xini,yini,date,header, $
          stokes_data=stokes_data, $
          see=see,print_parameters=print_parameters, $
          list_free_params=list_free_params,guess=guess, $
          num_lambdas=num_lambdas,synthesis=synthesis

END
