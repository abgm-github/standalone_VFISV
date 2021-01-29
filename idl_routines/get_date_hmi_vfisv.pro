FUNCTION get_date_hmi_vfisv,hd=hd,date=date,split1=split1,split2=split2,split3=split3

IF KEYWORD_SET(hd) THEN BEGIN
  element = WHERE(STRPOS(hd, 'DATE-OBS') EQ 0)
  date = STRMID(hd[element],STRPOS(hd[element],'=')+3)
  split1 = 'T'
  split2 = '-'
  split3 = ':'
ENDIF

IF KEYWORD_SET(date) AND (NOT KEYWORD_SET(split1) $
   OR NOT KEYWORD_SET(split2) OR NOT KEYWORD_SET(split3)) THEN BEGIN
   PRINT,''
   PRINT,'*****************************************'
   PRINT,''
   PRINT,'You have to specify the separator pattern'
   PRINT,''
   PRINT,'Ending...'
   PRINT,''
   PRINT,'*****************************************'
   PRINT,''
   STOP
ENDIF

date_split = STRSPLIT(date,split1,/extra)

year = (STRSPLIT(date_split[0],split2,/extra))[0]
month = (STRSPLIT(date_split[0],split2,/extra))[1]
day = (STRSPLIT(date_split[0],split2,/extra))[2]

hour = (STRSPLIT(date_split[1],split3,/extra))[0]
minute = (STRSPLIT(date_split[1],split3,/extra))[1]
second = (STRSPLIT(date_split[1],split3,/extra))[2]

julian = JULDAY(month,day,year,hour,minute,second)

out = {date:date,year:year,month:month,day:day,hour:hour, $
       minute:minute,second:second,julian:julian}

RETURN,out

END
