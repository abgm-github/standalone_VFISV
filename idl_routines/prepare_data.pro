FUNCTION prepare_data,path,in_path,out_path,text,option, $
                      cut=cut,points=points,single_pixel=single_pixel,date=date

maps = read_stokes_hmi(path,option,cut=cut,points=points,single_pixel=single_pixel, $
                       date=date)
xini = maps.xini
xfin = maps.xfin
yini = maps.yini
yfin = maps.yfin
header = maps.header
hmi_data = maps.hmi

file = 'hmidata_ready4vfisv_' + STRTRIM(text,2) + '_' + $
       STRTRIM(xini,2) + '_' + STRTRIM(xfin,2) + '_' + $
       STRTRIM(yini,2) + '_' + STRTRIM(yfin,2) + '.sav'

savename = out_path + STRJOIN((STRSPLIT(file,'.',/extract))[0:-2],'.')

SAVE,file=STRTRIM(in_path,2) + file,hmi_data

out = {xini:xini,xfin:xfin,yini:yini,yfin:yfin, $
       savename:savename,file:file}

RETURN,out


END

