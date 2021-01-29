PRO program

vfisv_path = '../../../src/'
yy = '2012'
mo = '09'
dd = '24'
hh = '19'
mi = '00'
ss = '00'

data_folder = '../data/'
info_date = yy+'.'+mo+'.'+dd+'_'+hh+':'+mi+':'+ss+'_TAI'

points = ['2595','2030','2595','2030']   ;1 pixel
invert_ff = 1 

vfisv_invert,vfisv_path,invert_ff=invert_ff,points=points, $
      data_folder=data_folder,info_date=info_date

END
