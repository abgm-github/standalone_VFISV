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

points = [2580,1950,2620,2000]

vfisv_invert,vfisv_path,points=points, $
       data_folder=data_folder,info_date=info_date

END
