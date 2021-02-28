PRO program

vfisv_path = '../../src/'
yy = '2012'
mo = '09'
dd = '24'
hh = '19'
mi = '00'
ss = '00'
info_date = yy+'.'+mo+'.'+dd+'_'+hh+':'+mi+':'+ss+'_TAI'

guess = DOUBLE([1.89, 170.09, 10.96, 0.50, 35.21, 1787.39, 40222.11, 7845.79, 39183.32, 0.64])
date = [yy,mo,dd,hh,mi]
points = ['1893','2090','1893','2090']   ;1 pixel
num_lambdas = 6

vfisv_invert,vfisv_path,synthesis=1,info_date=info_date,points=points, $
             guess=guess,num_lambdas=num_lambdas

END
