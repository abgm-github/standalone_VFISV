PRO program

vfisv_path = '../../../src/'
yy = '2012'
mo = '09'
dd = '24'
hh = '19'
mi = '00'
ss = '00'

date = [yy,mo,dd,hh,mi]

points = ['2595','2030','2595','2030']   ;1 pixel
invert_ff = 1

vfisv_invert,vfisv_path,invert_ff=invert_ff,points=points,date_it=date;,/see

END
