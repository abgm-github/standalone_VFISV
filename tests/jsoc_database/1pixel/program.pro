PRO program

vfisv_path = '../../../src/'
yy = '2012'
mo = '09'
dd = '24'
hh = '19'
mi = '00'
ss = '00'

date = [yy,mo,dd,hh,mi]

points = ['2615','2031','2615','2031']   ;1 pixel
invert_ff = 1

pol_low = 0.01d0
pol_upp = 0.05d0

vfisv_invert,vfisv_path,invert_ff=invert_ff,points=points,date_it=date, $
             pol_low=pol_low,pol_upp=pol_upp;,/see

END
