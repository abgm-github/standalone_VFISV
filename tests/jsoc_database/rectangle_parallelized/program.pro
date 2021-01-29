PRO program

vfisv_path = '../../../../src/'
yy = '2012'
mo = '09'
dd = '24'
hh = '19'
mi = '00'
ss = '00'

date = [yy,mo,dd,hh,mi]

points = [2580,1950,2620,2000]  

num_proc = 4

prepare_data_to_invert_in_parallel,num_proc=num_proc,vfisv_path=vfisv_path,date_it=date,points=points

END
