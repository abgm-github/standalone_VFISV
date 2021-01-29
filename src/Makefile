FC = /usr/local/bin/ifort 
IFC = -xavx
#FC = /usr/local/bin/gfortran
#CC = gcc
#IFC = -O3

#F90F1 = -c -O0 -g -fbacktrace
#F90F2 = -c -O0 -g -fbacktrace -DBYTE_SWAPPED=1
#F77F1 = -c -O0 -g -fbacktrace
#F77F2 = -c -O0 -g -fbacktrace -r8
F90F1 = -c $(IFC)
#F90F2 = -c -O3 -DBYTE_SWAPPED=1
F77F1 = -c $(IFC)
#F77F2 = -c -O3 -r8
CF = -c

LIBS = -L/homed/abgm/libraries/src/lapack/lapack-3.9.0/ -llapack -lblas

LDFLAGS =

OBJ = \
        cons_param.o \
        voigt.o \
        inv_param.o \
        lim_init.o \
        line_param.o \
        ran_mod.o \
        svbksb.o \
        voigt_data.o \
        voigt_init.o \
        voigt_taylor.o \
        wave_init.o \
        filt_param.o \
        free_init.o \
        free_memory.o \
        inv_init.o \
        inv_utils.o \
        line_init.o \
        wfa_guess.o \
        change_var.o \
        filt_init.o \
        forward.o \
        invert.o \
        vfisv.o

cons_param.o: cons_param.f90
	$(FC) $(F90F1) cons_param.f90

voigt.o: voigt.f
	$(FC) $(F90F1) voigt.f

inv_param.o: inv_param.f90 cons_param.o
	$(FC) $(F90F1) inv_param.f90

lim_init.o: lim_init.f90 inv_param.o
	$(FC) $(F90F1) lim_init.f90

line_param.o: line_param.f90 cons_param.o
	$(FC) $(F90F1) line_param.f90

ran_mod.o: ran_mod.f90 cons_param.o
	$(FC) $(F90F1) ran_mod.f90

svbksb.o: svbksb.f90 cons_param.o
	$(FC) $(F90F1) svbksb.f90

voigt_data.o: voigt_data.f90 cons_param.o
	$(FC) $(F90F1) voigt_data.f90

voigt_init.o: voigt_init.f90 cons_param.o voigt_data.o
	$(FC) $(F90F1) voigt_init.f90

voigt_taylor.o: voigt_taylor.f90 cons_param.o voigt_data.o line_param.o
	$(FC) $(F90F1) voigt_taylor.f90

wave_init.o: wave_init.f90 cons_param.o line_param.o
	$(FC) $(F90F1) wave_init.f90

filt_param.o: filt_param.f90 cons_param.o line_param.o
	$(FC) $(F90F1) filt_param.f90

free_init.o: free_init.f90 inv_param.o filt_param.o
	$(FC) $(F90F1) free_init.f90

free_memory.o: free_memory.f90 filt_param.o cons_param.o line_param.o \
        inv_param.o
	$(FC) $(F90F1) free_memory.f90

inv_init.o: inv_init.f90 cons_param.o inv_param.o
	$(FC) $(F90F1) inv_init.f90

inv_utils.o: inv_utils.f90 cons_param.o filt_param.o line_param.o inv_param.o \
        ran_mod.o
	$(FC) $(F90F1) inv_utils.f90

line_init.o: line_init.f90 cons_param.o line_param.o
	$(FC) $(F90F1) line_init.f90

wfa_guess.o: wfa_guess.f90 cons_param.o inv_param.o filt_param.o line_param.o
	$(FC) $(F90F1) wfa_guess.f90

change_var.o: change_var.f90 cons_param.o filt_param.o
	$(FC) $(F90F1) change_var.f90

filt_init.o: filt_init.f90 filt_param.o line_param.o cons_param.o
	$(FC) $(F90F1) filt_init.f90

forward.o: forward.f90 filt_param.o line_param.o cons_param.o change_var.o \
        inv_param.o voigt_data.o
	$(FC) $(F90F1) forward.f90

invert.o: invert.f90 filt_param.o cons_param.o line_param.o inv_param.o \
        inv_utils.o forward.o ran_mod.o change_var.o
	$(FC) $(F90F1) invert.f90

vfisv.o: vfisv.f90 filt_param.o inv_utils.o forward.o
	$(FC) $(F90F1) vfisv.f90


vfisv: $(OBJ)
	$(FC) $(OBJ) $(LDFLAGS) $(LIBS) -o vfisv.x

clean:
	rm -f *.o *.x *.mod
