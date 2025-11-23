FC = gfortran              # specifies Fortran compiler
FCFLAGS = -O3

all: mmf

mmf: main.o mm.o
	$(FC) -o $@ $^

mm.o: mm.f90
	$(FC) -c $(FCFLAGS) $<

main.o: main.f90
	$(FC) -c $(FCFLAGS) $<

clean:
	rm -f *.o *.mod mmf

