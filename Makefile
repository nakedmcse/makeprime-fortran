all: fmlib.a makeprime

fmlib.a: fmlib/fm.f95 fmlib/fmzm90.f95 fmlib/fmsave.f95
	gfortran -o fmsave.o -c -O2 fmlib/fmsave.f95
	gfortran -o fmzm90.o -c -O2 fmlib/fmzm90.f95
	gfortran -o fm.o -c -O2 fmlib/fm.f95
	ar rv fmlib.a fm*.o

makeprime: makeprime.f95 fmlib.a
	gfortran -o makeprime -O2 makeprime.f95 fmlib.a

clean:
	rm -f fmlib.a
	rm -rf fm*.o
	rm -f *.mod
	rm -f makeprime
