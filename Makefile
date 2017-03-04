
CC=gcc
CFLAGS = -std=c99 -O3 -fivopts -flax-vector-conversions -funsafe-math-optimizations 
vecflags = -msse4.2 -ftree-vectorizer-verbose=1
novecflags = -fno-tree-vectorize

libs = -lm
noopt = -O0

alltests = -DLINEAR_DEPENDENCE -DINDUCTION_VARIABLE

#alltests := -DGLOBAL_DATA_FLOW -DCONTROL_FLOW -DSYMBOLICS -DSTATEMENT_REORDERING -DLOOP_RESTRUCTURING
#alltests := -DNODE_SPLITTING -DEXPANSION -DCROSSING_THRESHOLDS -DREDUCTIONS -DRECURRENCES -DSEARCHING
#alltests := -DPACKING -DLOOP_REROLLING -DEQUIVALENCING -DINDIRECT_ADDRESSING -DCONTROL_LOOPS

all : runvec runnovec runrtvec runrtnovec

runnovec : tscnovec.o dummy.o
	$(CC) $(noopt) dummy.o tscnovec.o -o runnovec $(libs)

runvec : tscvec.o dummy.o
	$(CC) $(noopt) dummy.o tscvec.o -o runvec $(libs)

runrtvec: tscrtvec.o dummy.o
	$(CC) $(noopt) dummy.o tscrtvec.o -o runrtvec $(libs)

runrtnovec: tscrtnovec.o dummy.o
	$(CC) $(noopt) dummy.o tscrtnovec.o -o runrtnovec $(libs)

tscvec.o : tsc.c
	rm -f report.txt
	$(CC) $(CFLAGS) $(vecflags) $(alltests) -c -o tscvec.o tsc.c  2> report.txt

tscrtvec.o: tsc_runtime.c
	rm -f reportrt.txt
	$(CC) $(CFLAGS) $(vecflags) $(alltests) -c -o tscrtvec.o tsc_runtime.c  2> reportrt.txt

tscnovec.o : tsc.c
	$(CC) $(CFLAGS) $(novecflags) $(alltests) -c -o tscnovec.o tsc.c

tscrtnovec.o : tsc_runtime.c
	$(CC) $(CFLAGS) $(novecflags) $(alltests) -c -o tscrtnovec.o tsc_runtime.c

tsc.s : tsc.c dummy.o
	$(CC) $(CFLAGS) dummy.o tsc.c -S 

dummy.o : dummy.c
	$(CC) -c dummy.c

clean :
	rm -f *.o runnovec runvec runrtnovec runrtvec report*.txt *.s
