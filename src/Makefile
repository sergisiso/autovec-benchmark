
CC=gcc
CFLAGS = -std=c99 -O3 -fivopts -flax-vector-conversions -funsafe-math-optimizations -lm
vecflags = -msse4.2 -ftree-vectorizer-verbose=1
novecflags = -fno-tree-vectorize

alltests = -DLINEAR_DEPENDENCE -DINDUCTION_VARIABLE
alltests += -DGLOBAL_DATA_FLOW -DCONTROL_FLOW -DSYMBOLICS -DSTATEMENT_REORDERING -DLOOP_RESTRUCTURING
alltests += -DNODE_SPLITTING -DEXPANSION -DCROSSING_THRESHOLDS -DREDUCTIONS -DRECURRENCES -DSEARCHING
alltests += -DPACKING -DLOOP_REROLLING -DEQUIVALENCING -DINDIRECT_ADDRESSING -DCONTROL_LOOPS

params = 
params += -DRUNTIME_LOOP_BOUNDS_PARAMETERS
params += -DRUNTIME_ARITHMETIC_PARAMETERS
params += -DRUNTIME_INDEX_PARAMETERS
params += -DCONDITION_EVAL_PARAMETERS

all : runvec runnovec runrtvec runrtnovec

runnovec : tscnovec.o dummy.o
	$(CC) dummy.o tscnovec.o $(CFLAGS) -o runnovec

runvec : tscvec.o dummy.o
	$(CC) $(CFLAGS) dummy.o tscvec.o $(CFLAGS) -o runvec

runrtvec: tscrtvec.o dummy.o
	$(CC) $(CFLAGS) dummy.o tscrtvec.o $(CFLAGS) -o runrtvec

runrtnovec: tscrtnovec.o dummy.o
	$(CC) $(CFLAGS) dummy.o tscrtnovec.o $(CFLAGS) -o runrtnovec

tscrtvec.o: tsc_runtime.c
	$(CC) $(CFLAGS) $(vecflags) $(alltests) $(params) -c -o tscrtvec.o tsc_runtime.c

tscrtnovec.o : tsc_runtime.c
	$(CC) $(CFLAGS) $(novecflags) $(alltests) $(params) -c -o tscrtnovec.o tsc_runtime.c

tscvec.o : tsc_runtime.c
	$(CC) $(CFLAGS) $(vecflags) $(alltests) -c -o tscvec.o tsc_runtime.c

tscnovec.o : tsc_runtime.c
	$(CC) $(CFLAGS) $(novecflags) $(alltests) -c -o tscnovec.o tsc_runtime.c

dummy.o : dummy.c
	$(CC) -O0 -c dummy.c

clean :
	rm -f *.o runnovec runvec runrtnovec runrtvec report*.txt *.s
