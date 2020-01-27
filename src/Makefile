# Copyright (c) 2019 Sergi Siso
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#   1. Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#   2. Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
#   3. Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
# GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
# OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

CC=clang -g
CFLAGS = -O3 -ffast-math #-std=c99 -O3 -fivopts -flax-vector-conversions -funsafe-math-optimizations -lm
vecflags = #-msse4.2 #-ftree-vectorizer-verbose=1
novecflags = #-fno-tree-vectorize

alltests += -DCONTROL_FLOW
#alltests = -DLINEAR_DEPENDENCE -DINDUCTION_VARIABLE
#alltests += -DGLOBAL_DATA_FLOW -DCONTROL_FLOW -DSYMBOLICS -DSTATEMENT_REORDERING -DLOOP_RESTRUCTURING
#alltests += -DNODE_SPLITTING -DEXPANSION -DCROSSING_THRESHOLDS -DREDUCTIONS -DRECURRENCES -DSEARCHING
#alltests += -DPACKING
#alltests += -DLOOP_REROLLING -DEQUIVALENCING -DINDIRECT_ADDRESSING -DCONTROL_LOOPS

params = 
params += -DRUNTIME_LOOP_BOUNDS_PARAMETERS
params += -DRUNTIME_ARITHMETIC_PARAMETERS
params += -DRUNTIME_INDEX_PARAMETERS
params += -DCONDITION_EVAL_PARAMETERS
params += -DVARIABLE_ATTRIBUTES

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
