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

# Choose Vector ISA
ifeq ($(VECTOR_ISA),avx2)
	GNUISA=-mtune=avx2
	CLANGISA=-mtune=avx2
	INTELISA=-xAVX2
	PGIISA=-tp=haswell
else ifeq ($(VECTOR_ISA),avx512)
	GNUISA=-mtune=skylake-avx512 -mprefer-vector-width=512
	CLANGISA=-mtune=skylake-avx512 -mprefer-vector-width=512
	INTELISA=-xCORE-AVX512 -qopt-zmm-usage=high
	PGIISA=-tp=skylake
else ifeq ($(VECTOR_ISA),knl)
	GNUISA=-mtune=knl
	CLANGISA=-mtune=knl
	INTELISA=-xMIC-AVX512
	PGIISA=-tp=knl
else ifeq ($(VECTOR_ISA),altivec)
	GNUISA=-mcpu=power8
	CLANGISA=-mcpu=power8
	PGIISA=
else
	# NO ISA specified - use native one
	VECTOR_ISA=native
	GNUISA=-mtune=native
	INTELISA=-xHost
	PGIISA=-tp=native
endif

# Choose compiler toolchain - gnu by default
ifndef $(TEST_COMPILER)
	TEST_COMPILER := gnu
endif
IDSTRING := $(TEST_COMPILER)_$(VECTOR_ISA)_
ifeq ($(TEST_COMPILER),gnu)
	CC=g++
	CFLAGS=-O3 $(GNUISA) -std=c++11
	UNSAFE=-ffast-math
	NOVECFLAG=-fno-tree-vectorize
	REPORT=-fopt-info-optimized=$(IDSTRING)report.txt
	ASM=-S -o $(IDSTRING)
else ifeq ($(TEST_COMPILER),intel)
	CC=icpc 
	CFLAGS=-O3 $(INTELISA) -std=c++11
	UNSAFE=-fp-model fast=2
	NOVECFLAG=-no-simd -no-vec
	REPORT=-qopt-report=5 -qopt-report-file=$(IDSTRING)report.txt
	ASM=-S
else ifeq ($(TEST_COMPILER),nvidia)
	CC=nvc++
	CFLAGS=-O3 $(PGIISA) --c++11
	UNSAFE=-fast -fastsse
	NOVECFLAG=-Mnovect
	REPORT=-Minfo=all
	ASM=-S
else ifeq ($(TEST_COMPILER),ibm)
	CC=xlc++
	CFLAGS=-O3 -std=c++11
	UNSAFE= -qhot=fastmath
	NOVECFLAG=-qnoaltivec -qhot=novector -qsimd=noauto
	REPORT=-D
	ASM= -S
else ifeq ($(TEST_COMPILER),clang)
	CC=clang++
	CFLAGS=-O3 $(CLANGISA) -std=c++11 -lm -lstdc++
	UNSAFE=-ffast-math
	NOVECFLAG=-fno-vectorize
	REPORT=-fsave-optimization-record -foptimization-record-file=$(IDSTRING)report.txt
	ASM=-S
else
	$(error Unrecognized compiler $(TEST_COMPILER))
endif


# Disable vectorization if requested
ifndef $(NOVEC)
	NOVEC := no
endif

ifeq ($(NOVEC),yes)
	CFLAGS += $(NOVECFLAG)
	VECSTRING := novec
else
	VECSTRING := vec
endif

# Add runtime-invariants manual specialization 
ifndef $(SPECIALIZE)
	SPECIALIZE := no
endif

ifeq ($(SPECIALIZE),yes)
	CFLAGS += -DSPECIALIZE
	SPECSTRING := specialized
else
	SPECSTRING := rt
endif


ifndef PREFIX
PREFIXCMD ?=
else
PREFIXCMD := $(PREFIX) --
EXTRALDFLAGS = -I/home/sergi/workspace/phd/doping/bin /home/sergi/workspace/phd/doping/bin/libdoping.so -rdynamic -ldl
PREFSTRING = _doping
endif

NAME=$(APP)_$(VECSTRING)_$(SPECSTRING)$(PREFSTRING)

compileall:
	#make compile NOVEC=yes SPECIALIZE=yes
	#make compile NOVEC=yes SPECIALIZE=no
	${MAKE} compile NOVEC=no SPECIALIZE=no TEST_COMPILER=$(TEST_COMPILER) VECTOR_ISA=$(VECTOR_ISA)
	${MAKE} compile NOVEC=no SPECIALIZE=yes TEST_COMPILER=$(TEST_COMPILER) VECTOR_ISA=$(VECTOR_ISA)
	${MAKE} compile PREFIX=dope NOVEC=no SPECIALIZE=no TEST_COMPILER=$(TEST_COMPILER) VECTOR_ISA=$(VECTOR_ISA)

compiledoping:
	${MAKE} compile PREFIX=dope NOVEC=no SPECIALIZE=no

assembly:
	${CC} ${CFLAGS} -c ${ASM}$(NAME).s ${APP}_serial.cpp

compile:
	$(PREFIXCMD) ${CC} ${CFLAGS} ${REPORT} -I. -c -o ${NAME}.o ${APP}_serial.cpp
	${CC} ${CFLAGS} -o ${NAME}.exe ${NAME}.o ${APP}.cpp $(EXTRALDFLAGS)

run:
	./${NAME}.exe

rundoping:
	${MAKE} run PREFIX=dope NOVEC=no SPECIALIZE=no

runrt:
	${MAKE} run NOVEC=no SPECIALIZE=no

runct:
	${MAKE} run NOVEC=no SPECIALIZE=yes

runall:
	${MAKE} run NOVEC=no SPECIALIZE=no
	${MAKE} run NOVEC=no SPECIALIZE=yes
	${MAKE} run PREFIX=dope NOVEC=no SPECIALIZE=no

clean:
	rm -rf *.ppm *.o *.optrpt gnu_* *.exe *.doping.cpp doping_loop_* *report.txt
