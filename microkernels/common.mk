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
	GNUISA=-march=skylake
	CLANGISA=-march=skylake
	INTELISA=AVX2
	PGIISA=-tp=haswell
else ifeq ($(VECTOR_ISA),avx512)
	GNUISA=-march=skylake-avx512 -mprefer-vector-width=512
	CLANGISA=-march=skylake-avx512
	INTELISA=CORE-AVX512 -qopt-zmm-usage=high
	PGIISA=-tp=skylake
else ifeq ($(VECTOR_ISA),knl)
	GNUISA=-march=knl
	CLANGISA=-march=knl
	INTELISA=MIC-AVX512
	PGIISA=-tp=knl
else ifeq ($(VECTOR_ISA),altivec)
	GNUISA=-mcpu=power8
	CLANGISA=-mcpu=power8
	PGIISA=
else
	# NO ISA specified
	#$(info asdf)
	GNUISA=-march=native
	INTELISA=Host
endif


# Choose compiler toolchain
ifeq ($(TEST_COMPILER),gnu)
	CC=g++
	CFLAGS=-O3 $(GNUISA) -std=c++11
	UNSAFE=-ffast-math
	NOVEC=-fno-tree-vectorize
	REPORT=-fopt-info-optimized=optreports/gcc_$(VECTOR_ISA)_
	ASM=-S
else ifeq ($(TEST_COMPILER),intel)
	CC=icpc 
	CFLAGS=-O3 -x$(INTELISA) -std=c++11
	UNSAFE=-fp-model fast=2
	NOVEC=-no-simd -no-vec
	REPORT=-qopt-report=5 -qopt-report-file=optreports/intel_$(VECTOR_ISA)_
	ASM=-S
else ifeq ($(TEST_COMPILER),pgi)
	CC=pgc++
	CFLAGS=-O3 $(PGIISA) --c++11
	UNSAFE=-fast -fastsse
	NOVEC=-Mnovect
	REPORT=-Minfo=all -D
	ASM=-S
else ifeq ($(TEST_COMPILER),ibm)
	CC=xlc++
	CFLAGS=-O3 -std=c++11
	UNSAFE= -qhot=fastmath
	NOVEC=-qnoaltivec -qhot=novector:fastmath -qsimd=noauto
	REPORT=-D
	ASM= -S
else ifeq ($(TEST_COMPILER),clang)
	CC=clang
	CFLAGS=-O3 $(CLANGISA) -std=c++11 -lm -lstdc++
	UNSAFE=-ffast-math
	NOVEC=-fno-vectorize
	REPORT=-fsave-optimization-record -foptimization-record-file=optreports/clang_$(VECTOR_ISA)_
	ASM=-S
else
	CC=icpc
	CFLAGS=-O3 -xHost -std=c++11
	UNSAFE=-fp-model fast=2 -prec-sqrt -ftz -fma
	NOVEC=-no-simd -no-vec
	REPORT=-qopt-report=5
	ASM=-Fa
	#$(error No compiler specified)
	# NO COMPILER SPECIFIED
endif
