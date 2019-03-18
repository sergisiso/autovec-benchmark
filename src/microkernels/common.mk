
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
