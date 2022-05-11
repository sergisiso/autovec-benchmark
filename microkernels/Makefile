# Configurable parameters
TEST_COMPILER ?= intel # gcc intel gpi ibm clang
VECTOR_ISA ?= avx2 # avx2 avc512 knl altivec (native by default)


MCKERNELS := ao binomial black-scholes convolution mandelbrot stencil lattice_boltzmann

TOPTARGETS := compileall compiledoping runall runrt runct rundoping clean

.PHONY: $(MCKERNELS)

$(TOPTARGETS): $(MCKERNELS)

$(MCKERNELS):
	$(MAKE) -C $@ $(MAKECMDGOALS) TEST_COMPILER=$(TEST_COMPILER) VECTOR_ISA=$(VECTOR_ISA)

