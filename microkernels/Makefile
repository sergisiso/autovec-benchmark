# Configurable parameters
TEST_COMPILER ?= gnu # intel gpi ibm clang
VECTOR_ISA ?= # avx2 avc512 knl altivec (native by default)


MCKERNELS := black-scholes convolution mandelbrot stencil lattice_boltzmann
# ao binomial

TOPTARGETS := compileall compiledoping runall runrt runct rundoping clean

.PHONY: $(MCKERNELS)

$(TOPTARGETS): $(MCKERNELS)

$(MCKERNELS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

