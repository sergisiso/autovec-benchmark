# Configurable parameters
TEST_COMPILER ?= gnu # intel gpi ibm clang
VECTOR_ISA ?= # avx2 avc512 knl altivec (native by default)


MCKERNELS := binomial convolution \
	mandelbrot stencil
# ao black-scholes matrixmult lattice_boltzmann

TOPTARGETS := compileall compiledoping runall rundoping clean

.PHONY: $(MCKERNELS)

$(TOPTARGETS): $(MCKERNELS)

$(MCKERNELS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

