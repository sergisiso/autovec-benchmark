# Auto-vectorization benchmark with compiler information withholding 

## Introduction


## Instructions

Path                | Description
------------------- | -----------
src/                | Source files of the test suite which inlcues the extended TSVC and the micro-kernels.
scripts/            | Scripts to help with the execution of the full suite and the analysis of the results.
results-2019/       | Results associated with the published paper.
license.txt         | Copyright and permission notices
README              | This file

To run the full suite:
1. Generate Benchmarks with `bench.py` script
2. Execute Benchmarks with the runall.sh scripts (or the submit<CLUSTER>.lsf)
3. Run analysis scripts `plotresults*.py` to generate plots and tables.


## Results


## Contributors and attributions


## References
- http://polaris.cs.uiuc.edu/~maleki1/TSVC.tar.gz
- https://github.com/llvm-mirror/test-suite/blob/master/MultiSource/Benchmarks/TSVC
- https://ispc.github.io/
