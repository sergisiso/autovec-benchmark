Copyright(C) 2014-2015 Intel Corporation. All Rights Reserved.

The source code, information  and  material ("Material") contained herein is
owned  by Intel Corporation or its suppliers or licensors, and title to such
Material remains  with Intel Corporation  or its suppliers or licensors. The
Material  contains proprietary information  of  Intel or  its  suppliers and
licensors. The  Material is protected by worldwide copyright laws and treaty
provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
modified, published, uploaded, posted, transmitted, distributed or disclosed
in any way  without Intel's  prior  express written  permission. No  license
under  any patent, copyright  or  other intellectual property rights  in the
Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
implication, inducement,  estoppel or  otherwise.  Any  license  under  such
intellectual  property  rights must  be express  and  approved  by  Intel in
writing.

*Third Party trademarks are the property of their respective owners.

Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
this  notice or  any other notice embedded  in Materials by Intel or Intel's
suppliers or licensors in any way.

The package contains examples of using Intel(R) Math Kernel Library 
(Intel(R) MKL) in application to various numerical problems. The 
samples include source files, makefiles, and input data.

To build the examples you need to have Intel MKL installed and the 
environment set properly. The makefiles are written to use Intel 
Compilers. On a Windows* system use nmake tool (it uses makefile) 
and on a Linux* or OS X* system use make (it uses GNUmakefile). You 
can edit makefiles for use with other compilers. For that the Intel 
MKL Link Line Advisor tool can be helpful 
(http://software.intel.com/en-us/articles/intel-mkl-link-line-advisor).

Please provide your feedback on the examples at Intel MKL Forum
http://software.intel.com/en-us/forums/intel-math-kernel-library.

Content of Directories:

ANGLES/definition - example of computing principal angles between two subspaces in a linear space.
ANGLES/source - functions needed for computing principal angles between two subspaces.
ANGLES/uep_subspace1 - example of computing principal angles between two invariant subspaces of a triangular matrix.
ANGLES/uep_subspace2 - example of computing principal angles between two invariant subspaces of a block triangular matrix of comparatively small size.
ANGLES/uep_subspace3 - example of computing principal angles between two invariant subspaces of a block triangular matrix of comparatively big size.

black-scholes - Black-Scholes formula example

BlockTDS_GE/factor - example of factoring block tridiagonal general matrices.
BlockTDS_GE/solve - example of solving a system of linear equations with block tridiagonal general coefficient matrix.
BlockTDS_GE/source - functions needed for the factoring and solving samples.

BlockTDS_SPD/factor - example of factoring block tridiagonal symmetric positive definite matrices.
BlockTDS_SPD/solve - example of solving a system of linear equations with block tridiagonal symmetric positive definite coefficient matrix.
BlockTDS_SPD/source - functions needed for the factoring and solving samples.

fft-ct - example of using Fast Fourier Transforms for Computer Tomography Image Reconstruction.

lottery6of49 - example of Multiple Simple Random Sampling without Replacement.

mc - Monte Carlo European Options Pricing Example 

nf - example Noise filtering in financial market data streams.

sparse - example of finding an approximate solution to a nonlinear equation.

dgemm_python - example of speeding up Python* scientific computations.
