/* Copyright (c) 2019 Sergi Siso

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
  3. Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>
#define TYPE float
#define ALIGNMENT 64
#define LEN 32000
#define LEN2 256


//Declare arrays with but don't allocate them
//TYPE * X,* Y, * Z, * U, * V; //size LEN
//TYPE * array; // size LEN2*LEN2
//TYPE * x; //size LEN
//TYPE * a, * b, * c, * d, * e; //size LEN
//TYPE * aa, * bb, * cc, * tt; //size LEN2*LEN2

//int * indx; // size LEN
//TYPE* xx;
//TYPE* yy;

void allocate_arrays(
        TYPE **X, TYPE **Y, TYPE **Z, TYPE **U, TYPE **V,
        TYPE **a, TYPE **b, TYPE **c, TYPE **d, TYPE **e,
        TYPE **array, TYPE **x, int **indx,
        TYPE **aa, TYPE **bb, TYPE **cc, TYPE **tt, TYPE **xx ){
    int err;

    err = posix_memalign((void **) X, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) Y, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) Z, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) U, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) V, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}

    err = posix_memalign((void **) x, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) a, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) b, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) c, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) d, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) e, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) indx, ALIGNMENT, LEN*sizeof(int));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}

    err = posix_memalign((void **) array, ALIGNMENT, LEN2*LEN2*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) aa, ALIGNMENT, LEN2*LEN2*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) bb, ALIGNMENT, LEN2*LEN2*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) cc, ALIGNMENT, LEN2*LEN2*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}
    err = posix_memalign((void **) tt, ALIGNMENT, LEN2*LEN2*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}

    err  = posix_memalign((void **) xx, ALIGNMENT, LEN*sizeof(TYPE));
    if (err != 0){printf("Posix_memalign error:%d\n",err);exit(-1);}

}


int dummy(TYPE a[LEN], TYPE b[LEN], TYPE c[LEN], TYPE d[LEN], TYPE e[LEN], TYPE aa[LEN2][LEN2], TYPE bb[LEN2][LEN2], TYPE cc[LEN2][LEN2], TYPE s){
	// --  called in each loop to make all computations appear required
	return 0;
}

