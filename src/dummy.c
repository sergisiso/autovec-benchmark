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

void free_arrays(
        TYPE *X, TYPE *Y, TYPE *Z, TYPE *U, TYPE *V,
        TYPE *a, TYPE *b, TYPE *c, TYPE *d, TYPE *e,
        TYPE *array, TYPE * x, int * indx,
        TYPE *aa, TYPE *bb, TYPE *cc, TYPE *tt, TYPE *xx ){
    free(X);
    free(Y);
    free(Z);
    free(U);
    free(V);
    free(x);
    free(a);
    free(b);
    free(c);
    free(d);
    free(e);
    free(indx);
    free(array);
    free(aa);
    free(bb);
    free(cc);
    free(tt);
    free(xx);
}



int dummy(TYPE a[LEN], TYPE b[LEN], TYPE c[LEN], TYPE d[LEN], TYPE e[LEN], TYPE aa[LEN2][LEN2], TYPE bb[LEN2][LEN2], TYPE cc[LEN2][LEN2], TYPE s){
	// --  called in each loop to make all computations appear required
	return 0;
}

