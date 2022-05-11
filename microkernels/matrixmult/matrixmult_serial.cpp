#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <time.h>
#include <sstream>


void do_x_matrixMult ( unsigned nump, double *a, double *b, double *c, int MATRIXSIZEp) {

#if SPECIALIZE
    int num = 500000;
    int MATRIXSIZE = 32;
#else
    int num = nump;
    int MATRIXSIZE = MATRIXSIZEp;
#endif

    for ( unsigned n = 0; n < num; n++){
	    for ( unsigned x = 0; x < MATRIXSIZE; ++x ) {
		    for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
			    for ( unsigned z = 0; z < MATRIXSIZE; ++z ) {
				    c[x * MATRIXSIZE + y] += a[x * MATRIXSIZE + z] * b[z * MATRIXSIZE + y];
			    }
		    }
	    }
    }
}
