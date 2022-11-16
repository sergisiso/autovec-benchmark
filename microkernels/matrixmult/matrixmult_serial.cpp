#include <iostream>
#include <fstream>
#include <stdlib.h>
#include "../timing.h"
#include <sstream>

//#define PRINT_PROGRESS 1

void do_x_matrixMult ( unsigned nump, double *a, double *b, double *c, int MATRIXSIZEp) {

#if SPECIALIZE
    unsigned num = 500000;
    unsigned MATRIXSIZE = 32;
#else
    unsigned num = nump;
    unsigned MATRIXSIZE = MATRIXSIZEp;
#endif

#ifdef PRINT_PROGRESS
    double timings[20];
    unsigned iterations[20];
    timings[0] = rtc();
    iterations[0] = 0;
    unsigned next_print = 0;
    int print_count = 1;
#endif

    for ( unsigned n = 0; n < num; n++){
	    for ( unsigned x = 0; x < MATRIXSIZE; ++x ) {
		    for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
			    for ( unsigned z = 0; z < MATRIXSIZE; ++z ) {
				    c[x * MATRIXSIZE + y] += a[x * MATRIXSIZE + z] * b[z * MATRIXSIZE + y];
			    }
		    }
	    }

#ifdef PRINT_PROGRESS
        if (n > next_print){
            timings[print_count] = rtc();
            iterations[print_count] = n;
            print_count = print_count + 1;
            next_print = next_print + (num/15);
        }
#endif
    }
#ifdef PRINT_PROGRESS
    for(int i = 0; i < print_count; i++){
        printf("Progress: it: %d, time: %f \n", iterations[i], timings[i] - timings[0]);
    }
#endif
}
