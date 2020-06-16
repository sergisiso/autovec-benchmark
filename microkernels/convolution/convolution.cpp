#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef __linux__
#include <malloc.h>
#endif
#include <math.h>
#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <sys/types.h>

#include "../timing.h"


extern void BM_convolve(int * image, int * out, int p_n, int p_m, int * p_mask);

int main(int argc, char **argv)
{
    int mask[9] = {1,2,3,0,0,0,3,2,1};
    int m = 3;
    int n = 8192;

    int *image = new int[n*n];
    int *out = new int[(n-3)*(n-3)];

    //
    // Run the serial path, again test_iteration times, and report the
    // minimum time.
    //
    double minTimeSerial = 1e30;
    for (unsigned int i = 0; i < 5; i++) {
        reset_and_start_timer();
        BM_convolve(image, out, n, m, mask);
        double t = get_elapsed_msec();
        printf("@time of serial run:\t\t\t\t[%.3f] mseconds\n", t);
        minTimeSerial = std::min(minTimeSerial, t);
    }

    // Report more results, save another image...
    printf("[convolution serial]:\t\t[%.3f] mseconds (%d x %d image)\n", minTimeSerial, 
           n, m);
        
    return 0;
}
