/*  Copyright (c) 2019-2021, Sergi Siso

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    * Neither the name of Intel Corporation nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.


   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
   TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
*/

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

void init_image(int * image, int n){
    for (int i=0; i<n; i++){
        for (int j=0; j<n; j++){
            image[i*n + j] = 100;
        }
    }
}

int checksum(int * image, int n){
    int sum = 0;
    for (int i=0; i<n; i++){
        for (int j=0; j<n; j++){
            sum += image[i*n + j];
        }
    }
    return sum;
}

int main(int argc, char **argv)
{
    int mask[9] = {1,2,3,0,0,0,3,2,1};
    int m = 3;
    int n = 1024*20;

    int *image = new int[n*n];
    int *out = new int[(n-3)*(n-3)];
    init_image(image, n);

    //
    // Run the serial path, again test_iteration times, and report the
    // minimum time.
    //
    double minTimeSerial = 1e30;
    for (unsigned int i = 0; i < 1; i++) {
        reset_and_start_timer();
        BM_convolve(image, out, n, m, mask);
        double t = get_elapsed_msec();
        printf("@time of serial run:\t\t\t\t[%.3f] mseconds\n", t);
        minTimeSerial = std::min(minTimeSerial, t);
    }

    // Report more results, save another image...
    printf("[convolution serial]:\t\t%.3f mseconds (%d x %d image) checksum=%d\n",
           minTimeSerial, n, m, checksum(out, n-3));
        
    return 0;
}
