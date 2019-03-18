#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef __linux__
#include <malloc.h>
#endif
#include <math.h>
#include <map>
#include <string>
#include <algorithm>
#include <sys/types.h>

#include "../timing.h"

extern void draw(float * img, int width, int height, int matrixsize, float * output);


int main(int argc, char **argv)
{
    int width = 1024*5;
    int height = 1024*5;

    // Allocate space for output images
    float * img = new float[width * height * 3];
    float * output = new float[width * height * 3];

    for(int i=0; i< width*height*3; i++) img[i] = 1;

    //
    // Run the serial path, again test_iteration times, and report the
    // minimum time.
    //
    double minTimeSerial = 1e30;
    for (unsigned int i = 0; i < 5; i++) {
        reset_and_start_timer();
        draw(img, width, height, 3, output);
        double t = get_elapsed_msec();
        printf("@time of serial run:\t\t\t\t[%.3f] mseconds\n", t);
        minTimeSerial = std::min(minTimeSerial, t);
    }

    // Report more results, save another image...
    printf("[convolution serial]:\t\t[%.3f] mseconds (%d x %d image)\n", minTimeSerial, 
           width, height);
        
    return 0;
}
