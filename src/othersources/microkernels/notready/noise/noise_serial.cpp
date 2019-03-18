/*
  Copyright (c) 2010-2011, Intel Corporation
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

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <math.h>

#define NOISE_PERM_SIZE 256

static int NoisePerm[2 * NOISE_PERM_SIZE] = {
    151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140,
    36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120,
    234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33,
    88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168,  68, 175, 74, 165, 71, 
    134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 
    230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161,
    1, 216, 80, 73, 209, 76, 132, 187, 208,  89, 18, 169, 200, 196, 135, 130, 
    116, 188, 159, 86, 164, 100, 109, 198, 173, 186,  3, 64, 52, 217, 226, 250,
    124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 
    47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152,  2, 44,
    154, 163, 70, 221, 153, 101, 155, 167,  43, 172, 9, 129, 22, 39, 253,  19, 
    98, 108, 110, 79, 113, 224, 232, 178, 185,  112, 104, 218, 246, 97, 228, 251,
    34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249,
    14, 239, 107, 49, 192, 214,  31, 181, 199, 106, 157, 184, 84, 204, 176, 115,
    121, 50, 45, 127,  4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 
    243, 141, 128, 195, 78, 66, 215, 61, 156, 180, 151, 160, 137, 91, 90, 15,
    131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99,
    37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252,
    219, 203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 
    136, 171, 168,  68, 175, 74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158,
    231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245,
    40, 244, 102, 143, 54,  65, 25, 63, 161,  1, 216, 80, 73, 209, 76, 132, 187,
    208,  89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 
    198, 173, 186,  3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118,
    126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42,
    223, 183, 170, 213, 119, 248, 152,  2, 44, 154, 163, 70, 221, 153, 101, 155, 
    167,  43, 172, 9, 129, 22, 39, 253,  19, 98, 108, 110, 79, 113, 224, 232,
    178, 185,  112, 104, 218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144,
    12, 191, 179, 162, 241,  81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214,
    31, 181, 199, 106, 157, 184,  84, 204, 176, 115, 121, 50, 45, 127,  4, 150,
    254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 
    66, 215, 61, 156, 180
};


#if SPECIALIZED
    constexpr float x0 = -10;
    constexpr float yy0 = -10;
    constexpr float x1 = 10;
    constexpr float yy1 = 10;
    constexpr float z = 0.6f;
    constexpr int width = 768;
    constexpr int height = 768;
    constexpr int octaves = 8;
    constexpr float omega = 0.6;
    constexpr float lambda = 1.;
    constexpr float o = 1.;

    constexpr float factor1 = 1.99f;
    constexpr float factor2 = 0.5f;
    constexpr float factor3 = -2.f;
    constexpr float factor4 = 3.f;
    constexpr float factor5 = 0.f;
    constexpr float factor6 = 1.f;
    constexpr float factor7 = 6.f;
    constexpr float factor8 = 15.f;
    constexpr float factor9 = 10.f;

    constexpr int par15 = 15;
    constexpr int par8 = 8;
    constexpr int par12 = 12;
    constexpr int par13 = 13;
    constexpr int par4 = 4;
    constexpr int par1 = 1;
    constexpr int par2 = 2;
#else
    float x0;
    float yy0;
    float x1;
    float yy1;
    float z;
    int width;
    int height;
    int octaves;

    float omega;
    float lambda;
    float o;
    float factor1;
    float factor2;

    float factor3;
    float factor4;
    float factor5;
    float factor6;
    float factor7;
    float factor8;
    float factor9;

    int par15;
    int par8;
    int par12;
    int par13;
    int par4;
    int par1;
    int par2;
#endif

inline float Clamp(float v, float low, float high) {
    return v < low ? low : ((v > high) ? high : v);
}

inline float SmoothStep(float low, float high, float value) {
    float v = Clamp((value - low) / (high - low), factor5, factor6);
    return v * v * (factor3 * v  + factor4);
}

inline int Floor2Int(float val) {
    return (int)floorf(val);
}

inline float Grad(int x, int y, int z, float dx, float dy, float dz) {
    int h = NoisePerm[NoisePerm[NoisePerm[x]+y]+z];
    h &= par15;
    float u = h<par8 || h==par12 || h==par13 ? dx : dy;
    float v = h<par4 || h==par12 || h==par13 ? dy : dz;
    return ((h&par1) ? -u : u) + ((h&par2) ? -v : v);
}

inline float NoiseWeight(float t) {
    float t3 = t*t*t;
    float t4 = t3*t;
    return factor7*t4*t - factor8*t4 + factor9*t3;
}

inline float Lerp(float t, float low, float high) {
    return (factor6 - t) * low + t * high;
}

inline float Noise(float x, float y, float z) {
    // Compute noise cell coordinates and offsets
    int ix = Floor2Int(x), iy = Floor2Int(y), iz = Floor2Int(z);
    float dx = x - ix, dy = y - iy, dz = z - iz;

    // Compute gradient weights
    ix &= (NOISE_PERM_SIZE-par1);
    iy &= (NOISE_PERM_SIZE-par1);
    iz &= (NOISE_PERM_SIZE-par1);
    float w000 = Grad(ix,   iy,   iz,   dx,   dy,   dz);
    float w100 = Grad(ix+par1, iy,   iz,   dx-par1, dy,   dz);
    float w010 = Grad(ix,   iy+par1, iz,   dx,   dy-par1, dz);
    float w110 = Grad(ix+par1, iy+par1, iz,   dx-par1, dy-par1, dz);
    float w001 = Grad(ix,   iy,   iz+par1, dx,   dy,   dz-par1);
    float w101 = Grad(ix+par1, iy,   iz+par1, dx-par1, dy,   dz-par1);
    float w011 = Grad(ix,   iy+par1, iz+par1, dx,   dy-par1, dz-par1);
    float w111 = Grad(ix+par1, iy+par1, iz+par1, dx-par1, dy-par1, dz-par1);

    // Compute trilinear interpolation of weights
    float wx = NoiseWeight(dx), wy = NoiseWeight(dy), wz = NoiseWeight(dz);
    float x00 = Lerp(wx, w000, w100);
    float x10 = Lerp(wx, w010, w110);
    float x01 = Lerp(wx, w001, w101);
    float x11 = Lerp(wx, w011, w111);
    float y00 = Lerp(wy, x00, x10);
    float y11 = Lerp(wy, x01, x11);
    return Lerp(wz, y00, y11);
}

inline float Turbulence(float x, float y, float z, int octaves) {

    float sum = 0.;
    //#pragma omp simd reduction(+:sum) reduction(*:lambda,o)
    for (int i = 0; i < octaves; ++i) {
        sum += fabsf(o * Noise(lambda * x, lambda * y, lambda * z));
        lambda *= factor1;
        o *= omega;
    }
    return sum * factor2;
}

void noise_serial(float x0p, float y0p, float x1p, float y1p,
                  int widthp, int heightp, float output[])
{

    /*    

     std::cout << std::endl;
     std::cout << "x0: " << x0p << " y0: " << y0p << std::endl;
     std::cout << "x1: " << x1p << " y1: " << y1p << std::endl;
     std::cout << "width: " << widthp << " height: " << heightp << std::endl;
     std::exit(0);

    */

#if SPECIALIZED
    // Parameters already defined
    std::cout << "Specialized version" << std::endl;
#else
    std::cout << "Runtime version" << std::endl;
    // Set values
    x0 = x0p;
    yy0 = y0p;
    x1 = x1p;
    yy1 = y1p;
    width = widthp;
    height = heightp;

    //From file
    std::ifstream f;
    f.open("parameters.dat");
    f >> octaves;

    f >> z;
    f >> omega;
    f >> lambda;
    f >> o;
    f >> factor1;
    f >> factor2;

    f >> factor3;
    f >> factor4;
    f >> factor5;
    f >> factor6;
    f >> factor7;
    f >> factor8;
    f >> factor9;

    f >> par15;
    f >> par8;
    f >> par12;
    f >> par13;
    f >> par4;
    f >> par1;
    f >> par2;

    f.close();
#endif

    float dx = (x1 - x0) / width;
    float dy = (yy1 - yy0) / height;

    for (int j = 0; j < height; j++) {
        for (int i = 0; i < width; ++i) {
            float x = x0 + i * dx;
            float y = yy0 + j * dy;

            int index = (j * width + i);
            output[index] = Turbulence(x, y, z, octaves);
        }
    }
}

