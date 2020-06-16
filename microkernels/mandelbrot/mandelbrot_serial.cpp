/*  Copyright (c) 2019, Sergi siso 

  This file incorporates work covered by the following copyright and  
  permission notice: 

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
/*
  Modified from: https://github.com/ispc/ispc/tree/master/examples
*/

#include <iostream>
#include <cstdlib>

#if SPECIALIZE
    constexpr float x0 = -2.f;
    constexpr float yy0 = -1.f;
    constexpr float x1 = 1.f;
    constexpr float yy1 = 1.f;
    constexpr int width = 768;
    constexpr int height = 512;
    constexpr int maxIterations = 256;
#else
    float x0;
    float yy0;
    float x1;
    float yy1;
    int width;
    int height;
    int maxIterations;
#endif
 
void mandelbrot_serial(float x0p, float y0p, float x1p, float y1p,
                       int widthp, int heightp, int maxIterationsp,
                       int output[])
{
    
#if SPECIALIZE
    //std::cout << " specialize -------------" << std::endl;
#else
    //std::cout << " runtime -------------" << std::endl;
    x0 = x0p;
    yy0 = y0p;
    x1 = x1p;
    yy1 = y1p;
    width = widthp;
    height = heightp;
    maxIterations = maxIterationsp;
#endif
                       
    float dx = (x1 - x0) / width;
    float dy = (yy1 - yy0) / height;

    #pragma omp simd
    for (int j = 0; j < height; j++) {
        for (int i = 0; i < width; ++i) {
            float x = x0 + i * dx;
            float y = yy0 + j * dy;

            int index = (j * width + i);
            float z_re = x, z_im = y;
            int it;
            for (it = 0; it < maxIterations; ++it) {
                if (z_re * z_re + z_im * z_im > 4.f) break;
                float new_re = z_re*z_re - z_im*z_im;
                float new_im = 2.f * z_re * z_im;
                z_re = x + new_re;
                z_im = y + new_im;
            }
            output[index] = it;
        }
    }
}

