/*
*******************************************************************************
* Copyright(C) 2014-2015 Intel Corporation. All Rights Reserved.
*
* The source code, information  and  material ("Material") contained herein is
* owned  by Intel Corporation or its suppliers or licensors, and title to such
* Material remains  with Intel Corporation  or its suppliers or licensors. The
* Material  contains proprietary information  of  Intel or  its  suppliers and
* licensors. The  Material is protected by worldwide copyright laws and treaty
* provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
* modified, published, uploaded, posted, transmitted, distributed or disclosed
* in any way  without Intel's  prior  express written  permission. No  license
* under  any patent, copyright  or  other intellectual property rights  in the
* Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
* implication, inducement,  estoppel or  otherwise.  Any  license  under  such
* intellectual  property  rights must  be express  and  approved  by  Intel in
* writing.
* 
* *Third Party trademarks are the property of their respective owners.
* 
* Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
* this  notice or  any other notice embedded  in Materials by Intel or Intel's
* suppliers or licensors in any way.
*
********************************************************************************
*   Content : Image file I/O for Linux/MacOS using libpng (the free reference
*             library for reading and writing PNGs, see http://www.libpng.org)
*
********************************************************************************
*/

#include <string>
#include <atlstr.h>
#include <atlimage.h>
#include <mkl.h>

#define png_byte  unsigned char
#define png_bytep unsigned char*

#define IMAGE_SCALE_FACTOR 3

extern "C" int n1; /* height of input  image */
extern "C" int m1; /* width  of input  image */
extern "C" int n2; /* height of output image */
extern "C" int m2; /* width  of output image */
extern "C" int nc; /* number of color planes */

png_byte    color_type;
png_byte    bit_depth;
extern "C" png_bytep*  row_pointers1;
extern "C" png_bytep*  row_pointers2;


extern "C"
void read_png_file(char* file_name);

void read_png_file(char* file_name) {
    CImage image1;
    int x1,y1,y2,c;

    image1.Load(file_name);

    m1 = image1.GetWidth();
    n1 = image1.GetHeight();
    m2 = IMAGE_SCALE_FACTOR*m1;
    n2 = IMAGE_SCALE_FACTOR*n1;

    nc=3; /* Number of color planes */

    /* Allocate color planes memory for both images */
    row_pointers1 = (png_bytep*)mkl_malloc(sizeof(png_bytep) * n1,64);
    row_pointers2 = (png_bytep*)mkl_malloc(sizeof(png_bytep) * n2,64);
    for(y1=0;y1<n1;y1++) {
        row_pointers1[y1]=(png_byte*)mkl_malloc(sizeof(png_byte)*nc*n1*m1,64);
    }
    for(y2=0;y2<n2;y2++) {
        row_pointers2[y2]=(png_byte*)mkl_malloc(sizeof(png_byte)*nc*n2*m2,64);
    }

    /* Copy image pixels */
    for(y1=0; y1<n1; y1++) {
        for(x1=0;x1<m1;x1++) {
            COLORREF c = image1.GetPixel(x1,y1);
            row_pointers1[y1][x1*nc+0] = GetRValue(c);
            row_pointers1[y1][x1*nc+1] = GetGValue(c);
            row_pointers1[y1][x1*nc+2] = GetBValue(c);
        }
    }
}

extern "C"
void write_png_file(char* file_name);

void write_png_file(char* file_name) {
    CImage image2;
    int x2,y2,y1,c;

    image2.Create(m2,n2,nc*8/*BPP*/,0);

    /* Copy image pixels */
    for(y2=0; y2<n2; y2++) {
        for(x2=0;x2<m2;x2++) {
            BYTE r = row_pointers2[y2][x2*nc+0];
            BYTE g = row_pointers2[y2][x2*nc+1];
            BYTE b = row_pointers2[y2][x2*nc+2];
            image2.SetPixel(x2,y2,RGB(r,g,b));
        }
    }

    image2.Save(file_name);

    /* Release color planes memory we used for both images */
    for(y1=0; y1<n1; y1++) {
        mkl_free(row_pointers1[y1]);
    }
    mkl_free(row_pointers1);
    for(y2=0; y2<n2; y2++) {
        mkl_free(row_pointers2[y2]);
    }
    mkl_free(row_pointers2);
}
