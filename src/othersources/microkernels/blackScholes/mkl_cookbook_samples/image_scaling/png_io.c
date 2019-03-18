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

#include <png.h>
#include <mkl.h>

#define IMAGE_SCALE_FACTOR 3

extern int n1; /* height of input  image */
extern int m1; /* width  of input  image */
extern int n2; /* height of output image */
extern int m2; /* width  of output image */
extern int nc; /* number of color planes */

png_byte    color_type;
png_byte    bit_depth;
extern png_bytep*  row_pointers1;
extern png_bytep*  row_pointers2;

void read_png_file(char* file_name) {
    FILE* fin;
    int y1;
    png_structp png_ptr;
    png_infop info_ptr;
    #define HDR_CHK_BYTES 8
    png_byte hdr[HDR_CHK_BYTES];

    if( !(fin = fopen(file_name, "rb")) ) {
        printf("Error. Cannot open file %s.\n", file_name);
        exit(-1);
    }
    fread(hdr, 1, HDR_CHK_BYTES, fin);
    if( png_sig_cmp(hdr, 0, HDR_CHK_BYTES) ) {
        printf("Error. This is not PNG file: %s.\n", file_name);
        exit(-1);
    }
    if( !(png_ptr=png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL))) {
        printf("Error in png_create_read_struct.\n");
        exit(-1);
    }
    if(!(info_ptr=png_create_info_struct(png_ptr))) {
        printf("png_create_info_struct failed");
        exit(-1);
    }
    if(setjmp(png_jmpbuf(png_ptr))) {
        printf("Error during init_io");
        exit(-1);
    }

    png_init_io(png_ptr, fin);
    png_set_sig_bytes(png_ptr, HDR_CHK_BYTES);
    png_read_info(png_ptr, info_ptr);

    m1 = png_get_image_width(png_ptr, info_ptr);
    n1 = png_get_image_height(png_ptr, info_ptr);
    m2 = IMAGE_SCALE_FACTOR*m1;
    n2 = IMAGE_SCALE_FACTOR*n1;

    color_type = png_get_color_type(png_ptr, info_ptr);
    bit_depth = png_get_bit_depth(png_ptr, info_ptr);

    png_read_update_info(png_ptr, info_ptr);
    if(setjmp(png_jmpbuf(png_ptr))) {
        printf("Error during read_image");
        exit(-1);
    }

    nc=0; /* Number of color planes */
    if (png_get_color_type(png_ptr, info_ptr) == PNG_COLOR_TYPE_RGB)  nc=3;
    if (png_get_color_type(png_ptr, info_ptr) == PNG_COLOR_TYPE_RGBA) nc=4;

    /* Allocate color planes memory for both images */
    row_pointers1 = (png_bytep*)mkl_malloc(sizeof(png_bytep) * n1,64);
    row_pointers2 = (png_bytep*)mkl_malloc(sizeof(png_bytep) * n2,64);
    for(y1=0;y1<n1;y1++) {
        row_pointers1[y1]=(png_byte*)mkl_malloc(png_get_rowbytes(png_ptr,info_ptr),64);
    }
    for(y1=0;y1<n2;y1++) {
        row_pointers2[y1]=(png_byte*)mkl_malloc(png_get_rowbytes(png_ptr,info_ptr)*IMAGE_SCALE_FACTOR,64);
    }

    png_read_image(png_ptr, row_pointers1);
    fclose(fin);
}

void write_png_file(char* file_name) {
    FILE *fp;
    png_structp png_ptr;
    int y1;
    png_infop info_ptr;
    if(!(fp = fopen(file_name, "wb"))) {
        printf("File %s could not be opened for writing", file_name);
        exit(-1);
    }
    if(!(png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL))) {
        printf("png_create_write_struct failed");
        exit(-1);
    }
    if(!(info_ptr = png_create_info_struct(png_ptr))) {
        printf("png_create_info_struct failed");
        exit(-1);
    }
    if (setjmp(png_jmpbuf(png_ptr))) {
        printf("Error during init_io");
        exit(-1);
    }
    png_init_io(png_ptr, fp);
    if(setjmp(png_jmpbuf(png_ptr))) {
        printf("Error during writing header");
        exit(-1);
    }
    png_set_IHDR(png_ptr, info_ptr, m2, n2, bit_depth, color_type, PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
    png_write_info(png_ptr, info_ptr);
    if(setjmp(png_jmpbuf(png_ptr))) {
        printf("Error during writing bytes");
        exit(-1);
    }
    png_write_image(png_ptr, row_pointers2);
    if(setjmp(png_jmpbuf(png_ptr))) {
        printf("Error during end of write");
        exit(-1);
    }
    png_write_end(png_ptr, NULL);

    /* Release color planes memory we used for both images */
    for(y1=0; y1<n1; y1++) {
        mkl_free(row_pointers1[y1]);
    }
    mkl_free(row_pointers1);
    for(y1=0; y1<n2; y1++) {
        mkl_free(row_pointers2[y1]);
    }
    mkl_free(row_pointers2);
    fclose(fp);
}
