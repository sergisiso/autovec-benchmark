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
*   Content : Image Scaling Using Histospline Technique
*
********************************************************************************
*/

#include <math.h>
#include <stdio.h>
#include <mkl.h>

#define SPLORDER    DF_PP_CUBIC
#define SPLTYPE     DF_PP_NATURAL

/* We use SP, as it visually looks good enough */
#define fptype          float
#define NewTask1D       dfsNewTask1D
#define EditPPSpline1D  dfsEditPPSpline1D
#define Construct1D     dfsConstruct1D
#define Interpolate1D   dfsInterpolate1D

#if defined(_WIN32)||defined(_WIN64)
    #define png_byte  unsigned char
    #define png_bytep unsigned char*
    extern void read_png_file(char* file_name);
    extern void write_png_file(char* file_name);
#else
    #define png_byte  unsigned char
    #define png_bytep unsigned char*
    extern void read_png_file(char* file_name);
    extern void write_png_file(char* file_name);
#endif

/* Color planes memory for both images */
png_bytep* row_pointers1;
png_bytep* row_pointers2;
/* Macros for accessing value of c color component of pixel (x,y) */
#define IMAGE1_PIXEL(x,y,c,xsize,ysize,nc)      row_pointers1[y][x*nc+c]
#define IMAGE2_PIXEL(x,y,c,xsize,ysize,nc)      row_pointers2[y][x*nc+c]


/* Input/output file names for colored images */
char *infilename1  = "./png_input/image_scaling_colored.png";
char *outfilename1 = "./png_output/test_out1.png";
/* Input/output file names for grayscaled images */
char *infilename2  = "./png_input/image_scaling_grayscaled.png";
char *outfilename2 = "./png_output/test_out2.png";

int n1; /* height of input  image */
int m1; /* width  of input  image */
int n2; /* height of output image */
int m2; /* width  of output image */
int nc; /* number of color planes */

void interpolate_der(int nx, fptype* x,    int ny, fptype* f,    int nsite, fptype* xx,    fptype* r) {
    fptype* datahint=NULL;
    int der_orders_sz=2;
    int der_orders[]={0,1,};
    int* nxx_cell_indexes=0;
    fptype* scoeff;
    int errorCode;
    DFTaskPtr interpTask = 0;
    int xhint = DF_UNIFORM_PARTITION;
    int yhint = DF_MATRIX_STORAGE_ROWS;
    int siteHint = DF_UNIFORM_PARTITION;
    int rhint=DF_MATRIX_STORAGE_ROWS;

    scoeff=(fptype*)mkl_malloc(sizeof(fptype)*SPLORDER*ny*(nx-1),64);
    if(scoeff==NULL) {
        printf("Error: not enough memory for scoeff.\n");
        exit(-1);
    }

    errorCode = NewTask1D(&interpTask, nx,x,xhint, ny,f, yhint);
    if(errorCode)printf("NewTask1D errorCode=%d\n",errorCode);

    errorCode = EditPPSpline1D(interpTask,
        SPLORDER,
        SPLTYPE,
        DF_BC_FREE_END, NULL, /* Free-end boundary condition. */
        DF_NO_IC, NULL,       /* No internal conditions. */
        scoeff, DF_NO_HINT);
    if(errorCode)printf("EditPPSpline1D errorCode=%d\n",errorCode);

    errorCode = Construct1D(interpTask, 0, 0);
    if(errorCode)printf("Construct1D errorCode=%d\n",errorCode);

    errorCode = Interpolate1D(interpTask, DF_INTERP, ny, nsite,xx, siteHint, der_orders_sz,der_orders, datahint,r,rhint, nxx_cell_indexes);
    if(errorCode)printf("Interpolate1D errorCode=%d\n",errorCode);

    errorCode = dfDeleteTask(&interpTask);
    if(errorCode)printf("dfDeleteTask errorCode=%d\n",errorCode);

    mkl_free(scoeff);
}

void process_file(void) {
    int x1,x2,y1,y2,c,s;
    fptype fs;
    fptype v;
    int    i;

    fptype* VX = (fptype*)mkl_malloc(sizeof(fptype)*n1*(m1+1),64);
    fptype* VXR= (fptype*)mkl_malloc(sizeof(fptype)*n1*(m2+1),64);
    fptype* VY = (fptype*)mkl_malloc(sizeof(fptype)*(m2+1)*(n1+1),64);
    fptype* VYR= (fptype*)mkl_malloc(sizeof(fptype)*(m2+1)*(n2+1),64);
    fptype x_breaks[2];
    fptype x_sites [2];
    fptype y_breaks[2];
    fptype y_sites [2];

    if(VX ==NULL) {printf("Error. Not enough memory for VX .\n"); exit(-1); }
    if(VXR==NULL) {printf("Error. Not enough memory for VXR.\n"); exit(-1); }
    if(VY ==NULL) {printf("Error. Not enough memory for VY .\n"); exit(-1); }
    if(VYR==NULL) {printf("Error. Not enough memory for VYR.\n"); exit(-1); }

    x_breaks[0]=0.0f; x_breaks[1]=(fptype)m1;
    x_sites [0]=0.0f; x_sites [1]=(fptype)m1;
    y_breaks[0]=0.0f; y_breaks[1]=(fptype)n1;
    y_sites [0]=0.0f; y_sites [1]=(fptype)n1;

    for( c=0; c<nc; c++ ) {
        /* 1) input image pixel matrix n1*m1 --> to VX matrix n1*(m1+1) */
        for( y1=0; y1<n1; y1++ ) { /* For each row of input image */
            /* 1.a) get VX as accumulated sums of pixel intencities */
            for( x1=0, s=0; x1<=m1; x1++ ) {
                VX[y1*(m1+1)+x1]=(fptype)s;
                s+=IMAGE1_PIXEL(x1,y1,c,m1,n1,nc);
            }
            VX[y1*(m1+1)+x1]=(fptype)s;
        }
        /* 1.b) histopolation of VX into VXR */
        for( y1=0; y1<n1; y1++ ) {
            interpolate_der(m1+1,x_breaks, 1,&VX[y1*(m1+1)+0], m2+1,x_sites, &VXR[y1*(m2+1)+0]);
        }

        /* 2) transpose VXR to VXRT not needed (can be skipped) */

        /* 3) intermediate image pixel matrix VXRT (m2+1)*n1 --> to VY matrix (m2+1)*(n2+1) */
        for( x2=0; x2<=m2; x2++ ) {
            /* 3.a) transpose VXR; and get VY as accumulated sums of just computed values */
            for( y1=0,fs=0.0; y1<=n1; y1++ ) {
                VY[x2*(n1+1)+y1]=fs; fs+=VXR[y1*(m2+1)+x2];
            }
            VY[x2*(n1+1)+y1]=fs;
        }

        /* 3.b) histopolation of VY into VYR */
        for( x2=0; x2<=m2; x2++ ) {
            interpolate_der(n1+1,y_breaks, 1,&VY[x2*(n1+1)+0], n2+1,y_sites, &VYR[x2*(n2+1)+0]);
        }

        /* 4) transpose VYR to VR and get integer result (VR not needed to store) */
        for( y2=0; y2<n2; y2++ ) { /* not using last row */
            for( x2=0; x2<m2; x2++ ) { /* not using last column */
                v=VYR[x2*(n2+1)+y2];
                /* add 0.5 for rounding to nearest during next conversion to integer */
                v = v + 0.5f;
                i = (int)v;
                /* saturation */
                if(i<0) i=0;
                if(i>255)i=255;
                /* convert to integer and save to color plane c of output image pixel */
                IMAGE2_PIXEL(x2,y2,c,m2,n2,nc)=(png_byte)i;
            }
        }
    }

    mkl_free(VYR);
    mkl_free(VY );
    mkl_free(VXR);
    mkl_free(VX );
}


int main()
{
    read_png_file(infilename1);
    process_file();
    write_png_file(outfilename1);

    read_png_file(infilename2);
    process_file();
    write_png_file(outfilename2);

    return 0;
}
