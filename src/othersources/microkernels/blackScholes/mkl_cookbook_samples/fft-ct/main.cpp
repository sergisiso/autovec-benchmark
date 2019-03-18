/*******************************************************************************
! Copyright(C) 2014-2015 Intel Corporation. All Rights Reserved.
!
! The source code, information  and  material ("Material") contained herein is
! owned  by Intel Corporation or its suppliers or licensors, and title to such
! Material remains  with Intel Corporation  or its suppliers or licensors. The
! Material  contains proprietary information  of  Intel or  its  suppliers and
! licensors. The  Material is protected by worldwide copyright laws and treaty
! provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
! modified, published, uploaded, posted, transmitted, distributed or disclosed
! in any way  without Intel's  prior  express written  permission. No  license
! under  any patent, copyright  or  other intellectual property rights  in the
! Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
! implication, inducement,  estoppel or  otherwise.  Any  license  under  such
! intellectual  property  rights must  be express  and  approved  by  Intel in
! writing.
! 
! *Third Party trademarks are the property of their respective owners.
! 
! Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
! this  notice or  any other notice embedded  in Materials by Intel or Intel's
! suppliers or licensors in any way.
!
!*******************************************************************************
!  Content:
!      Demonstration of Computer Tomography image restoration with Fast Fourier Transform
!******************************************************************************/

#if defined(_WIN32)
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <complex>
#include <cstdarg>

#include "mkl.h"

#if !defined(M_PI)
#define M_PI 3.14159265358979323846
#endif

#if !defined(REAL)
#define REAL double
#endif

typedef std::complex<REAL> complex;

// A simple transparent matrix class, row major layout
template<typename T> struct Matrix
{
    T *data;
    int h, w, ldw;
    Matrix() { data = NULL; }
    Matrix(const Matrix&);
    Matrix& operator=(const Matrix&);
    void deallocate()
    {
        if (data) mkl_free(data); data = 0;
    }
    void allocate(int _h, int _w, int _ldw)
    {
        h = _h;
        w = _w;
        ldw = _ldw; // leading dimension for w
        deallocate();
        data = (T*)mkl_malloc( sizeof(T) * h * ldw, 64 );
    }
    ~Matrix() { deallocate(); }
};
typedef Matrix<REAL> MatrixR;
typedef Matrix<complex> MatrixC;

// Computational functions
void step1_fft_1d( MatrixR *radonImage );
void step2_interpolation(MatrixC *result, MatrixR *radonImage );
void step3_ifft_2d( MatrixC *fhat );

// Support functions
void bmpRead( MatrixR *image, const char *fname );
void bmpWrite( const char *fname, const MatrixR *image );
void bmpWrite( const char *fname, const MatrixC *image );
void acquireRadon( MatrixR *result, const MatrixR *input );
REAL integrateAlongLine( REAL theta, REAL s, const MatrixR *input );
template<typename T> inline int isOdd(const T& n) { return n & 1; }
void die(const char *fmt, ... );

int main(int argc, char **argv)
{
    // Usage:
    //   program.exe p q input.bmp radon.bmp restored.bmp
    // Input:
    //   p, q - parameters of Radon transform
    //   input.bmp - must be a 24-bit uncompressed input bitmap
    // Output:
    //   radon.bmp - p-by-(2q+1) result of Radon transform of input.bmp
    //   restored.bmp - 2q-by-2q result of FFT-based reconstruction
    int p = argc>1 ? atoi(argv[1]) : 200; // # of projections in range 0..PI
    int q = argc>2 ? atoi(argv[2]) : 100; // # of density points per projection is 2q+1
    const char *originalBmpName = argc>3 ? argv[3] : "input.bmp";
    const char *radonBmpName = argc>4 ? argv[4] : "radon.bmp";
    const char *restoredBmpName = argc>5 ? argv[5] : "restored.bmp";

    printf("Reading original image from %s\n", originalBmpName );
    MatrixR originalImage;
    bmpRead( &originalImage, originalBmpName );
    //bmpWrite( "check-original.bmp", &originalImage );

    printf("Allocating radonImage for backprojection\n");
    MatrixR radonImage;
    // space for p-by-(2q+2) reals, or for p-by-(q+1) complex numbers
    radonImage.allocate(p, 2*q+1, 2*q+2);
    if ( !radonImage.data ) die("cannot allocate memory for radonImage\n");

    printf("Performing backprojection\n");
    acquireRadon( &radonImage, &originalImage );
    bmpWrite( radonBmpName, &radonImage );

    printf("Restoring original: step1 - fft_1d in-place\n");
    step1_fft_1d( &radonImage );

    printf("Allocating array for radial->cartesian interpolation\n");
    MatrixC fhat;
    fhat.allocate(2*q, 2*q, 2*q);
    if ( !fhat.data ) die("cannot allocate memory for fhat\n");

    printf("Restoring original: step2 - interpolation\n");
    step2_interpolation( &fhat, &radonImage );
    //bmpWrite( "check-after-interpolation.bmp", &fhat );

    printf("Restoring original: step3 - ifft_2d in-place\n");
    step3_ifft_2d( &fhat );

    printf("Saving restored image to %s\n", restoredBmpName);
    bmpWrite( restoredBmpName, &fhat );

    return 0;
}

#if defined(_OPENMP)
#include <omp.h>
#define PRAGMA_OMP(args) _Pragma(STRINGIFY(omp args))
#define STRINGIFY(args) STRINGIFy(args)
#define STRINGIFy(args) #args
#if defined(_WIN32)
#undef PRAGMA_OMP
#define PRAGMA_OMP(args) __pragma(omp args)
#endif
#else
#define PRAGMA_OMP(args) /**/
#endif

// Step 1: batch of 1d r2c fft.
// ghat[j, lambda] <-- scale * FFT_1D( g[j,l] )
void step1_fft_1d(MatrixR *radon)
{
    MKL_LONG p = radon->h;
    MKL_LONG q2 = radon->w - 1; // w = 2*q + 1
    MKL_LONG ldw = radon->ldw;
    REAL scale = 1.0 / sqrt(0.0 + q2);

    // Make sure we can do in-place r2c
    if ( isOdd(ldw) ) die("c-domain needs even ldw at line %i\n",__LINE__);
    if ( q2/2+1 > ldw/2 ) die("no space for in-place r2c, line %i\n",__LINE__);

    DFTI_DESCRIPTOR_HANDLE fft1d = NULL;
    MKL_LONG err;

    DFTI_CONFIG_VALUE prec = sizeof(REAL)==sizeof(float) ? DFTI_SINGLE : DFTI_DOUBLE;
    err=DftiCreateDescriptor(&fft1d, prec, DFTI_REAL, 1, q2 );

    // Configure descriptor
    err=DftiSetValue(fft1d, DFTI_PLACEMENT,              DFTI_INPLACE);
    err=DftiSetValue(fft1d, DFTI_CONJUGATE_EVEN_STORAGE, DFTI_COMPLEX_COMPLEX);
    err=DftiSetValue(fft1d, DFTI_NUMBER_OF_TRANSFORMS,   p);
    err=DftiSetValue(fft1d, DFTI_INPUT_DISTANCE,         ldw); // in REALs
    err=DftiSetValue(fft1d, DFTI_OUTPUT_DISTANCE,        ldw/2); // in complex'es
    err=DftiSetValue(fft1d, DFTI_FORWARD_SCALE,          scale);

    err=DftiCommitDescriptor(fft1d);

    DftiComputeForward(fft1d, radon->data);

    DftiFreeDescriptor(&fft1d);
}

// Step 2: interpolation to Cartesian grid.
// ifreq_dom[x, y] <-- interpolation( freq_dom[theta, ksi] )
void step2_interpolation( MatrixC *fhat, MatrixR *radonImage )
{
    //radonImage is the result of r2c FFT
    //RT(pp,:) contains frequences 0...q
    complex *RT = (complex*)(radonImage->data);
    int q = (radonImage->w - 1) / 2; // w = 2q + 1
    int ldq = radonImage->ldw / 2;
    int p = radonImage->h;

    for (int i = 0; i < fhat->h; ++i)
    {
        for (int j = 0; j < fhat->w; ++j)
        {
            REAL yy = 2.0*i/fhat->h - 1; // yy = [-1...1]
            REAL xx = 2.0*j/fhat->w - 1; // xx = [-1...1]
            REAL r = sqrt(xx*xx + yy*yy);
            REAL phi = atan2(yy,xx);
            complex fhat_ij = complex(0.);
            if (r <= 1)
            {
                if (phi < 0)
                {
                    r = -r;
                    phi += M_PI;
                }

                int qq = floor(q + r * q + 0.5) - q; // qq = [-q...q)
                if (qq >= q) qq = q-1;

                int pp = floor(phi / M_PI * p + 0.5); // pp = [0...p)
                if (pp >= p) pp = p-1;


                if (qq >= 0) fhat_ij =      RT[pp*ldq + qq];
                else         fhat_ij = conj(RT[pp*ldq - qq]);

                if (isOdd(qq)) fhat_ij = -fhat_ij;
                if (isOdd(i)) fhat_ij = -fhat_ij;
                if (isOdd(j)) fhat_ij = -fhat_ij;
            }
            fhat->data[i*fhat->ldw + j] = fhat_ij;
        }
    }
}

// Step 3: inverse FFT
// ifreq_dom[x, y] <-- IFFT_2D( ifreq_dom[x, y] )
void step3_ifft_2d( MatrixC *fhat )
{
    DFTI_DESCRIPTOR_HANDLE ifft2d = NULL;
    MKL_LONG err;

    MKL_LONG sizes[2] = { fhat->h, fhat->w };

    DFTI_CONFIG_VALUE prec = sizeof(REAL)==sizeof(float) ? DFTI_SINGLE : DFTI_DOUBLE;
    err=DftiCreateDescriptor(&ifft2d, prec, DFTI_COMPLEX, 2, sizes);

    // Configure descriptor
    MKL_LONG strides[3] = { 0, fhat->ldw, 1 };
    err=DftiSetValue(ifft2d, DFTI_INPUT_STRIDES, strides);
    err=DftiCommitDescriptor(ifft2d);

    DftiComputeBackward(ifft2d, fhat->data);

    DftiFreeDescriptor(&ifft2d);
}

// Simplified BMP structure.
// See http://msdn.microsoft.com/en-us/library/dd183392(v=vs.85).aspx
#pragma pack(push, 1)
struct BMPHEADER
{
    char            bfType[2];
    unsigned int    bfSize;
    unsigned int    bfReserved;
    unsigned int    bfOffBits;

    unsigned int    biSize;
    unsigned int    biWidth;
    unsigned int    biHeight;
    unsigned short  biPlanes;
    unsigned short  biBitCount;
    unsigned int    biCompression;
    unsigned int    biSizeImage;
    unsigned int    biXPelsPerMeter;
    unsigned int    biYPelsPerMeter;
    unsigned int    biClrUsed;
    unsigned int    biClrImportant;
};
#pragma pack(pop)

// Read image from fname and convert it to gray-scale REAL.
void bmpRead(MatrixR *image, const char *fname )
{
    FILE *fp = fopen( fname, "rb" );
    if ( !fp ) die("cannot fopen %s\n",fname);

    BMPHEADER header;

    fread(&header, sizeof(header), 1, fp);
    if ( header.biBitCount != 24 ) die("not a 24-bit image in %s\n",fname);
    if ( header.biCompression ) die("%s is compressed bmp\n", fname);

    image->allocate( header.biHeight, header.biWidth, header.biWidth );
    if ( !image->data ) die("no memory to read %s\n", fname);

    fseek( fp, sizeof(header), SEEK_SET );
    for (int i = 0; i < image->h; ++i)
    {
        for (int j = 0; j < image->w; ++j)
        {
            struct { unsigned char b, g, r; } pixel;
            fread( &pixel, 3, 1, fp );
            REAL gray = (255 * 3.0 - pixel.r - pixel.g - pixel.b) / 255;
            image->data[i*image->ldw + j] = gray;
        }
        fseek( fp, (4 - 3*image->w % 4) % 4, SEEK_CUR ); // skip padding
    }
    fclose(fp);
}

inline REAL toREAL(const REAL& x) { return x; }
inline REAL toREAL(const complex& x) { return std::abs(x); }

template<typename T>
void bmpWrite_templ(const char *fname, const Matrix<T> *image)
{
    unsigned sizeof_line = (image->w*3 + 3) / 4 * 4;
    unsigned sizeof_image = image->h * sizeof_line;
    BMPHEADER header =
        { {'B','M'}, unsigned(sizeof(header)+sizeof_image), 0, sizeof(header),
          sizeof(header)-offsetof(BMPHEADER,biSize),
          unsigned(image->w), unsigned(image->h), 1, 24, 0,
          sizeof_image, 6000, 6000, 0, 0
    };
    FILE *fp = fopen( fname, "wb" );
    if ( !fp ) die("cannot fopen %s to save image\n",fname);

    fwrite( &header, sizeof(header), 1, fp );

    REAL minabs = 1e38, maxabs = 0;
    for (int i = 0; i < image->h; ++i)
        for (int j = 0; j < image->w; ++j)
        {
            REAL ijabs = toREAL(image->data[i*image->ldw + j]);
            if (!(ijabs > minabs)) minabs = ijabs;
            if (!(ijabs < maxabs)) maxabs = ijabs;
        }

    for (int i = 0; i < image->h; ++i)
    {
        for (int j = 0; j < image->w; ++j)
        {
            REAL ijabs = toREAL(image->data[i*image->ldw+j]);
            REAL gray = 255 * (ijabs - maxabs) / (minabs - maxabs);

            struct { unsigned char b, g, r; } pixel;
            pixel.b = pixel.g = pixel.r = (unsigned char)(gray + 0.5);
            fwrite( &pixel, 3, 1, fp );
        }
        for (int j = 3*image->w; j % 4; ++j)
            fputc(0,fp); // add padding
    }
    fclose(fp);
}

void bmpWrite(const char *fname, const MatrixR *image) { bmpWrite_templ(fname,image); }
void bmpWrite(const char *fname, const MatrixC *image) { bmpWrite_templ(fname,image); }

void acquireRadon(MatrixR *result, const MatrixR *input)
{
    PRAGMA_OMP(parallel for)
    for (int i = 0; i < result->h; ++i)
    {
        REAL theta = i*M_PI/result->h; // theta=[0,...,M_PI)
        for (int j = 0; j < result->w; ++j)
        {
            REAL s = -1. + (2.0*j + 1) / result->w; // s=(-1,...,1)
            REAL projection = integrateAlongLine( theta, s, input );
            result->data[i*result->ldw + j] = projection;
        }
    }
}

// Integrate image along line [(x,y),(cos theta, sin theta)] = R*s
REAL integrateAlongLine(REAL theta, REAL s, const MatrixR *image)
{
    REAL h = image->h, w = image->w;
    REAL R = 0.5 * sqrt(h * h + w * w);
    REAL S = s*R, B = sqrt(1-s*s)*R;
    REAL cs = cos(theta), sn = sin(theta);
    REAL dl = 1, dx = -dl*sn, dy = dl*cs; // integration step
    // unadjusted start of integration
    REAL x0 = 0.5*w + S*cs + B*sn;
    REAL y0 = 0.5*h + S*sn - B*cs;
    // unadjusted end of integration
    REAL x1 = 0.5*w + S*cs - B*sn;
    REAL y1 = 0.5*h + S*sn + B*cs;

    int N = 0; // number of sampling points on the interval
    do
    {
        // Adjust start-end of the integration interval
        if (x0 < 0) { if (x1 < 0) break; else { y0 -= (0 - x0)*cs/sn;  x0 = 0; }}
        if (y0 < 0) { if (y1 < 0) break; else { x0 -= (0 - y0)*sn/cs;  y0 = 0; }}
        if (x1 < 0) { if (x0 < 0) break; else { y1 -= (0 - x1)*cs/sn;  x1 = 0; }}
        if (y1 < 0) { if (y0 < 0) break; else { x1 -= (0 - y1)*sn/cs;  y1 = 0; }}
        if (x0 > w) { if (x1 > w) break; else { y0 -= (w - x0)*cs/sn;  x0 = w; }}
        if (y0 > h) { if (y1 > h) break; else { x0 -= (h - y0)*sn/cs;  y0 = h; }}
        if (x1 > w) { if (x0 > w) break; else { y1 -= (w - x1)*cs/sn;  x1 = w; }}
        if (y1 > h) { if (y0 > h) break; else { x1 -= (h - y1)*sn/cs;  y1 = h; }}
        // Compute number of steps
        N = int(std::abs(dx) > std::abs(dy) ? ((x1-x0) / dx) : ((y1-y0) / dy));
    } while(0);

    // Integrate
    REAL sum = 0;
    for (int n = 0; n < N; ++n)
    {
        int i = floor(y0 + n*dy + 0.5*dy);
        int j = floor(x0 + n*dx + 0.5*dx);
        sum += image->data[i*image->ldw + j];
    }
    sum *= dl;

    return sum;
}

void die(const char *fmt,...)
{
    va_list ap;
    va_start(ap,fmt);
    printf("Fatal error: ");
    vprintf(fmt,ap);
    va_end(ap);
    fflush(0);
    exit(1);
}
