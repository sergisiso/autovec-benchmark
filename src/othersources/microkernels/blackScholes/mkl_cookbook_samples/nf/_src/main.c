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
!      Noise Filtration in Financial Data Streams Example
!******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "mkl.h"
#include "nf_init.h"
#include "nf.h"

/*
   This sample demonstrates the methodology (see [1], [2]) that allows
   to split a correlation matrix — representing the overall dependencies
   in data — into two components, a “signal” matrix and a “noise” matrix.

   Bibliography
   1. Zhang Zhang, Andrey Nikolaev, and Victoriya Kardakova (2012).
      Optimizing Correlation Analysis of Financial Market Data Streams
      Using Intel® Math Kernel Library, Intel Parallel Universe Magazine,
      Issue 12 (pp. 42-48).
   2. Hillol Kargupta, Krishnamoorthy Sivakumar, Samiran Ghost (2002).
      A Random Matrix-Based Approach for Dependency Detection
      from Data Streams.
      In Proceedings of PKDD'2002 (pp. 250-262). Springer-Verlag
*/

/* Routine to read the next portion of data from the input file */
void nfReadDataBlock(MKL_INT m, MKL_INT n, double *x, FILE *fh);

/* Routine to print matrix with m rows and n columns */
void nfPrintMatrix(MKL_INT m, MKL_INT n, double *x);

/* Routine to check the correctness of noise filtering */
int nfCheckResults(MKL_INT n, double *cov_total, double *cov_signal,
                   double *cov_noise);

int main()
{
    int i, errcode;
    MKL_INT n, m;
    int n_block;
    char filename[100];
    FILE *fh;
    long long pos = 0;

    double lambda_min;
    double lambda_max;
    double sqrt_n_m;
    double *cor_copy;
    double *eval, *evect;
    double *work;
    double *cov_signal, *cov_noise;

    int *iwork, *isuppz;

    /* VSL Summary Statistics parameters */
    VSLSSTaskPtr task;
    double *x, *mean, *cor;
    double W[2];
    MKL_INT x_storage, cor_storage;

    /* Read command line parameters */
    scanf("%d", &m);            // number of observations in block
    scanf("%d", &n);            // number of stocks (task dimension)
    scanf("%d", &n_block);      // number of data blocks
    scanf("%99s", filename);    // input data file

    /* Set threshold that define noise component  */
    sqrt_n_m = sqrt((double)n / (double)m);
    lambda_min = (1.0 - sqrt_n_m) * (1.0 - sqrt_n_m);
    lambda_max = (1.0 + sqrt_n_m) * (1.0 + sqrt_n_m);

    /* Allocate memory */
    nfAllocate(m, n, &x, &mean, &cor, &cor_copy, &eval, &evect,
               &work, &iwork, &isuppz, &cov_signal, &cov_noise);

    /* Initialize Summary Statistics task structure */
    nfInitSSTask(&m, &n, &task, x, &x_storage, mean, cor, &cor_storage, W);

    /* Open input file for reading */
    fh = fopen(filename, "r");
    if (!fh)
    {
        printf("Error: Couldn't open input file (%s) for reading\n", filename);
        exit(1);
    }

    /* Loop over data blocks */
    for (i = 0; i < n_block; i++)
    {
        /* Read next portion of data */
        nfReadDataBlock(m, n, x, fh);

        /* Update "signal" and "noise" covariance estimates */
        nfKernel(m, n, lambda_min, lambda_max, x, cor, cor_copy,
                 task, eval, evect, work, iwork, isuppz,
                 cov_signal, cov_noise);
    }

    /* Close input file */
    fclose(fh);

    /* Print results */
    printf("Task dimension: %d\n", n);
    printf("Number of observations in one block: %d\n", m);
    printf("Number of data blocks: %d\n", n_block);

    printf("\nSignal part of the covariance matrix:\n");
    nfPrintMatrix(n, n, cov_signal);

    printf("\nNoise part of the covariance matrix:\n");
    nfPrintMatrix(n, n, cov_noise);

    /* Check the correctness of the results */
    errcode = nfCheckResults(n, cor, cov_signal, cov_noise);
    if (errcode != 0)
    {
        printf("\nError: Signal and noise parts of covariance matrix");
        printf(" are not agreed with theory\n");
    }

    /* Release memory */
    nfFree(&x, &mean, &cor, &cor_copy, &eval, &evect,
           &work, &iwork, &isuppz, &cov_signal, &cov_noise);

    /* Delete Summary Statistics task */
    nfFreeSSTask(&task);

    return errcode;
}

/*
// Routine to read the next portion of data from the input file
//
//  INPUT:
//  m  - number of rows to read
//  n  - number of columns in each row
//  fh - file handle
//
//  OUTPUT:
//  x  - array containing the data read from input file
*/
void nfReadDataBlock(MKL_INT m, MKL_INT n, double *x, FILE *fh)
{
    MKL_INT i, j;
    for (i = 0; i < m; i++)
    {
        for (j = 0; j < n-1; j++)
        {
            fscanf(fh, "%lf,", x + i*n + j);
        }
        fscanf(fh, "%lf\n", x + i*n + n-1);
    }
}

/*
// Routine to print matrix with m rows and n columns
//
//  INPUT:
//  m  - number of rows in the input matrix
//  n  - number of columns in the input matrix
//  x  - input matrix stored as an array of doubles of size m x n
*/
void nfPrintMatrix(MKL_INT m, MKL_INT n, double *x)
{
    MKL_INT i, j;
    for (i = 0; i < m; i++)
    {
        for (j = 0; j < n-1; j++)
        {
            printf("%7.4lf, ", x[i*n + j]);
        }
        printf("%7.4lf\n", x[i*n + n-1]);
    }
}

/*
// Routine to check the correctness of noise filtering
//
// Check that signal and noise parts of covariance matrix
// sum to total covariance
//
//  INPPUT:
//  n          - task dimension
//  cov_total  - covariance matrix
//  cov_signal - signal part of covariance matrix
//  cov_noise  - noise  part of covariance matrix
//
//  RETURN VALUE:
//  0, in case of success; 1 - otherwise
*/
int nfCheckResults(MKL_INT n, double *cov_total, double *cov_signal,
                   double *cov_noise)
{
    MKL_INT i, j;
    double tol = 1.0e-8;
    for (i = 0; i < n; i++)
    {
		for (j = 0; j <= i; j++)
		{
	        if (fabs(cov_signal[i*n + j] + cov_noise[i*n + j] - cov_total[i*n + j]) > tol)
	        {
	            return 1;
	        }
		}
    }
    return 0;
}
