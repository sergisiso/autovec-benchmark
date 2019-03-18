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

#include <math.h>

#include "nf.h"
#include "errcheck.h"

/* Calculate signal and noise components of covariance matrix
   from previously computed their eigenvalues and eigenvectors  */
int nfCalculateSignalNoiseCov(int n, int n_signal, int n_noise,
        double *eval, double *evect, double *cor, double *cov_signal,
        double *cov_noise);

/*
//  Noise Filtration algorithm implementation
//
//  INPUT:
//  m           - number of observations in the input data block
//  n           - task dimension
//  lambda_min  - left border of the interval of eigenvalues that correspond
//                to the noise components
//  lambda_max  - right border of the interval of eigenvalues that correspond
//                to the noise components
//  x           - new block of data - a matrix of size m x n
//  cor         - correlation matrix of size n x n
//  cor_copy    - copy of the correlation matrix which is used for eigenvectors
//                calculation by DSYEVR
//  task        - VSL Summary Statistics task descriptor
//  work,iwork,isuppz - buffers for storing intermediate results of DSYEVR
//
//  OUTPUT:
//  eval        - eigenvalues of the correlation matrix cor which belong
//                to the interval (lambda_min, lambda_max).
//  evect       - eigenvectors that corresponding the eigenvalues eval
//  cov_signal  - "signal" part of the covariance matrix
//  cov_noise   - "noise" part of the covariance matrix
//
//  RETURN VALUE:
//  Error status
//
//  ALGORITHM DESCRIPTION
//  Let X(t) be an m x n random matrix whose entries are independent
//  identically distributed random values - part of the data set
//  obtained at time t.
//  Need to update signal and noise parts of covariance matrix
//  of the whole data set with new portion of the data - X(t).
//
//  The algorithm consist of 3 parts:
//  1. Update the covariance matrix using X(t)
//  2. Calculate eigenvalues and eigenvectors of the covariance matrix
//  3. Update signal and noise components of covariance matrix
//     depending on eigenvalues obtained on the step 2.
//
//  REFERENCES
//  [1] Hillol Kargupta, Krishnamoorthy Sivakumar and Samiran Ghosh
//      Random matrices and online principal component analysis
//      from data streams
*/
void nfKernel(MKL_INT m, MKL_INT n, double lambda_min, double lambda_max,
              double *x, double *cor, double *cor_copy,
              VSLSSTaskPtr task, double *eval, double *evect,
              double *work, int *iwork, int *isuppz,
              double *cov_signal, double *cov_noise)
{
    MKL_INT i, j;
    int n_signal, n_noise;
    int errcode;

    /* SYEVR parameters */
    char jobz, range, uplo;
    int  info, lwork, liwork;
    int imin, imax;
    double lmin, lmax;
    double  abstol;

    /* Register new portion of data in Summary Statistics task structure */
    errcode = vsldSSEditTask(task, VSL_SS_ED_OBSERV, x);
    CheckSSError(errcode);

    /* Update correlation matrix estimate using FAST method */
    errcode = vsldSSCompute(task, VSL_SS_COR, VSL_SS_METHOD_FAST);
    CheckSSError(errcode);

    for (i = 0; i < n*n; i++) cor_copy[i] = cor[i];

    for (i = 0; i < n; i++) cor_copy[i*n + i] = 1.0;

    /* Compute eigenvalues and eigenvectors of the correlation matrix */
    jobz  = 'V';    // both eigenvalues and eigenvectors are computed
    range = 'V';    // eigenvalues from a half-open interval
                    // vl < lambda(i) <= vu are computed
    uplo  = 'U';    // lower triangle of the input matrix is used
                    // to compute the results
    lwork  = 26*n;
    abstol = 0.0;
    imin = 1;
    imax = n;
    lmin = lambda_min;
    lmax = lambda_max;
    liwork = 10*n;

    dsyevr(&jobz, &range, &uplo, &n, cor_copy, &n, &lmin, &lmax,
           &imin, &imax, &abstol, &n_noise, eval, evect, &n, isuppz,
           work, &lwork, iwork, &liwork, &info);
    if (info != 0)
    {
        printf("Error in SYEVR, code = %d\n", info);
        exit(1);
    }

    n_signal = n - n_noise;

    /* Calculate "signal" and "noise" part of covariance matrix */
    nfCalculateSignalNoiseCov(n, n_signal, n_noise,
        eval, evect, cor, cov_signal, cov_noise);
}

/*
//  Calculate "signal" and "noise" part of covariance matrix
//  from previously computed their eigenvalues and eigenvectors
//
//  INPUT:
//  n        - task dimension
//  n_signal - number of eigenvalues that correspond to signal part
//             of covariance matrix
//  n_noise  - number of eigenvalues that correspond to noise part
//             of covariance matrix
//  eval     - eigenvalues of covariance matrix
//  evect    - eigenvectors of covariance matrix
//  cor      - covariance matrix
//
//  OUTPUT:
//  cov_signal - signal part of covariance matrix
//  cov_noise  - noise part of covariance matrix
//
//  RETURN VALUE:
//  Error status
*/
static int nfCalculateSignalNoiseCov(int n, int n_signal, int n_noise,
        double *eval, double *evect, double *cor, double *cov_signal,
        double *cov_noise)
{
    int i, j;
    double lambda_i;
    double *evect_ptr, *evect_noise;

    /* SYRK parameters */
    char uplo, trans;
    int nn;
    double alpha, beta;

    /* Compute signal and noise components of covariance matrix.
       Cov_s = A_s * L_s * A_s' = ( A_s * SQRT(L_s) ) * ( A_s * SQRT(L_s) )',
       where
            A_s - matrix containing eigenvectors of noise
            L_s - diagonal matrix containing eigenvalues of noise
       Signal covariance matrix is computed by subtracting noise part
       from the total correlation estimate. */

    for (j = 0; j < n_noise; j++) eval[j] = sqrt(eval[j]);

    evect_ptr = evect;
    for (i = 0; i < n_noise; i++)
    {
        lambda_i = eval[i];
        for (j = 0; j < n; j++)
        {
            evect_ptr[j] *= lambda_i;
        }

        evect_ptr += n;
    }

    evect_noise = evect;

    uplo  = 'U';
    trans = 'N';
    alpha = 1.0;
    beta  = 0.0;
    nn = n;

    if (n_noise > 0)
    {
        dsyrk(&uplo, &trans, &nn, &n_noise,  &alpha, evect_noise, &nn,
              &beta, cov_noise, &nn);
    }

    if (n_signal > 0)
    {
        for (i = 0; i < n; i++)
        {
            for (j = 0; j <= i; j++)
            {
                cov_signal[i*n + j] = cor[i*n + j] - cov_noise[i*n + j];
            }
        }
    }
    else
    {
        for (i = 0; i < n*n; i++)
        {
            cov_signal[i] = 0.0;
        }
    }

    return 0;
}
