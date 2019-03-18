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

#include "errcheck.h"
#include "nf_init.h"

/* Allocate memory */
void nfAllocate(MKL_INT m, MKL_INT n, double **x, double **mean, double **cor,
               double **cor_copy, double **eval, double **evect,
               double **work, int **iwork, int **isuppz,
               double **cov_signal, double **cov_noise)
{
    *x = (double *)mkl_malloc(m*n*sizeof(double), ALIGN);
    CheckMalloc(*x);

    *mean = (double *)mkl_malloc(n*sizeof(double), ALIGN);
    CheckMalloc(*mean);

    *cor = (double *)mkl_malloc(n*n*sizeof(double), ALIGN);
    CheckMalloc(*cor);

    *cor_copy = (double *)mkl_malloc(n*n*sizeof(double), ALIGN);
    CheckMalloc(*cor_copy);

    *eval = (double *)mkl_malloc(n*sizeof(double), ALIGN);
    CheckMalloc(*eval);

    *evect = (double *)mkl_malloc(n*n*sizeof(double), ALIGN);
    CheckMalloc(*evect);

    *work = (double *)mkl_malloc(26*n*sizeof(double), ALIGN);
    CheckMalloc(*work);

    *iwork = (int *)mkl_malloc(10*n*sizeof(int), ALIGN);
    CheckMalloc(*iwork);

    *isuppz = (int *)mkl_malloc(10*n*sizeof(int), ALIGN);
    CheckMalloc(*isuppz);

    *cov_signal = (double *)mkl_calloc(n*n, sizeof(double), ALIGN);
    CheckMalloc(*cov_signal);

    *cov_noise = (double *)mkl_calloc(n*n, sizeof(double), ALIGN);
    CheckMalloc(*cov_noise);
}

/* Release memory */
void nfFree(double **x, double **mean, double **cor,
            double **cor_copy, double **eval, double **evect,
            double **work, int **iwork, int **isuppz,
            double **cov_signal, double **cov_noise)
{
    mkl_free(*x);
    mkl_free(*mean);
    mkl_free(*cor);
    mkl_free(*cor_copy);
    mkl_free(*eval);
    mkl_free(*evect);
    mkl_free(*work);
    mkl_free(*iwork);
    mkl_free(*isuppz);
    mkl_free(*cov_signal);
    mkl_free(*cov_noise);
}

/* Initialize VSL Summary Statistics task structure */
void nfInitSSTask(MKL_INT *m, MKL_INT *n, VSLSSTaskPtr *task, double *x,
                  MKL_INT *x_storage, double *mean, double *cor,
                  MKL_INT *cor_storage, double *W)
{
    int status;

    /* Create VSL Summary Statistics task */
    *x_storage = VSL_SS_MATRIX_STORAGE_COLS;
    status = vsldSSNewTask(task, n, m, x_storage, x, 0, 0);
    CheckSSError(status);

    /* Register array of weights in the task */
    W[0] = 0.0;
    W[1] = 0.0;
    status = vsldSSEditTask(*task, VSL_SS_ED_ACCUM_WEIGHT, W);
    CheckSSError(status);

    /* Initialization of the task parameters using full storage
       for correlation matrix computation */
    *cor_storage = VSL_SS_MATRIX_STORAGE_FULL;
    status = vsldSSEditCovCor(*task, mean, 0, 0, cor, cor_storage);
    CheckSSError(status);
}

/* Delete VSL Summary Statistics task structure */
void nfFreeSSTask(VSLSSTaskPtr *task)
{
    int status;
    status = vslSSDeleteTask(task);
    CheckSSError(status);
    MKL_Free_Buffers();
}
