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

#ifndef __NF_INIT_H__
#define __NF_INIT_H__

#include "mkl.h"

#define     ALIGN       64

/* Allocate memory */
void nfAllocate(MKL_INT m, MKL_INT n, double **x, double **mean, double **cor,
               double **cor_copy, double **eval, double **evect,
               double **work, int **iwork, int **isuppz,
               double **cov_signal, double **cov_noise);

/* Release memory */
void nfFree(double **x, double **mean, double **cor,
            double **cor_copy, double **eval, double **evect,
            double **work, int **iwork, int **isuppz,
            double **cov_signal, double **cov_noise);

/* Initialize VSL Summary Statistics task structure */
void nfInitSSTask(MKL_INT *m, MKL_INT *n, VSLSSTaskPtr *task, double *x,
                  MKL_INT *x_storage, double *mean, double *cor,
                  MKL_INT *cor_storage, double *W);

/* Delete VSL Summary Statistics task structure */
void nfFreeSSTask(VSLSSTaskPtr *task);
#endif
