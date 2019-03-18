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
*
* suppliers or licensors in any way.
*
********************************************************************************
*   Content : Example of solving nonlinear problem using PARDISO and Sparse BLAS
*   functionality
*
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "memory.h"

#include "mkl.h"
#include "mkl_types.h"
#include "mkl_spblas.h"
#include "mkl_service.h"

void construct_matrix_structure(MKL_INT nx, MKL_INT ny, MKL_INT nz, MKL_INT** ia_ptr, MKL_INT** ja_ptr, double** a_ptr, MKL_INT *error);
void construct_matrix_values(MKL_INT nx, MKL_INT ny, MKL_INT nz, double*u, MKL_INT* ia, MKL_INT* ja, double* a_ptr, MKL_INT *error);
MKL_INT finalize(void *pt, MKL_INT maxfct, MKL_INT mnum, MKL_INT mtype, MKL_INT phase, MKL_INT n, 
                 MKL_INT nrhs, MKL_INT *iparm, MKL_INT msglvl,
                 double *u, double *us, double *f, double *fs, 
                 double *a, MKL_INT *ia, MKL_INT *ja,  
                 MKL_INT error);

MKL_INT main (void)
{
    MKL_INT nx = 5;             /* mesh size */
    MKL_INT ny = 5;             /* mesh size */
    MKL_INT nz = 5;             /* mesh size */
    MKL_INT n;                  /* size of the square matrix a */
    double h = 1./nx;           /* mesh parameter (in case of uniform mesh) */
    MKL_INT mtype = 11;         /* real nonsymmetric matrix */
    double  *a = NULL, *f = NULL, *fs = NULL, *u = NULL, *us = NULL, res;
    MKL_INT *ia = NULL, *ja = NULL;
    MKL_INT nrhs = 1;           /* number of right hand sides. */
    double minus_one = -1.;
    MKL_INT one = 1;

    /* PARDISO control parameters */
    void    *pt[64];
    MKL_INT iparm[64];
    MKL_INT maxfct, mnum, phase, error, msglvl;
    MKL_INT i, j, kk, k, iter = 0;
    double  ddum;               /* double dummy */
    MKL_INT idum;               /* integer dummy. */
    char    *uplo;
    uplo = "non-transposed";
    n = (nx+1)*(ny+1)*(nz+1);

    /* Setup PARDISO control parameters. */
    for ( i = 0; i < 64; i++ )
    {
        iparm[i] = 0;
    }
    iparm[0]  = 1;        /* No default values for solver */
    iparm[1]  = 2;        /* Fill-in reducing ordering from METIS */
    iparm[3]  = 0;        /* No iterative-direct algorithm */
    iparm[4]  = 0;        /* No user fill-in reducing permutation */
    iparm[5]  = 0;        /* Write solution on x */
    iparm[6]  = 0;        /* Not in use */
    iparm[7]  = 2;        /* Maximum number of iterative refinement steps */
    iparm[8]  = 0;        /* Not in use */
    iparm[9]  = 13;       /* Perturb the pivot elements with 1E-13 */
    iparm[10] = 1;        /* Use nonsymmetric permutation and scaling MPS */
    iparm[11] = 0;        /* Solve with transposed/conjugate transposed matrix*/
    iparm[12] = 1;        /* Maximum weighted matching algorithm is enabled (default for nonsymmetric matrices) */
    iparm[13] = 0;        /* Output: Number of perturbed pivots */
    iparm[14] = 0;        /* Not in use */
    iparm[15] = 0;        /* Not in use */
    iparm[16] = 0;        /* Not in use */
    iparm[17] = -1;       /* Output: Number of non-zero values in the factor LU */
    iparm[18] = -1;       /* Output: Mflops for LU factorization */
    iparm[19] = 0;        /* Output: Numbers of CG Iterations */
    iparm[34] = 1;        /* Zero based indexing */
    maxfct    = 1;        /* Maximum number of numerical factorizations. */
    mnum      = 1;        /* Which factorization to use. */
    msglvl    = 0;        /* Print statistical information in file */
    error     = 0;        /* Initialize error flag */
    phase     = -10;        /* Before PARDISO call flag */

    /* Set matrix parameter corresponding to its structure */
    construct_matrix_structure(nx,ny,nz, &ia, &ja, &a, &error);
    if ( error != 0 ) return finalize( pt, maxfct, mnum, mtype, phase, n, nrhs, iparm, msglvl, 
                                       u, us, f, fs, a, ia, ja, -2);

    /* Allocate memory for solution and rhs vector */
    if (((u  = (double*) mkl_malloc( sizeof (double) * n, 64 )) == NULL ) ||
        ((us = (double*) mkl_malloc( sizeof (double) * n, 64 )) == NULL ) || 
        ((f  = (double*) mkl_malloc( sizeof (double) * n, 64 )) == NULL ) ||
        ((fs = (double*) mkl_malloc( sizeof (double) * n, 64 )) == NULL ) )
        return finalize( pt, maxfct, mnum, mtype, phase, n, nrhs, iparm, msglvl,  
                         u, us, f, fs, a, ia, ja, -2);

    for ( i = 0; i < n; i++ )
    {
        f[i] = h*h;
        u[i] = 0.;
        us[i] = 0.;
    }

    /* Set matrix value corresponding to current solution vector us */
    construct_matrix_values(nx, ny, nz, us, ia, ja, a, &error);
    if ( error != 0 ) return finalize( pt, maxfct, mnum, mtype, phase, n, nrhs, iparm, msglvl, 
                                       u, us, f, fs, a, ia, ja, error);

    /* Initialize PARDISO handle */
    for ( i = 0; i < 64; i++ )
    {
        pt[i] = 0;
    }

    /* Continue loop over nonlinearity until required value of residual is reached */
    res = 1.0;
    while ( res > 1.e-8 )
    {
        phase = 13;
        PARDISO ( pt, &maxfct, &mnum, &mtype, &phase, 
                  &n, a, ia, ja, &idum, &nrhs, iparm, &msglvl, f, u, &error );
        if ( error != 0 ) return finalize( pt, maxfct, mnum, mtype, phase, n, nrhs, iparm, msglvl, 
                                           u, us, f, fs, a, ia, ja, error);

        dcopy(&n, u, &one, us, &one);

        construct_matrix_values( nx, ny, nz, us, ia, ja, a, &error );
        if ( error != 0 ) return finalize( pt, maxfct, mnum, mtype, phase, n, nrhs, iparm, msglvl, 
                                           u, us, f, fs, a, ia, ja, error);

    /* Compute residual */
        mkl_cspblas_dcsrgemv( uplo, &n, a, ia, ja, u, fs );

        res = 0.;

        daxpy(&n, &minus_one, f, &one, fs, &one);

        res = dnrm2(&n, fs, &one);

        printf ("Relative residual = %e\n", res);
    }

    /* Set boundary condition */
    for (j = 0; j < ny + 1; j++) {
        for (k = 0; k < nz + 1; k++) {
            u[  k  *(nx+1)*(ny+1)+  j  *(nx+1)+  0  ] = 0.;
            u[  k  *(nx+1)*(ny+1)+  j  *(nx+1)+  nx ] = 0.;
        }
    }

    for (i = 0; i < nx + 1; i++) {
        for (k = 0; k < nz + 1; k++) {
            u[  k  *(nx+1)*(ny+1)+  0  *(nx+1)+  i  ] = 0.;
            u[  k  *(nx+1)*(ny+1)+  ny *(nx+1)+  i  ] = 0.;
        }
    }

    for (i = 0; i < nx + 1; i++) {
        for (j = 0; j < ny + 1; j++) {
            u[  0  *(nx+1)*(ny+1)+  j  *(nx+1)+  i  ] = 0.;
            u[  nz *(nx+1)*(ny+1)+  j  *(nx+1)+  i  ] = 0.;
        }
    }

    /*----------------------------------------------------------------------------------
    ** Release internal Intel(R) MKL memory that might have been used for computations  
    ** NOTE: It is important to call the routine below to avoid memory leaks            
    ** unless you disable Intel MKL Memory Manager                                      
    **--------------------------------------------------------------------------------*/
    return finalize( pt, maxfct, mnum, mtype, phase, n, nrhs, iparm, msglvl, 
                     u, us, f, fs, a, ia, ja, error);

}

/* This function constructs matrix structure based on 7-point grid Laplace operator in csr format.

   Because values of matrix change in each iteration, this function sets only 2 of 3 arrays in csr
   format: ia and ja.
   The matrix structure is based on the following stencil:
    
    -u(i,j,k-1)-u(i,j-1,k)-u(i-1,j,k)+6*u(i,j,k)-u(i+1,j,k)-u(i,j+1,k)-u(i,j,k+1) = f(i,j,k)

   Dirichlet boundary conditions are used. nx, ny, nz are the mesh sizes of computational domain. */


void construct_matrix_structure( MKL_INT nx, MKL_INT ny, MKL_INT nz, MKL_INT** ia_ptr, MKL_INT** ja_ptr, double** a_ptr, MKL_INT *error )
{
    MKL_INT i,j,k,kk, nonzero_elements, temp1, temp2, n_local;
    MKL_INT *ia, *ja;
    double *a;
    *error = 0;

    if ( ( nx < 1 ) || ( ny < 1 ) || ( nz < 1 ) )
    {
        *error = -1;
        return;
    }
    /* Allocate memory for ia array */
    if (( ia = (MKL_INT*) mkl_malloc (sizeof (MKL_INT) * ((nx+1)*(ny+1)*(nz+1)+1), 64)) == NULL )
    {
        *error = -2;
        return;
    }

    /* Calculate number of elements in each string of matrix; required to set ia array */
    for ( k = 0; k < nz+1; k++ )
    {
        for ( j = 0; j < ny+1; j++ )
        {
            for ( i = 0; i < nx+1; i++ )
            {
                nonzero_elements = 7;
                if ( (k==0) || (k==nz) ) nonzero_elements--;
                if ( (j==0) || (j==ny) ) nonzero_elements--;
                if ( (i==0) || (i==nx) ) nonzero_elements--;
                ia[k*(ny+1)*(nx+1)+j*(nx+1)+i] = nonzero_elements;
            }
        }
    }

    /* Set ia array */
    temp1 = ia[0];
    ia[0] = 0;

    for ( kk = 1; kk <= (nx+1)*(ny+1)*(nz+1); kk++ )
    {
        temp2  = ia[kk];
        ia[kk] = ia[kk-1] + temp1;
        temp1  = temp2;
    }

    /* Allocate memory for ja and a arrays */
    if (( ja = (MKL_INT*) mkl_malloc (sizeof (MKL_INT) * ia[(nx+1)*(ny+1)*(nz+1)], 64)) == NULL )
    {
        free( ia );
        *error = -2;
        return;
    }

    if (( a = (double*) mkl_malloc (sizeof (double) * ia[(nx+1)*(ny+1)*(nz+1)], 64)) == NULL )
    {
        free( ia );
        free( ja );
        *error = -2;
        return;
    }

    /* Set ja array */
    n_local = -1;
    for ( k = 0; k < nz+1; k++ )
    {
        for ( j = 0; j < ny+1; j++ )
        {
            for ( i = 0; i < nx+1; i++ )
            {
                if ( k > 0 )  {n_local++; ja[n_local]=(k-1)*(nx+1)*(ny+1)+(j  )*(nx+1)+i  ;}
                if ( j > 0 )  {n_local++; ja[n_local]=(k  )*(nx+1)*(ny+1)+(j-1)*(nx+1)+i  ;}
                if ( i > 0 )  {n_local++; ja[n_local]=(k  )*(nx+1)*(ny+1)+(j  )*(nx+1)+i-1;}
                               n_local++; ja[n_local]=(k  )*(nx+1)*(ny+1)+(j  )*(nx+1)+i;
                if ( i < nx ) {n_local++; ja[n_local]=(k  )*(nx+1)*(ny+1)+(j  )*(nx+1)+i+1;}
                if ( j < ny ) {n_local++; ja[n_local]=(k  )*(nx+1)*(ny+1)+(j+1)*(nx+1)+i  ;}
                if ( k < nz ) {n_local++; ja[n_local]=(k+1)*(nx+1)*(ny+1)+(j  )*(nx+1)+i  ;}
            }
        }
    }

    *ia_ptr = ia;
    *ja_ptr = ja;
    *a_ptr  = a;
}

/* This function sets matrix value of array a composed of 3 arrays of csr format. Matrix structure is based
   on the following  stencil

    - u(i,j,k-1)*(m(i,j,k)+m(i,j,k-1))/2 + u(i,j,k)*(m(i,j,k-1)+2*m(i,j,k)+m(i,j,k+1))/2 - u(i,j,k+1)*(m(i,j,k)+m(i,j,k+1))/2 - 
      u(i,j-1,k)*(m(i,j,k)+m(i,j-1,k))/2 + u(i,j,k)*(m(i,j-1,k)+2*m(i,j,k)+m(i,j+1,k))/2 - u(i,j+1,k)*(m(i,j,k)+m(i,j+1,k))/2 - 
      u(i-1,j,k)*(m(i,j,k)+m(i-1,j,k))/2 + u(i,j,k)*(m(i-1,j,k)+2*m(i,j,k)+m(i+1,j,k))/2 - u(i+1,j,k)*(m(i,j,k)+m(i+1,j,k))/2
    = f(i,j,k)*h*h

    Dirichlet boundary condition is used, where

    m(i,j,k) = (1 + alpha*u(i,j,k)), u(i,j,k) is a solution of the previous nonlinear step.
    nx, ny, nz are the mesh sizes of computational domain.
    h is a mesh parameter.
*/

void construct_matrix_values( MKL_INT nx, MKL_INT ny, MKL_INT nz, double*u, MKL_INT* ia, MKL_INT* ja, double* a, MKL_INT *error )
{
    MKL_INT i, j, k, i1, j1, k1, ii, jj, kk, num_el;
    double t;
    double alpha = 10.;
    double add;
    *error = 0;

    if ( ( nx < 1 ) || ( ny < 1 ) || ( nz < 1 ) )
    {
        *error = -1;
        return;
    }

    /* Initialize the matrix with unit matrix */
    for ( kk = 0; kk < ((nx+1)*(ny+1)*(nz+1)); kk++ )
    {
        for ( jj = ia[kk]; jj < ia[kk+1]; jj++ )
        {
            if (ja[jj] == kk)
                a[jj] = 1.0;
            else
                a[jj] = 0.0;
        }
    }

    /* Set correct values in the matrix */
    for ( k = 1; k < nz; k++ )
    {
        for ( j = 1; j < ny; j++ )
        {
            for ( i = 1; i < nx; i++ )
            {
                num_el = ia[k*(nx+1)*(ny+1)+j*(nx+1)+i] + 3;
                t = u[k*(nx+1)*(ny+1)+j*(nx+1)+i];

                add = 0.0;

                if ( k > 1 )    {a[num_el-3] = -1.0*(1+0.5*alpha*(u[(k-1)*(nx+1)*(ny+1)+  j  *(nx+1)+  i  ]+t)); add += u[(k-1)*(nx+1)*(ny+1)+  j  *(nx+1)+  i  ];} 
                else { a[num_el-3] = 0.0;}

                if ( j > 1 )    {a[num_el-2] = -1.0*(1+0.5*alpha*(u[  k  *(nx+1)*(ny+1)+(j-1)*(nx+1)+  i  ]+t)); add += u[  k  *(nx+1)*(ny+1)+(j-1)*(nx+1)+  i  ];} 
                else { a[num_el-2] = 0.0;}

                if ( i > 1 )    {a[num_el-1] = -1.0*(1+0.5*alpha*(u[  k  *(nx+1)*(ny+1)+  j  *(nx+1)+(i-1)]+t)); add += u[  k  *(nx+1)*(ny+1)+  j  *(nx+1)+(i-1)];} 
                else { a[num_el-1] = 0.0;}

                if ( i < nx-1 ) {a[num_el+1] = -1.0*(1+0.5*alpha*(u[  k  *(nx+1)*(ny+1)+  j  *(nx+1)+(i+1)]+t)); add += u[  k  *(nx+1)*(ny+1)+  j  *(nx+1)+(i+1)];} 
                else { a[num_el+1] = 0.0;}

                if ( j < ny-1 ) {a[num_el+2] = -1.0*(1+0.5*alpha*(u[  k  *(nx+1)*(ny+1)+(j+1)*(nx+1)+  i  ]+t)); add += u[  k  *(nx+1)*(ny+1)+(j+1)*(nx+1)+  i  ];} 
                else { a[num_el+2] = 0.0;}

                if ( k < nz-1 ) {a[num_el+3] = -1.0*(1+0.5*alpha*(u[(k+1)*(nx+1)*(ny+1)+  j  *(nx+1)+  i  ]+t)); add += u[(k+1)*(nx+1)*(ny+1)+  j  *(nx+1)+  i  ];} 
                else { a[num_el+3] = 0.0;}

                                 a[num_el  ] =  3.0*(1+    alpha* u[  k  *(nx+1)*(ny+1)+  j  *(nx+1)+  i  ]) + 3.0 + alpha*0.5*add;
            }
        }
    }
}

/* This function clean-up memory, print status and return actual error */
MKL_INT finalize(void *pt, MKL_INT maxfct, MKL_INT mnum, MKL_INT mtype, MKL_INT phase,
             MKL_INT n, MKL_INT nrhs, MKL_INT *iparm, MKL_INT msglvl,
             double *u, double *us, double *f, double *fs, 
             double *a, MKL_INT *ia, MKL_INT *ja,  
             MKL_INT error)
{
    double  ddum;               /* double dummy */
    MKL_INT idum;               /* integer dummy */
    MKL_INT local_error = 0;

    /* Release allocated memory */
    mkl_free( u  );
    mkl_free( us );
    mkl_free( f  );
    mkl_free( fs );
    mkl_free( a  );
    mkl_free( ia );
    mkl_free( ja );

    /* Release internal PARDISO memory */
    if ( phase != -10 )
    {
        MKL_INT local_phase = -1;
        PARDISO ( pt, &maxfct, &mnum, &mtype, &local_phase, 
                  &n, &ddum, &idum, &idum, &idum, &nrhs, iparm, &msglvl, &ddum, &ddum, &local_error );
    }
    if ( local_error != 0 ) 
    {
        printf ("\nThis example has FAILED during release internal PARDISO memory on phase %d !\n", phase);
        return local_error;
    }

    if ( error != 0) 
    {
        printf ("\nThis example has FAILED on phase %d !\n", phase);
    }
    else
    {
        printf ("\nThis example has successfully PASSED through all computation steps!\n");
    }
    return error;
}