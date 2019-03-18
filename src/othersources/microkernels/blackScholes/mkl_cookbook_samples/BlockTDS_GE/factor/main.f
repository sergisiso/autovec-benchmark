!***********************************************************************
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
!***********************************************************************
!  Content:
!      Example of LU factorization of general block tridiagonal matrix
!  See respective recipe at: https://software.intel.com/en-us/node/507040
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Purpose:
! ========  
! Testing LU factorization of block tridiagonal matrix 
!          (D_1  C_1                          )
!          (B_1  D_2  C_2                     )
!          (     B_2  D_3  C_3                )
!          (           .........              )
!          (              B_N-2 D_N-1  C_N-1  )
!          (                    B_N-1  D_N    )
! provided by subroutine DGEBLTTRF by calculating Frobenius norm of the 
! residual ||A-L*U||. Computation of the residual and its Frobenius norm  
! is done by function RESID1 (for source see file auxi.f). 
! Input block tridiagonal matrix A is randomly generated.
!
      PROGRAM TEST
      IMPLICIT NONE
! ..Scalar parameters..
! N   INTEGER
!     The number of block rows of the matrix A.  N >= 0.
!
! NB  INTEGER
!     The size of blocks.  NB >= 0.
!
      INTEGER N, NB
      PARAMETER(N=611, NB=217)
!
! INFO  INTEGER
!     On exit from DGEBLTTRF:
!     = 0:        successful exit
!     = -1000     memory buffer was not allocated
!     < 0:        if INFO = -i, the i-th argument had an illegal value
!     > 0:        if INFO = i, U(i,i) is exactly zero. The factorization
!                 can be not completed. 
      INTEGER INFO
!
! I, J   INTEGER
!     Local loop indices
      INTEGER J, I
!
! EPS     REAL*8
!     Local parameter
      REAL*8 EPS
!
!
! ..Arrays..
! D       REAL*8 array, dimension (NB,N*NB)
!     On entry to DGEBLTTRF, the array stores N diagonal blocks (each of 
!         size NB by NB) of the matrix to be factored. The blocks are  
!         stored sequentially: first NB columns of D store block D_1, 
!         second NB columns store block D_2,...,last NB columns store 
!         block D_N. For testing purposes the array initialized randomly 
!         by calling DLARNV.
!     On exit from DGEBLTTRF, the array stores diagonal blocks of  
!         triangular factor L and U. Diagonal blocks of lower triangular 
!         factor L replace respective lower triangles of blocks D_j    
!         (1 <= j <= N). Diagonal units are not stored. Diagonal blocks  
!         of upper triangular factor U replace respective upper 
!         triangles of blocks D_j.
!
! DL      REAL*8 array, dimension (NB,(N-1)*NB)
!     On entry to DGEBLTTRF, the array stores N-1 subdiagonal blocks   
!         (each of size NBxNB) of the matrix to be factored. The blocks  
!         are stored sequentially: first NB columns of DL store block  
!         B_1, second NB columns store block B_2,...,last NB columns 
!         store block B_N-1.      
!     On exit from DGEBLTTRF, the array stores subdiagonal blocks of 
!         lower triangular factor L. For testing purposes the array is
!         initialized randomly by calling DLARNV.
!
! DU1     REAL*8 array, dimension (NB,(N-1)*NB)
!     On entry to DGEBLTTRF, the array stores N-1 superdiagonal blocks 
!         (each of size  NBxNB) of the matrix to be factored. The blocks 
!         are stored sequentially: first NB columns of DU1 store block 
!         C_1, second  NB columns store block C_2,...,last NB columns 
!         store block C_N-1. For testing purposes the array is initialized 
!         randomly by calling DLARNV.
!     On exit from DGEBLTTRF, the array stores superdiagonal blocks of 
!         triangular factor U.
!
! DU2     REAL*8 array, dimension (NB,(N-2)*NB)
!     On exit from DGEBLTTRF, the array stores blocks of the second 
!         superdiagonal of triangular factor U.
!
! DCPY   REAL*8 array, dimension (NB,N*NB)
!     Stores copy of the array D for testing purposes
!
! DLCPY  REAL*8 array, dimension (NB,(N-1)*NB)
!     Stores copy of the array DL for testing purposes
!
! DU1CPY REAL*8 array, dimension (NB,(N-1)*NB)
!     Stores copy of the array DU1 for testing purposes
      REAL*8 D(NB,N*NB), DL(NB,(N-1)*NB), DU1(NB,(N-1)*NB)
      REAL*8 DU2(NB,(N-2)*NB)
      REAL*8 DCPY(NB,N*NB), DLCPY(NB,(N-1)*NB), DU1CPY(NB,(N-1)*NB)
!
! IPIV    INTEGER array, dimension (NB,N)
!     On exit from DGEBLTTRF, stores pivot 'local' row indices  
!     ('local' means indices vary in the range 1..2*NB. Global row 
!     index is IPIV(I,K) + (K-1)*NB ).  
      INTEGER IPIV(NB,N)
!
! ISEED   INTEGER array, dimension 4
!     Used as seed parameter for random generation of matrix.
!     Initialized statically.
      INTEGER ISEED(4)
!
! .. External Functions ..
!
! RESID1  REAL*8
!     Function calculating residual ratio ||A-L*U||/||A||. 
!     For source see auxi.f
      REAL*8 RESID1
!
      DATA ISEED /9,41,11,3/

      PRINT *,"Testing accuracy of LU factorization with pivoting"
      PRINT *,"of randomly generated block tridiagonal matrix "
      PRINT *,"by calculating norm of the residual matrix."
!
! Initializing arrays randomly
      CALL DLARNV(2, ISEED, N*NB*NB, D)
      CALL DLARNV(2, ISEED, (N-1)*NB*NB, DL)
      CALL DLARNV(2, ISEED, (N-1)*NB*NB, DU1)
!
! Copying arrays for testing purposes
      CALL DCOPY(N*NB*NB, D, 1, DCPY, 1)
      CALL DCOPY((N-1)*NB*NB, DL, 1, DLCPY, 1)
      CALL DCOPY((N-1)*NB*NB, DU1, 1, DU1CPY, 1)
!
! Factoring the matrix
      CALL DGEBLTTRF(N, NB, D, DL, DU1, DU2, IPIV, INFO)
!
! Check the exit INFO for success
      IF(INFO .NE. 0) THEN
          PRINT *,"DGEBLTTRF returned nonzero INFO=",INFO
          STOP 1
      END IF
!
! Computing the ratio ||A - LU||_F/||A||_F
      EPS = RESID1(N, NB, DL, D, DU1, DU2, IPIV, DLCPY, DCPY, DU1CPY)
      PRINT *,"||A - LU||_F/||A||_F =",EPS
      END


