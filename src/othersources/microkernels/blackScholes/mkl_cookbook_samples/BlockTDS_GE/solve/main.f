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
!      Example of solving a system of linear equations with general 
!      block tridiagonal coefficient matrix 
!  See respective recipe at: https://software.intel.com/en-us/node/515759 
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Purpose:
! ========  
! Testing solution of a linear system of equations with general block 
! tridiagonal matrix of coefficients
!          D(1)*X(1) +     C(1)*X(2)               = F(1)
!          B(1)*X(1) +     D(2)*X(2) +   C(2)*X(3) = F(2) 
!          B(2)*X(2) +     D(3)*X(3) +   C(3)*X(4) = F(3) 
!      ...
!      B(N-2)*X(N-2) + D(N-1)*X(N-1) + C(N-1)*X(N) = F(N-1) 
!                      B(N-1)*X(N-1) +   D(N)*X(N) = F(N) 
! Here D(J),B(J),C(J) are NB by NB matrices - block matrix coefficients
!     X(J),F(J) are NB by NRHS-matrices - unknowns and RHS components
!
! Solving is done via LU factorization of the coefficient matrix 
! (call DGEBLTTRF) followed by call DGEBLTTRS to solve a system of
! equations with coefficient matrix factored by DGEBLTTRF.
!
! Coefficients and right hand sides are randomly generated.
!
! Testing is done via calculating 
!      max{||F(1)-D(1)*X(1)-C(1)*X(2)||,
!          ||F(2)-B(1)*X(1)-D(1)*X(1)-C(1)*X(2)||,
!           ...
!           ||F(N)-B(N-1)*X(N-1)-D(N)*X(N)||}
!
! ||.|| denotes Frobenius norm of a respective matrix           
      PROGRAM TEST
      IMPLICIT NONE
! ..Scalar parameters..
! N   INTEGER,    N > 0.
!     The number of matrix equations to solve (block rows in the 
!     coefficient matrix).  
!
! NB  INTEGER,     NB > 0.
!     The size of blocks. 
!
! NRHS  INTEGER,     NRHS > 0.
!     The number of right hand sides. 
!
! LDF INTEGER,     LDF >= N*NB
!     Leading dimension of the matrix of right hand sides.
      INTEGER N, NB, NRHS, LDF
      PARAMETER(N=523, NB=5, NRHS=32, LDF = NB*N)
!
! INFO  INTEGER
!     On exit from DGEBLTTRF:
!     = 0:        successful exit
!     = -1000     memory buffer was not allocated
!     < 0:        if INFO = -i, the i-th argument had an illegal value
!     > 0:        if INFO = i, U(i,i) is exactly zero. The factorization
!                 can be not completed. 
!
!     On exit from DGEBLTTRS:
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
      INTEGER INFO
!
! I, J   INTEGER
!     Local loop indices
      INTEGER I, J
!
! EPS     REAL*8
!     Local parameter
      REAL*8 EPS
!
! ..Arrays..
! D       REAL*8 array, dimension (NB,N*NB)
!     On entry to DGEBLTTRF, the array stores N diagonal blocks (each of 
!         size NB by NB) of the matrix to be factored. The blocks are  
!         stored sequentially: first NB columns of D store block D_1, 
!         second NB columns store block D_2,...,last NB columns store 
!         block D_N. For testing purposes the array initialized randomly 
!         by calling DLARNV.
!     On exit from DGEBLTTRF (entry to DGEBLTTRS), the array stores   
!         diagonal blocks of triangular factor L and U. Diagonal blocks 
!         of lower triangular factor L replace respective lower triangles
!         of blocks D_j (1 <= j <= N). Diagonal units are not stored. 
!         Diagonal blocks of upper triangular factor U replace respective 
!         upper triangles of blocks D_j.
!
! DL      REAL*8 array, dimension (NB,(N-1)*NB)
!     On entry to DGEBLTTRF, the array stores N-1 subdiagonal blocks   
!         (each of size NBxNB) of the matrix to be factored. The blocks  
!         are stored sequentially: first NB columns of DL store block  
!         B_1, second NB columns store block B_2,...,last NB columns 
!         store block B_N-1. For testing purposes the array is 
!         initialized randomly by calling DLARNV.     
!     On exit from DGEBLTTRF (entry to DGEBLTTRS), the array stores 
!         subdiagonal blocks of lower triangular factor L. 
!
! DU1     REAL*8 array, dimension (NB,(N-1)*NB)
!     On entry to DGEBLTTRF, the array stores N-1 superdiagonal blocks 
!         (each of size  NBxNB) of the matrix to be factored. The blocks 
!         are stored sequentially: first NB columns of DU1 store block 
!         C_1, second  NB columns store block C_2,...,last NB columns 
!         store block C_N-1. For testing purposes the array is initialized 
!         randomly by calling DLARNV.
!     On exit from DGEBLTTRF (entry to DGEBLTTRS), the array stores 
!         superdiagonal blocks of triangular factor U.
!
! DU2     REAL*8 array, dimension (NB,(N-2)*NB)
!     On exit from DGEBLTTRF (entry to DGEBLTTRS), the array stores  
!         blocks of the second superdiagonal of triangular factor U.
      REAL*8 D(NB,N*NB), DL(NB,(N-1)*NB), DU1(NB,(N-1)*NB)
      REAL*8 DU2(NB,(N-2)*NB)
!
! DCPY   REAL*8 array, dimension (NB,N*NB)
!     Stores copy of the array D for testing purposes
!
! DLCPY  REAL*8 array, dimension (NB,(N-1)*NB)
!     Stores copy of the array DL for testing purposes
!
! DU1CPY REAL*8 array, dimension (NB,(N-1)*NB)
!     Stores copy of the array DU1 for testing purposes
      REAL*8 DCPY(NB,N*NB), DLCPY(NB,(N-1)*NB), DU1CPY(NB,(N-1)*NB)
!
! F   REAL*8 array, dimension (LDF,NRHS)
!     On entry to DGEBLTTRS stores right hand side vectors.
!     On exit from DGEBLTTRS stores unknowns.
!
! FCPY REAL*8 array, dimension (LDF,NRHS)
!     On entry to RESID2 stores copy of F for testing purposes
!      
      REAL*8 F(LDF,NRHS), FCPY(LDF,NRHS)
!
! IPIV INTEGER array, dimension (NB,N)
!     On exit from DGEBLTTRF (entry to DGEBLTTRS), the array stores
!         pivot indices.
      INTEGER IPIV(NB,N)
!     
! ISEED INTEGER array, dimension 4
!     The array stores 'seed' numbers for generator DLARNV.
!          To be initialized before the first call DLARNV.
      INTEGER ISEED(4)
!
! .. External Functions ..
!
! RESID2  REAL*8
!     Function calculating 
!         max_(i=1,...,NRHS){||AX(i)-F(i)||/||F(i)|| 
!     For source see auxi.f
      REAL*8 RESID2
      EXTERNAL RESID2,
     &         DLARNV,
     &         DCOPY,
     &         DGEBLTTRF,
     &         DGEBLTTRS


      DATA ISEED /1,4,23,77/
! ..
! .. Executable Statements ..
! ..
! Initializing arrays randomly
      CALL DLARNV(2, ISEED, N*NB*NB, D)
      CALL DLARNV(2, ISEED, (N-1)*NB*NB, DL)
      CALL DLARNV(2, ISEED, (N-1)*NB*NB, DU1)
      CALL DLARNV(2, ISEED, N*NB*NRHS, F)
!
! Copying arrays for testing purposes
      CALL DCOPY(N*NB*NB, D, 1, DCPY, 1)
      CALL DCOPY((N-1)*NB*NB, DL, 1, DLCPY, 1)
      CALL DCOPY((N-1)*NB*NB, DU1, 1, DU1CPY, 1)
      CALL DCOPY(N*NB*NRHS, F, 1, FCPY, 1)
      
      PRINT *,"Testing accuracy of solution of linear equations system"
      PRINT *,"with randomly generated block tridiagonal coefficient" 
      PRINT *,"matrix by calculating ratios of residuals"
      PRINT *,"to RHS vectors norms."
!      
! LU factorization of the coefficient matrix      
      CALL DGEBLTTRF(N, NB, D, DL, DU1, DU2, IPIV, INFO)
      IF (INFO .NE. 0) THEN 
          PRINT *,"DGEBLTTRF returned nonzero INFO=",INFO
          STOP 1
      END IF
!
! Solving the system of equations using factorized coefficient matrix
      CALL DGEBLTTRS(N, NB, NRHS, D, DL, DU1, DU2, IPIV, F, LDF, INFO)
      IF (INFO .EQ. 0) THEN
! 
! computing the residual      
          EPS = RESID2(N, NB, NRHS, DLCPY, DCPY, DU1CPY, F, LDF, 
     &            FCPY, LDF)
          PRINT *,"max_(i=1,...,NRHS){||AX(i)-F(i)||/||F(i)||}=",EPS
      ELSE
          PRINT *,-INFO,"-th parameter in call of DGEBLTTRS has 
     &            illegal value"
          STOP 1
      END IF
      STOP 
      END

