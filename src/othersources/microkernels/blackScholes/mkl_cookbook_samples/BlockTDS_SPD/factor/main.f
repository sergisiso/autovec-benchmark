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
!      Example of Cholesky factorization of a symmetric positive 
!      definite block tridiagonal matrix 
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Purpose:
! ========  
! Testing accuracy of Cholesky factorization A=
!      | L_1                  | | L_1^t  C_1^t                   |
!      | C_1  L_2             | |        L_2^t  C_2^t            |
!  A = |    .     .           |*|             .       .          |  
!      |        .     .       | |                  .     C_N-1^t |  
!      |           C_N-1  L_N | |                        L_N^t   |
!
! of a symmetric positive definite block tridiagonal matrix A
!  | D_1  B_1^t                           |
!  | B_1  D_2   B_2^t                     |
!  |      B_2  D_3   B_3^t                | 
!  |         .     .      .               |
!  |             .     .      .           |
!  |               B_N-2  D_N-1   B_N-1^t |
!  |                      B_N-1   D_N     | 
! by calling TEST_RES which calculates ratio of Frobenius norms 
!      ||A-L*L^t||_F/||A||_F. 
      
      PROGRAM test
      IMPLICIT NONE
! ..Scalar parameters..
! N   INTEGER,    N > 0.
!     The number of matrix equations to solve (block rows in the 
!     coefficient matrix).  
!
! NB  INTEGER,     NB > 0.
!     The size of blocks. 
      INTEGER N,NB
      PARAMETER (N=92500,NB=19)
!      
! INFO  INTEGER
!     On exit from DPBLTRF:
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
!     > 0:        if INFO = i, the leading minor of order i (and 
!                 therefore the matrix A itself) is not 
!                 positive-definite, and the factorization could not be
!                 completed. This may indicate an error in forming the 
!                 matrix A.
      INTEGER INFO
!
! J, K   INTEGER
!     Local loop indices
      INTEGER J,K
!
! ..Arrays..
! D       REAL*8 array, dimension (NB,N*NB)
!     On entry to DPBLTRF, the array is randomly initialized with 
!         diagonal dominance to provide positive definiteness and
!         stores N diagonal blocks (each of size NB by NB) of the 
!         matrix to be factored. The blocks are stored sequentially: 
!         first NB columns of D store block D_1,  second NB columns 
!         store block D_2,...,last NB columns store block D_N.      
!     Note: As the diagonal blocks are symmetric only lower or upper 
!     ====
!         triangle is needed to store blocks' elements. In this code 
!         lower storage is used!!! 
!     On exit from DPBLTRF, the array stores diagonal blocks of  
!         triangular factor L. Diagonal blocks of lower triangular 
!         factor L replace respective lower triangles of blocks D_j 
!         (1 <= j <= N).      
!     Caution: upper triangles of diagonal blocks are not zeroed on 
!     =======
!         exit!!!
!
! B   REAL*8 array, dimension (NB,(N-1)*NB)
!     On entry to DPBLTRF, the array is randomly initialized and stores
!         sub-diagonal  blocks (each of size NB by NB) of the matrix to
!         be factored. The blocks  are stored sequentially: first NB 
!         columns of B store block B_1, second  NB columns store block 
!         B_2,...,last NB columns store block B_N-1. 
!     On exit from DPBLTRF, the array stores sub-diagonal blocks of 
!         triangular factor L.  
      REAL*8  D(NB,NB*N),  B(NB,NB*(N-1))
!
! D1  REAL*8 work array, dimension (NB,N*NB)
!     The array is for internal computations in TEST_RES.
! B1  REAL*8 work array, dimension (NB,(N-1)*NB)
!     The array is for internal computations in TEST_RES.
      REAL*8 D1(NB,NB*N), B1(NB,NB*(N-1))
!      
! D2  REAL*8 array, dimension (NB,N*NB)
!     Copy of the array D for testing purposes
! B2  REAL*8 array, dimension (NB,(N-1)*NB)
!     Copy of the array B for testing purposes
      REAL*8 D2(NB,NB*N), B2(NB,NB*(N-1))
!
! ISEED INTEGER array, dimension 4
!     The array stores 'seed' numbers for generator DLARNV.
!          To be initialized before the first call DLARNV.    
      INTEGER ISEED(4)
      DATA ISEED /1,2,33,15/
! ..
! .. Executable Statements ..
      PRINT *,"Testing accuracy of Cholesky factorization"
      PRINT *,"of randomly generated positive definite symmetric " 
      PRINT *,"block tridiagonal matrix by calculating residual."
      PRINT *,"Matrix size=",N
      PRINT *,"Block  size=",NB 
      PRINT *,"..."
      PRINT *,"Matrices are being generated."
      PRINT *,"..."
! ..
! Initializing arrays randomly
      CALL DLARNV(2, ISEED, (N-1)*NB*NB, B)
      CALL DCOPY((N-1)*NB*NB, B, 1, B2, 1) 
      DO K = 1, N
          DO J = 1, NB
              CALL DLARNV(2, ISEED, NB-J+1, D(J,(K-1)*NB+J))
              CALL DCOPY(NB-J, D(J+1,(K-1)*NB+J), 1, 
     &                         D(J,(K-1)*NB+J+1), NB) 
          END DO
!
! Diagonal dominance to make the matrix positive definite           
          DO J = 1, NB
              D(J,(K-1)*NB+J)  = D(J,(K-1)*NB+J) + NB*3D0
          END DO
      END DO
      CALL DCOPY(N*NB*NB, D, 1, D2, 1) 
      
      PRINT *, "Call Cholesky factorization"
      PRINT *,"..."
      CALL DPBLTRF(N, NB, D, NB, B, NB, INFO)
      IF(INFO .NE. 0)THEN
          PRINT *,"Factorization failed. INFO=",INFO
          STOP 1
      ELSE
          PRINT *,"Cholesky factorization succeeded."
          PRINT *,"Testing the residual"
          PRINT *,"..."
          CALL TEST_RES(N, NB, D, NB, B, NB, D1, NB, B1, NB, D2, NB,
     &                    B2, NB, INFO)
          PRINT *,"Residual test" 
          PRINT *, "||A-L*L^t||_F/||A||_F <= 5*EPS"
          IF (INFO .NE. 0) THEN
              PRINT *,"failed"
              STOP 1              
          ELSE 
              PRINT *,"passed"
          END IF
      END IF
      
      STOP
      END
      
      
      