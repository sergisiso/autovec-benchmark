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
!      Example of solving a system of linear equations with symmetric 
!      positive definite block tridiagonal coefficient matrix Cholesky 
!      factored
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Purpose:
! ========  
! Testing accuracy of solution of a system of linear equations A*X=F 
! with a symmetric positive definite block tridiagonal coefficient 
! matrix A
!  | D_1  B_1^t                           |
!  | B_1  D_2   B_2^t                     |
!  |      B_2  D_3   B_3^t                | 
!  |         .     .      .               |
!  |             .     .      .           |
!  |               B_N-2  D_N-1   B_N-1^t |
!  |                      B_N-1   D_N     | 
! preliminarily Cholesky factored as follows:
!      | L_1                  | | L_1^t  C_1^t                   |
!      | C_1  L_2             | |        L_2^t  C_2^t            |
!  A = |    .     .           |*|             .       .          |  
!      |        .     .       | |                  .     C_N-1^t |  
!      |           C_N-1  L_N | |                        L_N^t   |
!
! To test the solution subroutine TES_RES1 is called.
!      
      PROGRAM test
      IMPLICIT NONE
! ..Scalar parameters..
! N   INTEGER,    N > 0.
!     The number of matrix equations to solve (block rows in the 
!     coefficient matrix).  
!
! NB  INTEGER,     NB > 0.
!     The size of blocks. 
      INTEGER N, NB
!      
! NRHS INTEGER,     NRHS > 0.
!     The number of right hand sides 
!
! LDF INTEGER,    LDF>=NB*N
!     The leading dimension of the matrix of right hand sides
!      
      INTEGER NRHS, LDF
      PARAMETER (N=4203, NB=32, NRHS=41, LDF=NB*N)
! INFO  INTEGER
!     On exit from DPBLTRF:
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
!     > 0:        if INFO = i, the leading minor of order i (and 
!                 therefore the matrix A itself) is not 
!                 positive-definite, and the factorization could not be
!                 completed. This may indicate an error in forming the 
!                 matrix A.
!     On exit from TEST_RES1:
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
!     = 1:        max_i||A*X(i)-F(i)||>10*||F(i)||*EPS
!     = 10:       not enough memory for internal array
      INTEGER INFO
! I,J,K   INTEGER
!     Local loop indices
      INTEGER I, J, K
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
!     On exit from DPBLTRF (entry to DPBLTRS), the array stores 
!         diagonal blocks of triangular factor L. Diagonal blocks of 
!         lower triangular factor L replace respective lower triangles
!         of blocks D_j (1 <= j <= N).      
!     Caution: upper triangles of diagonal blocks are not zeroed on 
!     =======
!         exit!!!
      REAL*8 D(NB,NB*N)
!
! B   REAL*8 array, dimension (NB,(N-1)*NB)
!     On entry to DPBLTRF, the array is randomly initialized and stores
!         sub-diagonal  blocks (each of size NB by NB) of the matrix to
!         be factored. The blocks  are stored sequentially: first NB 
!         columns of B store block B_1, second  NB columns store block 
!         B_2,...,last NB columns store block B_N-1. 
!     On exit from DPBLTRF (entry to DPBLTRS), the array stores sub-
!         diagonal blocks of triangular factor L.  
      REAL*8 B(NB,NB*(N-1))
!      
! D2  REAL*8 array, dimension (NB,N*NB)
!     Copy of the array D for testing purposes
! B2  REAL*8 array, dimension (NB,(N-1)*NB)
!     Copy of the array B for testing purposes
      REAL*8 D2(NB,NB*N), B2(NB,NB*(N-1))
!
! F   REAL*8 array, dimension (LDF,NRHS)
!     On entry to DPBLTRS, the array stores right hand side vectors F(i) 
!         of the system of linear equations.
!     On exit from DPBLTRS, the array stores unknown vectors X(i)
!         of the system of linear equations. 
! F2  REAL*8 array, dimension (LDF,NRHS)
!     Copy of the array F for testing purposes (used in TEST_RES1).
      REAL*8 F(LDF,NRHS), F2(LDF,NRHS)
!      
! ISEED INTEGER array, dimension 4
!     The array stores 'seed' numbers for generator DLARNV.
!          To be initialized before the first call DLARNV.    
      INTEGER ISEED(4)
      DATA ISEED /1, 2, 3, 19/
! ..
! .. Executable Statements ..
      PRINT *,"Testing accuracy of solution of linear equations system"
      PRINT *,"with randomly generated positive definite symmetric " 
      PRINT *,"block tridiagonal coefficient matrix by calculating "
      PRINT *,"ratios of residuals to RHS vectors' norms."
      PRINT *,"..."
      PRINT *,"Matrices are being generated."
      PRINT *,"..."
      
! ..
! Initializing arrays randomly
      CALL DLARNV(2, ISEED, (N-1)*NB*NB, B)
      CALL DCOPY((N-1)*NB*NB, B, 1, B2, 1) 
      CALL DLARNV(2, ISEED, NRHS*LDF, F)
      CALL DCOPY(NRHS*LDF, F, 1, F2, 1) 

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
      CALL DCOPY(NB*NB*N, D, 1, D2, 1)
      
! Factor the coefficient matrix
      PRINT *, "Call Cholesky factorization"
      PRINT *,"..."
      CALL DPBLTRF(N, NB, D, NB, B, NB, INFO)
      IF(INFO .NE. 0)THEN
          PRINT *,"Cholesky factorization failed. INFO=", INFO
          STOP 1
      ELSE
          PRINT *,"Cholesky factorization succeeded."
      END IF
! Solve the system of equations with factored coefficient matrix
      PRINT *, "Call solving the system of linear equations"
      PRINT *,"..."
      CALL DPBLTRS(N, NRHS, NB, D, NB, B, NB, F, LDF, INFO)
      IF(INFO .NE. 0)THEN
          PRINT *,"Solution failed. INFO=", INFO
          STOP 1
      ELSE
          PRINT *,"Solution succeeded."
      END IF
! Test the accuracy of the solution
      PRINT *, "The system is solved. Testing the residual"
      PRINT *,"..."
      CALL TEST_RES1(N, NRHS, NB, D2, NB, B2, NB, F2, LDF, 
     &     F, LDF, INFO)
      IF(INFO .EQ. 0) THEN
              PRINT *,"Residual test" 
              PRINT *, "max_(i=1,...,NRHS){||A*X(i)-F(i)||/||F(i)||}  
     &         <= 10*EPS "
              PRINT *,"passed"
      ELSE
              PRINT *,"Residual test"
              PRINT *, "max_(i=1,...,NRHS){||A*X(i)-F(i)||/||F(i)||}  
     &         <= 10*EPS "
              PRINT *,"failed"
              STOP 1              
      END IF
      
      STOP
      END
      
      
