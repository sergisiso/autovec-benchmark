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
!**********************************************************************
!  Content:
!      Auxiliary subroutines for: 
!      - Testing accuracy of Cholesky factorization by computing ratio 
!        ||A-L*L^t||_F/||A||_F of Frobenius norms of the residual to the  
! 	   Frobenius norm of the initial matrix and comparing it to 5*EPS. 
!      - Calculating max_(i=1,...,NRHS){||AX(i)-F(i)||/||F(i)||} of  
!        ratios of residuals to norms of RHS vectors for a system of 
!        linear equations with tridiagonal coefficient matrix and 
!        multiple RHS      
!
!**********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      SUBROUTINE TEST_RES(N, NB, D, LDD, B, LDB, D1, LDD1, B1, LDB1,
!     &                  D2, LDD2, B2, LDB2, INFO)
!
! ..Scalar arguments..
!      INTEGER N, NB, LDD, LDB, LDD1, LDB1, LDD2, LDB2, INFO
! ..       
! ..Array arguments..
!      REAL*8 D(LDD,*), B(LDB,*), D1(LDD1,*), B1(LDB1,*)
!      REAL*8 D2(LDD2,*), B2(LDB2,*)
! ..
! Purpose:
! ========  
! Given L*L^t factorization of block tridiagonal matrix A TEST_RES  
! computes ratio ||A-L*L^t||_F/||A||_F of Frobenius norm of the residual  
! to the Frobenius norm of the initial matrix. The test is considered as
! passed if the ratio does not exceed 5*EPS. The result is returned via 
! value of INFO.
!
! Arguments:
! ==========  
! N (input) INTEGER
!     The number of block rows of the matrix A.  N > 0.
!
! NB (input) INTEGER
!     The size of blocks.  NB > 0.
!      
! D (input) REAL*8 array, dimension (LDD,N*NB)
!     The array stores N diagonal blocks (each of size NB by NB) 
!         of  the triangular factor L if factorized matrix. The blocks
!         are stored sequentially block by block.
!     Caution: upper triangles of diagonal blocks are not zeroed! 
!     =======
!        
! LDD (input) INTEGER.
!     The leading dimension of the array D, LDD >= NB
!
! B (input) REAL*8 array, dimension (LDB,(N-1)*NB)
!     The array stores sub-diagonal blocks of triangular factor L.
!     The blocks are stored sequentially block by block.
!
! LDB (input) INTEGER.
!     The leading dimension of the array B, LDB >= NB
!
! D1 (work array) REAL*8 array, dimension (LDD1,N*NB)
!     The array is destined for internal computations.
!        
! LDD1 (input) INTEGER.
!     The leading dimension of the array D1, LDD1 >= NB
!
! B1 (work array) REAL*8 array, dimension (LDB1,(N-1)*NB)
!     The array is destined for internal computations.
!
! LDB1 (input) INTEGER.
!     The leading dimension of the array B1, LDB1 >= NB
!
! D2 (input) REAL*8 array, dimension (LDD2,N*NB)
!     The array stores N diagonal blocks (each of size NB by NB) 
!         of  the initial symmetric positive definite matrix A.
!         The blocks are stored sequentially block by block. The 
!         array is used for comparison.      
!        
! LDD2 (input) INTEGER.
!     The leading dimension of the array D2, LDD2 >= NB
!
! B2 (input) REAL*8 array, dimension (LDB2,(N-1)*NB)
!     The array stores sub-diagonal blocks of the initial symmetric
!         positive definite matrix A. The blocks are stored 
!         sequentially block by block. The array is used for comparison.   
!
! LDB2 (input) INTEGER.
!     The leading dimension of the array B2, LDB2 >= NB
!
! INFO (output) INTEGER
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
!     = 1:        the ratio ||A-L*L^t||_F/||A||_F exceeds 5*EPS
! =====================================================================
      SUBROUTINE TEST_RES(N, NB, D, LDD, B, LDB, D1, LDD1, B1, LDB1,
     &                  D2, LDD2, B2, LDB2, INFO)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER N, NB, LDD, LDB, LDD1, LDB1, LDD2, LDB2, INFO
! ..Array arguments..
      REAL*8 D(LDD,*), B(LDB,*), D1(LDD1,*), B1(LDB1,*)
      REAL*8 D2(LDD2,*), B2(LDB2,*)
! =====================================================================
! .. Local Scalars ..
      INTEGER I, J, K
      REAL*8 S, S1, S2, EPS
! .. Local Arrays ..
      REAL*8 WORK(1)
! .. External Functions ..
      REAL*8 DLANGE, DLAMCH
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO = 0
      IF(N .LE. 0) THEN
          INFO = -1
      ELSE IF (NB .LE. 0) THEN
          INFO = -2
      ELSE IF (LDD .LT. NB) THEN
          INFO = -4
      ELSE IF (LDB .LT. NB) THEN
          INFO = -6
      ELSE IF (LDD1 .LT. NB) THEN
          INFO = -8
      ELSE IF (LDB1 .LT. NB) THEN
          INFO = -10
      ELSE IF (LDD2 .LT. NB) THEN
          INFO = -12
      ELSE IF (LDB2 .LT. NB) THEN
          INFO = -14
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
!
! Define EPS as a relative precision
      EPS = DLAMCH('E')
!
! Compute S2 = ||A||_F
      S  = DLANGE( 'F', NB, NB*N,     D2, LDD2, WORK )
      S1 = DLANGE( 'F', NB, NB*(N-1), B2, LDB2, WORK )
      S2 = DSQRT(S*S+2D0*S1*S1)
!
! Copy D -> D1, B -> B1 and nullify the upper triangle of blocks in D1
      DO K = 0, N - 1
          DO J = 1, NB
              CALL DCOPY(NB-J+1, D(J,K*NB+J), 1, D1(J,K*NB+J), LDD1)
              DO I = 1, J - 1
                  D1(J,K*NB+I) = 0D0
              END DO
          END DO
      END DO
      
      DO K = 1, N - 1
          DO J = 1, NB
              CALL DCOPY(NB, B(1,(K-1)*NB+J), 1, B1(1,(K-1)*NB+J), 1)
          END DO
      END DO
! Compute product of lower block bidiagonal matrix by its transpose      
! | L_1                        | | L_1^t B_1^t                      |
! | B_1 L_2                    | |       L_2^t B_2^t                |
! |    .    .                  |*|           .       .              |  
! |        .    .              | |               .        .         |  
! |          B_N-2 L_N-1       | |                  L_N-1^t  B_N-1^t|
! |                B_N-1 L_N | |                             L_N^t  |
! 
! Result matrix has the following structure
!   D_1  B_1^t
!   B_1  D_2   B_2^t
!        B_2   D_3   B_3^t
!           .     .      .
!               .     .      .
!                 B_N-2  D_N-1   B_N-1^t
!                        B_N-1    D_N   
!
! D_1 := L_1*L_1^t      
      CALL DTRMM('L', 'L', 'N', 'N', NB, NB, 1D0, D, LDD, D1, LDD1)
      DO K = 1, N - 1
! B_k := B_k*L_k^t
          CALL DTRMM('R', 'L', 'T', 'N', NB, NB, 1D0, 
     &                D(1,(K-1)*NB+1), LDD, B1(1,(K-1)*NB+1), LDB1)
! D_k := L_k*L_k^t     
          CALL DTRMM('L', 'L', 'N', 'N', NB, NB, 1D0, 
     &                D(1,K*NB+1), LDD, D1(1,K*NB+1), LDD1)
! D_k := D_k + B_k*B_k^t     
          CALL DGEMM('N', 'T', NB, NB, NB, 1D0, B(1,(K-1)*NB+1), LDB, 
     &                B(1,(K-1)*NB+1), LDB, 1D0, D1(1,K*NB+1), LDD1)
      END DO

! Compute the difference between the calculated product L*L^t and initial
! matrix that was factored
      DO J = 1, NB*N
          DO I = 1, NB
              D1(I,J) = D1(I,J) - D2(I,J)
          END DO
      END DO
      DO J = 1, NB*(N-1)
          DO I = 1, NB
              B1(I,J) = B1(I,J) - B2(I,J)
          END DO
      END DO
      
      S = DLANGE( 'F', NB, NB*N, D1, LDD1, WORK )
      S1 = DLANGE( 'F', NB, NB*(N-1), B1, LDB1, WORK )
      S = DSQRT(S*S+2D0*S1*S1)/S2
      IF (S/EPS .GT. 5D0) THEN
          INFO = 1
      END IF
      RETURN
      END
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      SUBROUTINE TEST_RES1(N, NRHS, NB, D, LDD, B, LDB, F, LDF, X, LDX,
!     &           INFO)
!
! ..Scalar arguments..
!      INTEGER N, NRHS, NB, LDD, LDB, LDF, LDX, INFO
! ..       
! ..Array arguments..
!      REAL*8 D(LDD,*), B(LDB,*), F(LDF,*), X(LDX,*)
! ..
! Purpose:
! ========  
! Given approximate solution X of system of linear equations A*X=F 
! with symmetric positive definite block tridiagonal coefficient matrix
! A =
!   D_1  B_1^t
!   B_1  D_2   B_2^t
!        B_2  D_3   B_3^t
!           .     .      .
!               .     .      .
!                 B_N-2  D_N-1   B_N-1^t
!                        B_N-1    D_N   
! the routine computes max_(i=1,...,NRHS){||AX(i)-F(i)||/F(i)} of ratios 
! of residuals to norms of RHS vectors. he test is considered as passed 
! if the value does not exceed 10*EPS  where EPS is the machine 
! precision.    
!
! Arguments:
! ==========  
! N (input) INTEGER
!     The number of block rows of the matrix A.  N > 0.
!
! NRHS (input) INTEGER
!     The number of right hand sides (number of columns in matrix F.
!
! NB (input) INTEGER
!     The block size of blocks D_j, B_j
!
! D (input) REAL*8 array, dimension (LDD,N*NB)
!     The array stores N diagonal blocks (each of size NB by NB) 
!         of  matrix A. The blocks are stored sequentially block by 
!         block.
!     Caution: The diagonal blocks are symmetric matrices  - this 
!     =======
!         feature is assumed.
!        
! LDD (input) INTEGER.
!     The leading dimension of the array D, LDD >= NB
!
! B (input) REAL*8 array, dimension (LDB,(N-1)*NB)
!     The array stores sub-diagonal blocks of matrix A.
!     The blocks are stored sequentially block by block.
!
! LDB (input) INTEGER.
!     The leading dimension of the array B, LDB >= NB
!
! F (input) REAL*8 array, dimension (LDF,NRHS)
!     The right hand sides of the system of linear equations.
!      
! LDF (input) INTEGER.
!     The leading dimension of the array F, LDF >= NB*N
!
! X (input) REAL*8 array, dimension (LDX,NRHS)
!     The solutions of the system of linear equations.
!      
! LDX (input) INTEGER.
!     The leading dimension of the array X, LDX >= NB*N
!
! INFO (output) INTEGER
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
!     = 1:        max_(i=1,...,NRHS){||AX(i)-F(i)||/F(i)} exceeds 10*EPS
!     = 10:       note enough memory for internal array
      
      SUBROUTINE TEST_RES1(N, NRHS, NB, D, LDD, B, LDB, F, LDF, X, LDX,
     &           INFO)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER N, NRHS, NB, LDD, LDB, LDF, LDX, INFO
! ..Array arguments..
      REAL*8 D(LDD,*), B(LDB,*), F(LDF,*), X(LDX,*)
! =====================================================================
! .. Local Scalars ..
      INTEGER I, J, K, MEMSTAT
      REAL*8 RES, EPS, S
! .. Local Arrays ..
      REAL*8, ALLOCATABLE :: NORMS(:) 
! .. External Functions ..
      REAL*8 DNRM2, DLAMCH
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO=0
      IF(N .LE. 0) THEN
          INFO = -1
      ELSE IF (NRHS .LE. 0) THEN
          INFO = -2
      ELSE IF (NB .LE. 0) THEN
          INFO = -3
      ELSE IF (LDD .LT. NB) THEN
          INFO = -5
      ELSE IF (LDB .LT. NB) THEN
          INFO = -7
      ELSE IF (LDF .LT. NB*N) THEN
          INFO = -9
      ELSE IF (LDX .LT. NB*N) THEN
          INFO = -11
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
!
! Define EPS as machine relative precision
      EPS = DLAMCH('E')
      ALLOCATE(NORMS(NRHS),STAT=MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *, "TESR_RES1: Could not allocate memory for norms 
     &            of size=",NRHS
         INFO = 10
         GOTO 10
      END IF

! Compute norms of RHS vectors
      DO I = 1, NRHS
         NORMS(I) = DNRM2(NB*N, F(1,I), 1)
      END DO
!      
! Out-of-loop compute F(1):=F(1)-D(1)*X(1)-B(1)^t*X(2)      
      CALL DGEMM('N','N',NB, NRHS, NB,-1D0, D,LDD, X, LDX, 1D0, F, LDF)
      CALL DGEMM('T','N',NB, NRHS, NB,-1D0, B,LDB, X(NB+1,1), LDX, 
     &            1D0, F, LDF)
      DO K=2, N-1
!
! Compute F(K):=F(K)-B(K-1)*X(K-1)-D(K)*X(K)-B(K)^t*X(K+1)      
          CALL DGEMM('N','N',NB, NRHS, NB,-1D0, B(1,(K-2)*NB+1),LDB, 
     &        X((K-2)*NB+1,1), LDX, 1D0, F((K-1)*NB+1,1), LDF)
          CALL DGEMM('N','N',NB, NRHS, NB,-1D0, D(1,(K-1)*NB+1),LDD, 
     &        X((K-1)*NB+1,1), LDX, 1D0, F((K-1)*NB+1,1), LDF)
          CALL DGEMM('T','N',NB, NRHS, NB,-1D0, B(1,(K-1)*NB+1),LDB, 
     &        X(K*NB+1,1), LDX, 1D0, F((K-1)*NB+1,1), LDF)
      END DO
!
! Out-of-loop compute F(N):=F(N)-B(N-1)*X(N-1)-D(N)*X(N)      
      CALL DGEMM('N','N',NB, NRHS, NB, -1D0, B(1,(N-2)*NB+1), LDB, 
     &        X((N-2)*NB+1,1), LDX, 1D0, F((N-1)*NB+1,1), LDF)
      CALL DGEMM('N','N',NB, NRHS, NB, -1D0, D(1,(N-1)*NB+1), LDD, 
     &        X((N-1)*NB+1,1), LDX, 1D0, F((N-1)*NB+1,1), LDF)
      
! Compute norms of residual vectors divided by norms of RHS vectors
      RES = 0D0
      DO I = 1, NRHS
         S  = DNRM2(N*NB, F(1,I), 1)
         RES = MAX(RES,S/NORMS(I))
      END DO
      IF (RES/EPS .GT. 1D1) THEN
          INFO = 1
      END IF
      
      DEALLOCATE(NORMS)
   10 RETURN
      END
