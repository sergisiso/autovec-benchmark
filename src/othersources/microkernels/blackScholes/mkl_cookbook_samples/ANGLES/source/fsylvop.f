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
!      Subroutine FSYLVOP for forming in CSR storage format a sparse 
!      coefficient matrix of a system of linear equations that 
!      corresponds to Sylvester equation alpha*A*X + beta*X*B = F
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!     SUBROUTINE FSYLVOP(M, A, LDA, N, B, LDB, ALPHA, BETA, NNZ, COLS,
!    &                    ROWINDEX, VAL)
! ..
! ..Scalar arguments..
!      INTEGER M, N, LDA, LDB, NNZ
!      REAL*8 ALPHA, BETA
! ..       
! ..Array arguments..
!      REAL*8 A(LDA,*), B(LDB,*), VAL(*)
!      INTEGER COLS(*), ROWINDEX(*)
! ..
! Purpose:
! ========  
! FSYLVOP forms a sparse coefficient N*M-by-N*M-matrix AA of a system of
! linear equations AA*x = f that corresponds to matrix Sylvester 
! equation
!     alpha*A*X + beta*X*B = F
! where A is M-by-M-matrix and B ix N-by-N-matrix. In Sylvester equation
! equation right hand side matrix F and unknown matrix X have sizes  
! M-by-N. Respectively, vectors f and x have dimension M*N.
!
! Matrix of the operator is built assuming MxN-matrices X and F are 
! represented by columns ('vectorized') as follows      
!x_11
!x_21
!.
!.
!.
!x_M1
!x_12
!x_22
!.
!.
!.
!x_M2
!.
!.
!.
!x_MN
!
! The M-by-M-block structure of matrix AA is 
!
!  beta*B(1,1)*I_M  beta*B(2,1)*I_M  ... beta*B(N,1)*I_M
!    + alpha*A    
!  beta*B(1,2)*I_M  beta*B(2,2)*I_M  ... beta*B(N,2)*I_M
!                     + alpha*A 
!  ......................................................
!  beta*B(1,N)*I_M  beta*B(2,N)*I_M  ... beta*B(N,N)*I_M
!                                          + alpha*A
! It has (M+N-1) nonzero elements in each row.
!
! Arguments:
! ==========  
! M (input) INTEGER
!     The order of matrix A.  M > 0.
!
! A (input) REAL*8 array, dimension (LDA,M)
!     The array stores elements of matrix A of Sylvester equation
!
! LDA (input) INTEGER
!     The leading dimension of array A. LDA >= M.
!
! N (input) INTEGER
!     The order of matrix B.  N > 0.
!
! B (input) REAL*8 array, dimension (LDB,N)
!     The array stores elements of matrix B of Sylvester equation
!
! LDB (input) INTEGER
!     The leading dimension of array B. LDA >= N.
!
! ALPHA (input) REAL*8
!     The scalar coefficient in Sylvester equation 
!     alpha*A*X + beta*X*B = F      
!      
! BETA (input) REAL*8
!     The scalar coefficient in Sylvester equation 
!     alpha*A*X + beta*X*B = F      
!      
! COLS (output) INTEGER array, dimension (M*N*(M+N-1))
!     The column indices of nonzero elements of the coefficient matrix.
!
! ROWINDEX (output) INTEGER array, dimension (M*N+1)
!     The row indices of the CSR format
!
! VAL (output) REAL*8 array, dimension (M*N*(M+N-1))
!     Nonzero values of the coefficient matrix stored in CSR format.
! INFO (output) INTEGER
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
! =====================================================================
      SUBROUTINE FSYLVOP(M, A, LDA, N, B, LDB, ALPHA, BETA, COLS,
     &                    ROWINDEX, VAL, INFO)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER M, N, LDA, LDB, INFO
      REAL*8 ALPHA, BETA
! ..Array arguments..
      REAL*8 A(LDA,*),B(LDB,*)
      REAL*8 VAL(*)
      INTEGER COLS(*), ROWINDEX(*)
! =====================================================================
! .. Local Scalars ..
      INTEGER I, J, K, L
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO=0
      IF(M .LE. 0) THEN
          INFO = -1
      ELSE IF(LDA .LT. M) THEN
          INFO = -3
      ELSE IF(N .LE. 0) THEN
          INFO = -4
      ELSE IF(LDB .LT. N) THEN
          INFO = -6
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF

! Every row has the M+N-1 nonzero elements
      ROWINDEX(1)=1
      DO I=1,M*N
          ROWINDEX(I+1) = ROWINDEX(I) + (M+N-1)
      END DO
!
      K = 0
      DO I = 1,N
          DO J = 1,M
! Block lower triangle of the coefficient matrix
              DO L = 1,I-1
                  COLS(K*(M+N-1) + L) = J + M*(L-1)
                   VAL(K*(M+N-1) + L) = BETA*B(L,I)
              END DO
! Diagonal blocks of the coefficient matrix              
              DO L = 1,M
                  COLS(K*(M+N-1) + I + L - 1) = L + M*(I-1)
                   VAL(K*(M+N-1) + I + L - 1) = ALPHA*A(J,L)
              END DO
! Diagonal elements of the coefficient matrix to be corrected
		    COLS(K*(M+N-1) + I + J - 1) = J + M*(I-1) 
		    VAL( K*(M+N-1) + I + J - 1) = ALPHA*A(J,J) + BETA*B(I,I)
! Block upper triangle of the coefficient matrix
              DO L = I+1,N
                  COLS(K*(M+N-1) + L + M - 1) = J + M*(L-1)
                   VAL(K*(M+N-1) + L + M - 1) = BETA*B(L,I)
              END DO
              K = K+1
          END DO
      END DO
      
      RETURN
      END

