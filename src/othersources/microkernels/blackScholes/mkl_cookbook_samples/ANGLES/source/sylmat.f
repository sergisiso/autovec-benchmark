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
!      Subroutine SYLMAT for forming a dense coefficient matrix of a 
!      system of linear equations that corresponds to Sylvester 
!      equation alpha*A*X + beta*X*B = F
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      SUBROUTINE SYLMAT(M, A, LDA, N, B, LDB, ALPHA, BETA, AA, LDAA)
! ..
! ..Scalar arguments..
!      INTEGER M, N, LDA, LDB, LDAA
!      REAL*8 ALPHA, BETA
! ..       
! ..Array arguments..
!      REAL*8 A(LDA,*), B(LDB,*), AA(LDAA,*)
! ..
! Purpose:
! ========  
! SYLMAT forms a dense coefficient N*M-by-N*M-matrix AA of a system of
! linear equations AA*x = f that corresponds to matrix Sylvester 
! equation
!     alpha*A*X + beta*X*B = F
! where A is M-by-M-matrix and B ix N-by-N-matrix. In Sylvester equation
! equation right hand side matrix F and unknown matrix X have sizes  
! M-by-N. Respectively, vectors f and x have dimension M*N.
!
! Coefficient matrix is built assuming MxN-matrices X and F are 
! represented by columns ('vectorized') as follows:
! x_11
! x_21
! .
! .
! .
! x_M1
! x_12
! x_22
! .
! .
! .
! x_M2
! .
! .
! .
! x_MN
! The M-by-M-block structure of matrix AA is 
!
!  beta*B(1,1)*I_M  beta*B(2,1)*I_M  ... beta*B(N,1)*I_M
!    + alpha*A    
!  beta*B(1,2)*I_M  beta*B(2,2)*I_M  ... beta*B(N,2)*I_M
!                     + alpha*A 
!  ......................................................
!  beta*B(1,N)*I_M  beta*B(2,N)*I_M  ... beta*B(N,N)*I_M
!                                          + alpha*A
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
! AA (output) REAL*8 array, dimension (LDAA,(M*N))
!     The array stores elements of coefficeint matrix of a system of
!     linear equations AA*x = f that corresponds to Sylvester equation
!     alpha*A*X + beta*X*B = F
!
! LDAA (input) INTEGER
!     The leading dimension of array AA. LDAA >= M*N.      
!
! INFO (output) INTEGER
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
! =====================================================================
      SUBROUTINE SYLMAT(M, A, LDA, N, B, LDB, ALPHA, BETA, AA, LDAA, 
     &            INFO)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER M, N, LDA, LDB, LDAA, INFO
      REAL*8 ALPHA, BETA
! ..Array arguments..
      REAL*8 A(LDA,*), B(LDB,*), AA(LDAA,*)
! =====================================================================
! .. Local Scalars ..
      INTEGER I, J, K
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
      ELSE IF(LDAA .LT. M*N) THEN
          INFO = -10
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
!
! Zeroing the output matrix
      DO I = 1, M*N
          DO J = 1, M*N
              AA(I,J) = 0D0
          END DO
      END DO
!
! Taking into account contribution from A
      DO I = 1, N
          DO J = 1, M
              DO K = 1, M
                  AA((I-1)*M+J,(I-1)*M +K) = ALPHA*A(J,K)
              END DO
          END DO
      END DO
!
! Taking into account contribution from B
      DO I = 1, N
          DO J = 1, N
              DO K = 1, M
                  AA((I-1)*M+K,(J-1)*M+K) =  AA((I-1)*M+K,(J-1)*M+K)
     &                                     + BETA*B(J,I)
              END DO
          END DO
      END DO
      
      RETURN
      END


