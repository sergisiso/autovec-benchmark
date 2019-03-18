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
!      Auxiliary subroutine for testing 'quality' of the basis U built
!      of columns of matrix X by orthogonalization of them and choosing 
!      linear independent columns.
!
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!     SUBROUTINE TEST_BASIS(N, K, K1, X, LDX, U, LDU, INFO, EPS)
!      
! ..Scalar arguments..
!     INTEGER N, K, K1, LDX, LDU, INFO
!     REAL*8 EPS       
! ..       
! ..Array arguments..
!     REAL*8 X(LDX,*), U(LDU,*)
! ..
! Purpose:
! ========  
! Given N-by-K matrix X and N-by-K1 matrix U TEST_BASIS tests 'quality'
! of basis stored in columns of U. Two acceptance criteria are checked:
! 1) Orthogonality of columns of U     
!         ||I-U^t*U||_F/(2*N*EPS1) < 1
!    Here EPS1 is relative machine precision.
! 2) Fullness of columns of U (i.e. columns of X are linear combinations
!    of columns of U (up to te threshold)
!         ||U*U^t*X - X||_F /||X||_F/(10*EPS) < 1
!    Here EPS is the threshold.
! 
! Arguments:
! ==========  
! N (input) INTEGER
!     The dimension of the linear space. N > 0.
! 
! K (input) INTEGER,
!     The number of columns in matrix X. K > 0.
! 
! K1 (input) INTEGER,
!     The number of columns in matrix U. 0 < K1 <= K.
!
! X (input) REAL*8 array, dimension (LDX,K)
!     The array stores components of vectors that span the subspace.
!         Unperturbed on exit.
!          
! LDX (input) INTEGER
!     The leading dimesion of matrix X. LDX >= N.
!
! U (input) REAL*8 array, dimension (LDU,K1)
!     The array stores components of vectors of the orthonormal basis
!          in the subspace. Unperturbed on exit.
!          
! LDU (input) INTEGER
!     The leading dimesion of matrix U. LDU >= N.
!
! INFO (output) INTEGER
!     = 0:            successful exit
!     = -i            (-i)-th argument of TEST_BASIS has illegal value
!     = -1000         allocating memory failed, internal error.
!     = 1             orthogonality test failed.
!     = 2             fullness test failed.
!
! EPS (input) REAL*8
!     Threshold used in BASIS to define the basis. The parameter is 
!         needed to test fullness.
! ======================================================================
      SUBROUTINE TEST_BASIS(N, K, K1, X, LDX, U, LDU, INFO, EPS)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER N, K, K1, LDX, LDU, INFO
      REAL*8 EPS
! ..Array arguments..
      REAL*8 X(LDX,*), U(LDU,*)
! ======================================================================
! .. Local Scalars ..
      INTEGER I, J, MEMSTAT
      REAL*8 S, S0
! ..Local arrays..
      REAL*8, ALLOCATABLE :: W(:,:), XCPY(:,:)
      REAL*8 WORK(1)
!
! .. External Functions ..
      REAL*8   DLANGE, 
     &         DLAMCH
      EXTERNAL DLANGE,
     &         DLAMCH,
     &         DCOPY,
     &         DGEMM
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO = 0
      IF(N .LE. 0) THEN
          INFO = -1
      ELSE IF(K .LE. 0) THEN
          INFO = -2
      ELSE IF((K1 .LE. 0) .OR. (K1 .GT. K)) THEN
          INFO = -3
      ELSE IF(LDX .LT. N) THEN
          INFO = -5
      ELSE IF(LDU .LT. N) THEN
          INFO = -7
      ELSE IF((EPS. LT. DLAMCH('E')) .OR. (EPS. GT. 1D0)) THEN
          INFO = -9
      END IF
      IF(INFO .NE. 0) THEN
          GOTO 1
      END IF
!
! Allocating XCPY and computing ||X||_F
      ALLOCATE(XCPY(N,K), STAT = MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         INFO = -1000
         GOTO 1
      END IF
      DO J=1,K
          CALL DCOPY(N, X(1,J), 1, XCPY(1,J), 1)              
      END DO
      S0 = DLANGE("F", N, K, X, LDX, WORK)
!      
!Allocate memory for computing ||I-U^t*U||_F 
      ALLOCATE(W(K1,K), STAT = MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         INFO = -1000
         GOTO 2
      END IF
!
! W(:,1:K1) is initialized as a unit matrix
      DO I = 1, K1
          DO J = 1, K1
              W(I,J) = 0D0
          END DO
          W(I,I) = 1D0
      END DO
      CALL DGEMM("T", "N", K1, K1, N, -1D0, U, LDU, U, LDU, 1D0, W, K1)
! Compute S = ||I - U^t*U||_F/EPS1
      S = DLANGE("F", K1, K1, W, K1, WORK)/DLAMCH('E')
!
! Coefficient 1/(2*N) below is empirical - needs some better proven 
! estimate
      IF (S/(2*N) .LT. 1D0) THEN
!
! If S is acceptable we can compute the second metric.
! Compute S= ||X - U*(U^t*X)||_F/(||X||_F*EPS)
          CALL DGEMM("T", "N", K1, K, N, 1D0, U, LDU, XCPY, N, 0D0, 
     &                W, K1)
          CALL DGEMM("N", "N", N, K, K1, -1D0, U, LDU, W, K1, 1D0, 
     &                XCPY, N)
          S = DLANGE("F", N, K, XCPY, N, WORK)/(S0*EPS)
!
! Coefficient 1/10 is empirical - needs some better proven estimate
          IF (S/10 .GT. 1D0) THEN
              INFO = 2
          END IF
      ELSE
          INFO = 1
      END IF
      
      DEALLOCATE(W)
    2 DEALLOCATE(XCPY)
    1 RETURN
      END

