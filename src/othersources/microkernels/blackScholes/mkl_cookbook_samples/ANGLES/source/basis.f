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
!      Subroutine BASIS for building an orthonormal basis in the linear
!      subspace spanned by columns of a matrix. Along with building
!      the basis dimension of the subspace is computed.
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!     SUBROUTINE BASIS(N, K, EPS, X, LDX, K1, Y, LDY, INFO)
! ..Scalar arguments..
!     INTEGER N, K, LDX, K1, LDY, INFO
!     REAL*8 EPS
! ..       
! ..Array arguments..
!
!     REAL*8 X(LDX,*), Y(LDY,*)
! ..
! Purpose:
! ========  
! BASIS performs QR factorization with pivoting of matrix Y (a copy of 
! input matrix X) and analyzes diagonal elements of factor R. Computed
! rank K1 is set to be the the last index k of diagonal element of R 
! which satisfies inequality  |R_j,j| >= EPS *||Y||_F (j = 1, 2,...,K).
! After cal DORGQR the orthonormal basis is stored as first K1 columns 
! of the matrix Y.
!
! Arguments:
! ==========  
! N (input) INTEGER, 
!     The dimension of the linear space. N > 0.
! 
! K (input) INTEGER,
!     The number of columns in matrix X. K > 0.
!
! EPS (input) REAL*8
!     The threshold used to define the dimension of the subspace. 
!
! X (input) REAL*8 array, dimension (LDX,K)
!     On entry, the array stores components of vectors that span the 
!         subspace. Does not change on exit.
!     
! LDX (input) INTEGER
!     The leading dimesion of matrix X. LDX >= N.
!
! K1 (output) INTEGER
!     The computed rank of matrix X (dimesion of the subspace spanned
!         by its columns.
!
! Y (output) REAL*8 array, dimesion (LDY,K)
!     On exit, K1 first columns of the array store components of the 
!         orthonormal basis in the subspace spanned by columns of X.
!
! LDY (input) INTEGER
!     The leading dimension of matrix Y. LDY >= N.
!
! INFO (output) INTEGER
!     = 0:            successful exit
!     = -i            (-i)-th argument of BASIS has illegal value
!     = -1000         allocating memory failed, internal error.
!     = -2000 + INFO1 where INFO1 returned by DGEQPF in a case of 
!                     nonsuccessful exit, internal error. 
!     = -3000 + INFO1 where INFO1 returned by DORGQR in a case of 
!                     nonsuccessful exit, internal error. 
!           
! =====================================================================
      SUBROUTINE BASIS(N, K, EPS, X, LDX, K1, Y, LDY, INFO)
      IMPLICIT NONE
! ..      
! ..Scalar arguments..
      INTEGER N, K, LDX, K1, LDY, INFO
      REAL*8 EPS
! ..
! ..Array arguments..
      REAL*8 X(LDX,K), Y(LDY,K)
! =====================================================================
! .. Local Scalars ..
      INTEGER I, J, MEMSTAT, LWORK
      REAL*8 F, THRESH
!      
! .. Local arrays ..
      REAL*8, ALLOCATABLE :: WORK1(:), WORK(:), TAU(:)
      INTEGER, ALLOCATABLE :: JPVT(:)
!
! .. External Functions ..
      REAL*8   DLANGE, 
     &         DLAMCH
      EXTERNAL DLANGE,
     &         DLAMCH, 
     &         DGEQPF,
     &         DORGQR
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO = 0
      IF(N .LE. 0) THEN
          INFO = -1
      ELSE IF(K .LE. 0) THEN
          INFO = -2
      ELSE IF((EPS. LT. DLAMCH('E')) .OR. (EPS. GT. 1D0)) THEN
          INFO = -3
      ELSE IF(LDX .LT. N) THEN
          INFO = -5
      ELSE IF(LDY .LT. N) THEN
          INFO = -8
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
!       
! Copy X -> Y
      DO J=1,K
          CALL DCOPY(N, X(1,J), 1, Y(1,J), 1)
      END DO
!
! Compute the treshold as a product EPS*||X||_F
      F = DLANGE("F", N, K, X, LDX, WORK)
      THRESH = EPS*F
!
!   Allocating JPVT, TAU, WORK for DGEQPF
      ALLOCATE(JPVT(K), STAT=MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
          INFO = -1000
          GOTO 1
      END IF
!
      ALLOCATE(TAU(N), STAT=MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
          INFO = -1000
          GOTO 2
      END IF
!
      ALLOCATE(WORK(3*N), STAT=MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
          INFO = -1000
          GOTO 3
      END IF
!      
! Apply QR factorization with pivoting 
      CALL DGEQPF(N, K, Y, LDY, JPVT, TAU, WORK, INFO)
      IF(INFO .NE. 0) THEN
          INFO = -2000 + INFO
          GOTO 4
      END IF
! Compute the rank
      K1=0
      DO WHILE((K1 .LT. K) .AND. (ABS(Y(K1+1,K1+1)) .GT. THRESH))
          K1 = K1 + 1
      END DO
!      
! Form K1 orthonormal vectors via call DORGQR
      LWORK = -1
      CALL DORGQR(N, K1, K1, Y, LDY, TAU, WORK, LWORK, INFO)
      IF(INFO .NE. 0) THEN
          INFO = -3000 + INFO
          GOTO 4
      END IF
      LWORK = WORK(1)
      IF(LWORK .GT. 3*N) THEN
          ALLOCATE(WORK1(LWORK), STAT = MEMSTAT)
          IF(MEMSTAT .NE. 0) THEN
              INFO = -1000
              GOTO 4
          END IF
          CALL DORGQR(N, K1, K1, Y, LDY, TAU, WORK1, LWORK, INFO)
          IF(INFO .NE. 0) THEN
              INFO = -3000 + INFO
          END IF
          DEALLOCATE(WORK1)
      ELSE
          CALL DORGQR(N, K1, K1, Y, LDY, TAU, WORK, LWORK, INFO)
          IF(INFO .NE .0) THEN
              INFO = -3000 + INFO
              GOTO 4
          END IF
      END IF
!
! Free allocated memory
    4 DEALLOCATE(WORK) 
    3 DEALLOCATE(TAU)
    2 DEALLOCATE(JPVT)
    1 RETURN
      END
