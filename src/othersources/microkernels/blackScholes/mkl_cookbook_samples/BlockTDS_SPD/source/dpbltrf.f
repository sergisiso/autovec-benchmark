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
!      Subroutine DPBLTRF for Cholesky factorization of symmetric  
!         positive definite block tridiagonal matrix.
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      SUBROUTINE DPBLTRF(N, NB, D, LDD, B, LDB, INFO)
!
! ..Scalar arguments..
!      INTEGER N, NB, LDD, LDB, INFO
! ..       
! ..Array arguments..
!      REAL*8 D(LDD,*), B(LDB,*)
! ..
! Purpose:
! ========  
! DPBLTRF computes Cholesky L*L^t-factorization of symmetric positive 
! definite block tridiagonal matrix A
!   D_1  B_1^t
!   B_1  D_2   B_2^t
!        B_2  D_3   B_3^t
!           .     .      .
!               .     .      .
!                 B_N-2  D_N-1   B_N-1^t
!                        B_N-1    D_N   
! The factorization has the form A = L*L**t, where L is a lower 
! bidiagonal block matrix 
!   L_1  
!   C_1  L_2   
!        C_2   L_3 
!           .     .      .
!               .     .      .
!                 C_N-2  L_N-1
!                        C_N-1    L_N   
! This is a block version of LAPACK DPTTRF subroutine.
!
! Arguments:
! ==========  
! N (input) INTEGER
!     The number of block rows of the matrix A.  N >= 0.
!
! NB (input) INTEGER
!     The size of blocks.  NB >= 0.
!
! D (input/output) REAL*8 array, dimension (LDD,N*NB)
!     On entry, the array stores N diagonal blocks (each of size NB by  
!         NB) of the matrix to be factored. The blocks are stored 
!         sequentially: first NB columns of D store block D_1, second NB 
!         columns store block D_2,...,last NB columns store block D_N.
!     Note: As the diagonal blocks are symmetric only lower or upper 
!     ====
!         triangle is needed to store blocks' elements. In this code 
!         lower storage is used!!!
!     On exit, the array stores diagonal blocks of triangular factor L. 
!         Diagonal blocks of lower triangular factor L replace
!         respective lower triangles of blocks D_j (1 <= j <= N). 
!     Caution: upper triangles of diagonal blocks are not zeroed on 
!     =======
!         exit!!!
!
! LDD (input) INTEGER
!     The leading dimension of array D. LDD >= NB.
!
! B (input/output) REAL*8 array, dimension (LDB,(N-1)*NB)
!     On entry, the array stores sub-diagonal  blocks (each of size NB
!         by NB) of the matrix to be factored. The blocks are stored 
!         sequentially: first NB columns of B store block B_1, second  
!         NB columns store block B_2,...,last NB columns store block 
!         B_N-1.
!     On exit, the array stores sub-diagonal blocks of triangular factor 
!         L.  
!
! LDB (input) INTEGER
!     The leading dimension of array B. LDB >= NB.
!
! INFO (output) INTEGER
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
!     > 0:        if INFO = i, the leading minor of order i (and 
!                 therefore the matrix A itself) is not 
!                 positive-definite, and the factorization could not be
!                 completed. This may indicate an error in forming the 
!                 matrix A.
! =====================================================================

      SUBROUTINE DPBLTRF(N, NB, D, LDD, B, LDB, INFO)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER N, NB, LDD, LDB, INFO
! ..Array arguments..
      REAL*8 D(LDD,*), B(LDB,*)
! =====================================================================
! .. Local Scalars ..
      INTEGER K
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO = 0
      IF(N .LT. 0) THEN
          INFO = -1
      ELSE IF(NB .LT. 0) THEN
          INFO = -2
      ELSE IF(LDD .LT. NB) THEN
          INFO = -4
      ELSE IF(LDB .LT. NB) THEN
          INFO = -6
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
! ..
! Compute Cholesky factorization of the first diagonal block
      CALL DPOTRF('L', NB, D, LDD, INFO)
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
!
! Main loop
      DO K = 1, N-1
          CALL DTRSM('R', 'L', 'T', 'N', NB, NB, 1D0, 
     &                D(1,(K-1)*NB+1), LDD, B(1,(K-1)*NB+1), LDB)
          CALL DSYRK('L', 'N', NB, NB, -1D0, 
     &               B(1,(K-1)*NB+1), LDB, 1D0, D(1,K*NB+1), LDD)
          CALL DPOTRF('L', NB, D(1,K*NB+1), LDD, INFO)
          IF(INFO .NE. 0) THEN
              INFO = INFO + K*NB
! INFO is equal to not local but global number of the row              
              RETURN
          END IF
      END DO
      RETURN
      END
      
      