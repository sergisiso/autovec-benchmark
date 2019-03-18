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
!      Subroutine DPBLTRS for solving a system of linear equations with 
!      Cholesky factored symmetric positive definite block tridiagonal 
!      coefficient matrix.
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      SUBROUTINE DPBLTRS(N, NRHS, NB, D, LDD, B, LDB, F, LDF)
!
! ..Scalar arguments..
!      INTEGER N, NRHS, NB, LDD, LDB, LDF, INFO
! ..       
! ..Array arguments..
!      REAL*8 D(LDD,*), B(LDB,*), F(LDF,*)
! ..
! Purpose:
! ========  
! DPBLTRS computes a solution to system of linear equations A*X=F with 
! symmetric positive definite block tridiagonal coefficient matrix A 
!   D_1  B_1^t
!   B_1  D_2   B_2^t
!        B_2  D_3   B_3^t
!           .     .      .
!               .     .      .
!                 B_N-2  D_N-1   B_N-1^t
!                        B_N-1    D_N   
! and multiple right hand sides F. Before call this routine the 
! coefficient matrix should factored A=L*L^T by calling DPBLTRF where 
! L is a lower block bidiagonal matrix
!   L_1  
!   C_1  L_2   
!        C_2   L_3 
!           .     .      .
!               .     .      .
!                 C_N-2  L_N-1
!                        C_N-1    L_N   
! This is a block version of LAPACK DPTTRS subroutine.
!
! Arguments:
! ==========  
! N (input) INTEGER
!     The number of block rows of the matrix A.  N >= 0.
!
! NRHS (input) INTEGER
!     The number of right hand sides (the number of columns in matrix F).
!
! NB (input) INTEGER
!     The size of blocks.  NB >= 0.
!
! D (input) REAL*8 array, dimension (LDD,N*NB)
!     On entry, the array stores diagonal blocks of triangular factor L. 
!         Diagonal blocks L_j of lower triangular factor L are stored as
!         respective lower triangles of blocks D_j (1 <= j <= N).
!         Caution: upper triangles of D_j are not assumed to be zeroed.
!         =======      
!
! LDD (input) INTEGER
!     The leading dimension of array D. LDD >= NB.
!
! B (input) REAL*8 array, dimension (LDB,(N-1)*NB)
!     On entry, the array stores sub-diagonal blocks L_j of triangular 
!          factor L.
!
! LDB (input) INTEGER
!     The leading dimension of array B. LDB >= NB.
!
! F   (input/output) REAL*8 array, dimension (LDF,NRHS)
!     On entry, the columns of the array store vectors F(i) of right  
!         hand sides of system of linear equations A*X=F.
!
! LDF (input) INTEGER
!     The leading dimension of array F. LDF >= NB*N.
!
! INFO (output) INTEGER
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
! =====================================================================
     
      SUBROUTINE DPBLTRS(N, NRHS, NB, D, LDD, B, LDB, F, LDF, INFO)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER N, NRHS, NB, LDD, LDB, LDF, INFO
! ..Array arguments..
      REAL*8 D(LDD,*), B(LDB,*), F(LDF,*)
! =====================================================================
! .. Local Scalars ..
      INTEGER I, J, K
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO = 0
      IF(N .LT. 0) THEN
          INFO = -1
      ELSE IF(NRHS .LT. 0) THEN
          INFO = -2
      ELSE IF(NB .LT. 0) THEN
          INFO = -3
      ELSE IF(LDD .LT. NB) THEN
          INFO = -5
      ELSE IF(LDB .LT. NB) THEN
          INFO = -7
      ELSE IF(LDF .LT. NB*N) THEN
          INFO = -9
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
! ..
! Solving the system of linear equations L*Y=F      
      CALL DTRSM('L', 'L', 'N', 'N', NB, NRHS, 1D0, D, LDD, F, LDF)
      DO K = 2, N
          CALL DGEMM('N', 'N', NB, NRHS, NB, -1D0, B(1,(K-2)*NB+1), 
     &         LDB, F((K-2)*NB+1,1), LDF, 1D0, F((K-1)*NB+1,1), LDF)
          CALL DTRSM('L','L', 'N', 'N', NB, NRHS, 1D0, D(1,(K-1)*NB+1), 
     &         LDD, F((K-1)*NB+1,1), LDF)
      END DO
! ..
! Solving the system of linear equations L^T*X=Y      
      CALL DTRSM('L', 'L', 'T', 'N', NB, NRHS, 1D0, D(1,(N-1)*NB+1), 
     &         LDD, F((N-1)*NB+1,1), LDF)
      DO K = N-1, 1, -1
          CALL DGEMM('T', 'N', NB, NRHS, NB, -1D0, B(1,(K-1)*NB+1), 
     &         LDB, F(K*NB+1,1), LDF, 1D0, F((K-1)*NB+1,1), LDF)
          CALL DTRSM('L','L', 'T', 'N', NB, NRHS, 1D0, D(1,(K-1)*NB+1), 
     &         LDD, F((K-1)*NB+1,1), LDF)
      END DO

      RETURN 
      END