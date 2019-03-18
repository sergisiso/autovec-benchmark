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
!      Subroutine DGEBLTTRF for LU factorization of general block 
!         tridiagonal matrix;
!      Subroutine PTLDGETRF for partial LU factorization of general 
!         rectangular matrix.
!  See respective recipe at: https://software.intel.com/en-us/node/507040
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      SUBROUTINE DGEBLTTRF(N, NB, D, DL, DU1, DU2, IPIV, INFO)
!
! ..Scalar arguments..
!      INTEGER N, NB, INFO
! ..       
! ..Array arguments..
!      INTEGER IPIV(*)       
!      REAL*8 D(NB,*), DL(NB,*), DU1(NB,*), DU2(NB,*)
! ..
! Purpose:
! ========  
! DGEBLTTRF computes LU factorization of general block tridiagonal 
! matrix
!          (D_1  C_1                          )
!          (B_1  D_2  C_2                     )
!          (     B_2  D_3  C_3                )
!          (           .........              )
!          (              B_N-2 D_N-1  C_N-1  )
!          (                    B_N-1  D_N    )
! using elimination with partial pivoting and row interchanges. 
! The factorization has the form A = L*U, where L is a product of 
! permutation and unit lower bidiagonal block matrices and U is upper 
! triangular with nonzeroes in only the main block diagonal and first 
! two block superdiagonals.  
! This is a block version of LAPACK DGTTRF subroutine.
!
! Arguments:
! ==========  
! N (input) INTEGER
!     The number of block rows of the matrix A.  N > 0.
!
! NB (input) INTEGER
!     The size of blocks.  NB > 0.
!
! D (input/output) REAL*8 array, dimension (NB,N*NB)
!     On entry, the array stores N diagonal blocks (each of size NB by NB) 
!         of the matrix to be factored. The blocks are stored 
!         sequentially: first NB columns of D store block D_1, second NB 
!         columns store block D_2,...,last NB columns store block D_N.
!     On exit, the array stores diagonal blocks of triangular factor L 
!         and U. Diagonal blocks of lower triangular factor L replace
!         respective lower triangles of blocks D_j (1 <= j <= N). 
!         Diagonal units are not stored. Diagonal blocks of upper 
!         triangular factor U replace respective upper triangles of 
!         blocks D_j.
!
! DL (input/output) REAL*8 array, dimension (NB,(N-1)*NB)
!     On entry, the array stores N-1 subdiagonal blocks (each of size  
!         NB by NB) of the matrix to be factored. The blocks are stored 
!         sequentially: first NB columns of DL store block B_1, second 
!         NB columns store block B_2,...,last NB columns store block
!         B_N-1.      
!     On exit, the array stores subdiagonal blocks of lower triangular  
!         factor L.
!
! DU1 (input/output) REAL*8 array, dimension (NB,(N-1)*NB)
!     On entry, the array stores N-1 superdiagonal blocks (each of size  
!         NB by NB) of the matrix to be factored. The blocks are stored 
!         sequentially: first NB columns of DU1 store block C_1, second  
!         NB columns store block C_2,...,last NB columns store block 
!         C_N-1.
!     On exit, the array stores superdiagonal blocks of triangular  
!         factor U.
!
! DU2 (output) REAL*8 array, dimension (NB,(N-2)*NB)
!     On exit, the array stores blocks of the second superdiagonal of   
!         triangular factor U.
!
! IPIV (output) INTEGER array, dimension (NB,N)
!     The pivot 'local' row indices ('local' means indices vary in the 
!     range 1..2*NB. Global row index is IPIV(I,K) + (K-1)*NB ).
!
! INFO (output) INTEGER
!     = 0:        successful exit
!     = -1000     memory buffer could not be allocated
!     < 0:        if INFO = -i, the i-th argument had an illegal value
!     > 0:        if INFO = i, U(i,i) is exactly zero. The factorization
!                 can be not completed. 
! =====================================================================
      SUBROUTINE DGEBLTTRF(N, NB, D, DL, DU1, DU2, IPIV, INFO)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER N, NB, INFO
! ..Array arguments..
      REAL*8 D(NB,*), DL(NB,*), DU1(NB,*), DU2(NB,*)
      INTEGER IPIV(NB,*)
! =====================================================================
! .. Local Scalars ..
      INTEGER K, J, I, MEMSTAT
! .. Local Arrays ..
      REAL*8, ALLOCATABLE :: A(:,:)
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO=0
      IF(N .LE. 0) THEN
          INFO = -1
      ELSE IF(NB .LE. 0) THEN
          INFO = -2
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
! ..
!   Allocating a contiguous buffer for partial factorizations
      ALLOCATE(A(2*NB,3*NB), STAT=MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
          INFO = -1000
          RETURN
      END IF

      DO K = 1, N-2
! ..
! Form a 2*NB x 3*NB submatrix
!     D_K   C_K 0
!     B_K D_K+1 C_K+1
          DO J = 1, NB
              CALL DCOPY(NB,   D(1,(K-1)*NB + J), 1,      A(1,     J),1)
              CALL DCOPY(NB,  DL(1,(K-1)*NB + J), 1,   A(NB+1,     J),1)
              CALL DCOPY(NB, DU1(1,(K-1)*NB + J), 1,      A(1,  NB+J),1)
              CALL DCOPY(NB,   D(1,K*NB + J),     1,   A(NB+1,  NB+J),1)
              CALL DCOPY(NB, DU1(1,K*NB + J),     1, A(  NB+1,2*NB+J),1)
              DO I = 1, NB
                  A(I,2*NB + J) = 0D0
              END DO
          END DO
! ..
! Partial factorization of the submatrix
!     (D_K    C_K   0    )        (L_K,K    )   (U_K,K U_K,K+1, U_K,K+2)
!     (                  )  = P * (         ) *                          
!     (B_K  D_K+1   C_K+1)        (L_K+1,K+1)                            
!
!    (  0    0       0     )
!  + (                     )
!    (  0    D'_K+1  C'_K+1)
! ..
          CALL PTLDGETRF(2*NB, 3*NB, NB, A, 2*NB, IPIV(1,K), INFO)
          IF(INFO .GT. 0) THEN
! ..
! INFO is equal to the 'global' index of the element u_ii of the factor 
! U which is equal to zero
              INFO = INFO + (K-1)*NB
              GOTO 1
          END IF
! ..
! Factorization results to be copied back to arrays:
! L_K,K, U_K,K, D'_K+1 -> D
! L_K+1,K -> DL
! U_K,K+1 -> DU1
! U_K,K+2 -> DU2
          DO J = 1, NB
              CALL DCOPY(NB, A(   1,     J), 1,   D(1,(K-1)*NB + J), 1)
              CALL DCOPY(NB, A(NB+1,     J), 1,  DL(1,(K-1)*NB + J), 1)
              CALL DCOPY(NB, A(   1,  NB+J), 1, DU1(1,(K-1)*NB + J), 1)
              CALL DCOPY(NB, A(NB+1,  NB+J), 1,   D(1,K*NB     + J), 1)
              CALL DCOPY(NB, A(   1,2*NB+J), 1, DU2(1,(K-1)*NB + J), 1)
              CALL DCOPY(NB, A(NB+1,2*NB+J), 1, DU1(1,K*NB     + J), 1)
          END DO
      END DO
! ..
! Out of loop factorization of the last 2*NBx2*NB submatrix
!  (D_N-1    C_N-1)          (L_N-1,N-1      0)   (U_N-1,N-1   U_N-1,N )
!  (              ) = P_N-1* (                ) * (                    )
!  (B_N-1      D_N)          (  L_N,N-1  L_N,N)   (      0     U_N,N   )
      DO J = 1, NB
          CALL DCOPY(NB,   D(1, (N-2)*NB + J), 1,    A(1,    J), 1)
          CALL DCOPY(NB,  DL(1, (N-2)*NB + J), 1, A(NB+1,    J), 1)
          CALL DCOPY(NB, DU1(1, (N-2)*NB + J), 1, A(   1, NB+J), 1)
          CALL DCOPY(NB,   D(1, (N-1)*NB + J), 1, A(NB+1, NB+J), 1)
      END DO
! ..
! Pivoting array for the last factorization has 2*NB elements stored in
! two last columns of IPIV
      CALL DGETRF(2*NB, 2*NB, A, 2*NB, IPIV(1,N-1), INFO)
      IF(INFO .GT. 0) THEN
! ..
! INFO is equal to the 'global' index of the element u_ii of the factor  
! U which is equal to zero
          INFO = INFO + (N-2)*NB
          GOTO 1
      END IF
! ..
! Copy the last result back to arrays:
! L_N-1,N-1, L_N,N, U_N-1,N-1, U_N,N  -> D
! L_N,N-1  -> DL
! U_N-1,N  -> DU1
      DO J = 1, NB
          CALL DCOPY(NB,    A(1,      J), 1,   D(1, (N-2)*NB + J), 1)
          CALL DCOPY(NB, A(NB+1,      J), 1,  DL(1, (N-2)*NB + J), 1)
          CALL DCOPY(NB,    A(1, NB + J), 1, DU1(1, (N-2)*NB + J), 1)
          CALL DCOPY(NB, A(NB+1, NB + J), 1,   D(1, (N-1)*NB + J), 1)
      END DO
    1 DEALLOCATE(A)
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      SUBROUTINE PTLDGETRF(M, N, K, A, LDA, IPIV, INFO)
!
! ..Scalar arguments..
!      INTEGER M, N, K, LDA, INFO
! ..       
! ..Array arguments..
!      INTEGER IPIV(*)       
!      REAL*8 A(LDA,*)
! ..
! Purpose:
! ========  
! PTLDGETRF computes partial (in a case K<min(M,N)) LU factorization 
! of matrix A = P*(L*U+A1)
!
! Arguments:
! ==========     
!  M (input) INTEGER
!     The number of rows of the matrix A.  M >= 0.
!
!  N (input) INTEGER
!     The number of columns of the matrix A.  N >= 0.
!     
!  K (input) INTEGER
!     The number of columns of the matrix A participating in 
!     factorization. N >= K >= 0
!
!  A (input/output) REAL*8 array, dimension (LDA,N)
!     On entry, the M-by-N matrix A to be factored.
!     On exit:
!         if K >= min(M,N), A is overwritten by details of its LU
!                 factorization as returned by DGETRF.
!         if K < min(M,N), partial factorization A = P * (L * U + A1) 
!             is performed where P is permutation matrix (pivoting);
!         L is M by K lower trapezoidal (with unit diagonal) matrix  
!             stored in lower MxK trapezoid of A. Diagonal units 
!                 are not stored.
!         U is K by N upper trapezoidal matrix stored in upper 
!             K by N trapezoid of A;
!         A1 is (M-K) by (N-K) residual stored in intersection
!             of last M-K rows and last N-K columns of A.
!
!  LDA (input) INTEGER
!     The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV (output) INTEGER array, dimension (min(M,K))
!     The pivot indices; for 1 <= i <= min(M,K), row i of the
!     matrix was interchanged with row IPIV(i).
!
!  INFO (output) INTEGER
!     = 0:  successful exit
!     < 0:  if INFO = -i, the i-th argument had an illegal value
!     > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!           can be not completed. 
! =====================================================================
      SUBROUTINE PTLDGETRF(M, N, K, A, LDA, IPIV, INFO)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER M, N, K, LDA, INFO
! ..Array arguments..
      INTEGER IPIV(*)
      REAL*8 A(LDA,*)
! =====================================================================
! .. Local Scalars ..
      INTEGER I
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO=0
      IF(M .LT. 0) THEN
          INFO = -1
      ELSE IF(N .LT. 0) THEN
          INFO = -2
      ELSE IF( (K .GT. N) .OR. (K .LT.0)) THEN
          INFO = -3
      ELSE IF(LDA .LT. M) THEN
          INFO = -5
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
      IF(K .LT. MIN(M,N)) THEN
! ..
!    LU factorization of first K columns
! ..          
          CALL DGETRF(M, K, A, LDA, IPIV, INFO)
          IF(INFO .NE. 0) THEN
              RETURN
          END IF
! ..
!    Applying permutations returned by DGETRF to last N-K columns
! ..
          DO I = 1, K
              IF(IPIV(I).NE.I) THEN
                  CALL DSWAP(N-K, A(I,K+1), LDA, A(IPIV(I), K+1), LDA)
              END IF
          END DO
! ..
!    Updating A1
! ..          
          CALL DTRSM('L', 'L', 'N', 'U', K, N-K, 1D0, A, LDA, A(1,K+1),
     &               LDA)
          CALL DGEMM('N', 'N', M-K, N-K, K, -1D0, A(K+1,1), LDA,
     &               A(1,K+1), LDA, 1D0, A(K+1,K+1), LDA)
      ELSE
          CALL DGETRF(M, N, A, LDA, IPIV, INFO)
          IF(INFO .NE. 0) THEN
              RETURN
          END IF
      END IF

      RETURN
      END
