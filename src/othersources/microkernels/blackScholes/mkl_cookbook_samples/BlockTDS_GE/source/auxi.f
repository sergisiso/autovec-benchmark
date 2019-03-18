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
!      - Computing ratio ||A-L*U||_F/||A||_F of Frobenius norm of the   
! 	 residual to the Frobenius norm of the initial matrix. 
!      - Calculating max_(i=1,...,NRHS){||AX(i)-F(i)||/||F(i)||} of  
!        ratios of residuals to norms of RHS vectors for a system of 
!        linear equations with tridiagonal coefficient matrix and 
!        multiple RHS      
!
!**********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      REAL*8 FUNCTION RESID1(N, NB, DL, D, DU1, DU2, IPIV, DLCPY,
!     &                       DCPY, DU1CPY)
!
! ..Scalar arguments..
!      INTEGER N, NB
! ..       
! ..Array arguments..
!      INTEGER IPIV(*)       
!      REAL*8 D(NB,*), DL(NB,*), DU1(NB,*), DU2(NB,*)
!      REAL*8 DCPY(NB,*), DLCPY(NB,*), DU1CPY(NB,*)
! ..
! Purpose:
! ========  
! Given LU factorization of block tridiagonal matrix A function RESID1  
! returns ratio ||A-L*U||_F/||A||_F of Frobenius norm of the residual  
! to the Frobenius norm of the initial matrix. The ratio provides info
! on how good the factorization is.
! 
! Arguments:
! ==========  
! N (input) INTEGER
!     The number of block rows of the matrix A.  N > 0.
!
! NB (input) INTEGER
!     The size of blocks.  NB > 0.
!
! DL (input) REAL*8 array, dimension (NB,(N-1)*NB)
!     The array stores N-1 subdiagonal blocks (each of size NB by NB) of  
!         lower triangular factor L. The blocks are stored sequentially 
!         block by block.
!
! D (input) REAL*8 array, dimension (NB,N*NB)
!     The array stores N diagonal blocks (each of size NB by NB) 
!         of triangular factors L and U. 
!
! DU1 (input) REAL*8 array, dimension (NB,(N-1)*NB)
!     The array stores N-1 superdiagonal blocks (each of size  
!         NB by NB) of triangular factor U.
!
! DU2 (input) REAL*8 array, dimension (NB,(N-2)*NB)
!     The array stores N-2 blocks of the second superdiagonal of   
!         triangular factor U.
!
! IPIV (input) INTEGER array, dimension (NB,N)
!     The array stores pivot 'local' row indices. 
!
! DLCPY (input) REAL*8 array, dimension (NB,(N-1)*NB)
!     The array stores N-1 subdiagonal blocks of initial block 
!         tridiagonal matrix.
!
! DCPY (input) REAL*8 array, dimension (NB,N*NB)
!     The array stores N diagonal blocks of initial block 
!         tridiagonal matrix.
!
! DU1CPY (input) REAL*8 array, dimension (NB,(N-1)*NB)
!     The array stores N-1 superdiagonal blocks of initial block 
!         tridiagonal matrix.
! =====================================================================
      REAL*8 FUNCTION RESID1(N, NB, DL, D, DU1, DU2, IPIV, DLCPY,
     &                       DCPY, DU1CPY)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER N, NB
! ..Array arguments..
      REAL*8 D(NB,*), DL(NB,*), DU1(NB,*), DU2(NB,*)
      REAL*8 DCPY(NB,*), DLCPY(NB,*), DU1CPY(NB,*)
      INTEGER IPIV(NB,*)
! =====================================================================
! .. Local Scalars ..
      INTEGER I, J, K, MEMSTAT
      REAL*8 EPS, S, NORM
! .. Local Arrays ..
      REAL*8, ALLOCATABLE :: B(:,:), B1(:,:), L(:,:), U(:,:)
      REAL*8 WORK(1)
! .. External Functions ..
      REAL*8 DLANGE
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      IF(N .LE. 0) THEN
          PRINT*,"RESID1: Wrong value of parameter N=",N
          STOP 1
      END IF
      IF(NB .LE. 0) THEN
          PRINT*,"RESID1: Wrong value of parameter NB=",NB
          STOP 1
      END IF
!
! Compute Frobenius norm of the initial matrix
      S    = DLANGE('F', NB,     NB*N,   DCPY, NB, WORK)
      NORM = S*S
      S    = DLANGE('F', NB, NB*(N-1),  DLCPY, NB, WORK)
      NORM = NORM + S*S
      S    = DLANGE('F', NB, NB*(N-1), DU1CPY, NB, WORK)
      NORM = DSQRT(NORM + S*S)
!
! For computing residual it is convenient to have buffers where blocks
! of L and U factors and copy of the initial matrix are copied. Then we
! can use GEMM for computations.
!
! B and B1 are 2*NB x 3*NB buffers of the following structure
! (B_11 B_12 B_13)
! (B_21 B_22 B_33)
! to store intermediate results. B is used to accumulate row-wise 
! products of L and U factors.
      ALLOCATE(B(2*NB,3*NB), STAT=MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
          PRINT *,"RESID1: Could not allocate B of size ",2*NB,"x",3*NB
          STOP 1
      END IF
      ALLOCATE(B1(2*NB,3*NB), STAT=MEMSTAT)
      IF(MEMSTAT.NE.0) THEN
          PRINT *,"RESID1: Could not allocate B1 of size ",2*NB,"x",3*NB
          STOP 1
      END IF
      DO J= 1, 3*NB
          DO I = 1, 2*NB
              B(I,J)  = 0D0
              B1(I,J) = 0D0
          END DO
      END DO
!
! L is a buffer to store lower triangular blocks from the left factor
! L_K,K
! L_K+1,K
! in the form suitable for GEMM. Both blocks of L are considered as full matrices.
      ALLOCATE(L(2*NB,NB), STAT=MEMSTAT)
      IF(MEMSTAT.NE.0) THEN
          PRINT *,"RESID1: Could not allocate L of size ", 2*NB, "x", NB
          STOP 1
      END IF
      DO J = 1, NB
          DO I = 1, 2*NB
              L(I,J) = 0D0
          END DO
          L(J,J) = 1D0
      END DO
!
! U is a buffer to store upper triangular blocks from the right factor
! U_K,K U_K,K+1 U_K,K+2
! in the form suitable for GEMM. All three blocks of U are considered as full matrices.
      ALLOCATE(U(NB,3*NB), STAT=MEMSTAT)
      IF(MEMSTAT.NE.0) THEN
          PRINT *,"RESID1: Could not allocate U of size ", NB, "x", 3*NB
          STOP 1
      END IF
      DO J = 1, 3*NB
          DO I = 1,NB
              U(I,J) = 0D0
          END DO
      END DO
!
! Out-of-loop computing the residuals for the last block row
      DO J = 1, NB
          CALL DCOPY(   J, D(  1, (N-1)*NB + J), 1, U(  1, J), 1)
          CALL DCOPY(NB-J, D(J+1, (N-1)*NB + J), 1, L(J+1, J), 1)
      END DO
!
! B_22 = L_N,N * U_N,N
      CALL DGEMM('N', 'N', NB, NB, NB, 1D0, L, 2*NB,
     &           U, NB, 1D0, B1(NB+1, NB+1), 2*NB)
!
      DO J = 1, NB
          CALL DCOPY(   J,   D(  1, (N-2)*NB + J), 1, U(   1,      J),1)
          CALL DCOPY(  NB, DU1(  1, (N-2)*NB + J), 1, U(   1, NB + J),1)
          CALL DCOPY(NB-J,   D(J+1, (N-2)*NB + J), 1, L( J+1,      J),1)
          CALL DCOPY(  NB,  DL(  1, (N-2)*NB + J), 1, L(NB+1,      J),1)
      END DO
!
! (B_11 B_12)    (L_N-1,N-1     0)      (U_N-1,N-1 U_N-1,N)
! (B_21 B_22)  = (  L_N,N-1 L_N,N)  *   (0           U_N,N)
      CALL DGEMM('N', 'N', 2*NB, 2*NB, NB, 1D0, L, 2*NB,
     &           U, NB, 1D0, B1, 2*NB)
!
! Pivoting permutations should be taken into account.
! The last column of pivoting relates the last block row
      DO I = NB, 1, -1
          IF(IPIV(I,N) .NE. NB + I) THEN
              CALL DSWAP(2*NB, B1(NB+I, 1), 2*NB, B1(IPIV(I,N),1), 2*NB)
          END IF
      END DO
      DO I = NB, 1, -1
          IF(IPIV(I,N-1) .NE. I) THEN
              CALL DSWAP(2*NB, B1(I, 1), 2*NB, B1(IPIV(I,N-1), 1), 2*NB)
          END IF
      END DO
!
! The N-th block row won't change - we can compute the respective residual
      DO J = 1, NB
          DO I = 1, NB
              B1(NB+I,    J) = B1(NB+I,    J) - DLCPY(I, (N-2)*NB+J)
              B1(NB+I, NB+J) = B1(NB+I, NB+J) -  DCPY(I, (N-1)*NB+J)
          END DO
      END DO
!
! ... and compute its contribution in Frobenius norm of the residual.
      S   = DLANGE('F', NB, NB, B1(NB+1, 1), 2*NB, WORK)
      EPS = S*S
      S   = DLANGE('F', NB, NB, B1(NB+1, NB+1), 2*NB, WORK)
      EPS = EPS + S*S
!
! Main loop along block rows in reverse order.
      DO K = N-2, 1, -1
          DO J = 1, NB
              CALL DCOPY(   J,  D(  1,(K-1)*NB + J),1, U(   1,     J),1)
              CALL DCOPY(  NB,DU1(  1,(K-1)*NB + J),1, U(   1,  NB+J),1)
              CALL DCOPY(  NB,DU2(  1,(K-1)*NB + J),1, U(   1,2*NB+J),1)
              CALL DCOPY(NB-J,  D(J+1,(K-1)*NB + J),1, L( J+1,     J),1)
              CALL DCOPY(  NB, DL(  1,(K-1)*NB + J),1, L(NB+1,     J),1)
          END DO

          CALL DGEMM('N', 'N', 2*NB, 3*NB, NB, 1D0, L, 2*NB,
     &               U, NB, 0D0, B, 2*NB)

          DO J = 1, 2*NB
              DO I = 1, NB
                  B(NB+I, NB+J) = B(NB+I, NB+J) + B1(I, J)
              END DO
          END DO
          DO I = NB, 1, -1
              IF(IPIV(I, K) .NE. I) THEN
                  CALL DSWAP(3*NB, B(I, 1), 2*NB, B(IPIV(I,K), 1), 2*NB)
              END IF
          END DO
!
! K-th row is ready for computing residual
          DO J = 1, NB
              DO I = I, NB
                  B1(NB+I,     J) = B(NB+I,     J) - DLCPY(I,(K-1)*NB+J)
                  B1(NB+I,  NB+J) = B(NB+I,  NB+J) -  DCPY(I,K*NB+J)
                  B1(NB+I,2*NB+J) = B(NB+I,2*NB+J) -DU1CPY(I,K*NB+J)
              END DO
      END DO
!
! In the exact result the second upper diagonal must be zero:
          S   = DLANGE('F', NB, NB, B(1,2*NB+1), 2*NB, WORK)
          EPS = EPS + S*S
! 
! Contributing norms in the K-th row to the norm of the residual:
          S   = DLANGE('F', NB, NB, B1(NB+1, 1), 2*NB, WORK)
          EPS = EPS + S*S
          S   = DLANGE('F', NB, NB, B1(NB+1, NB+1), 2*NB, WORK)
          EPS = EPS + S*S
          S   = DLANGE('F', NB, NB, B1(NB+1, 2*NB+1), 2*NB, WORK)
          EPS = EPS + S*S
          DO J = 1, 2*NB
              CALL DCOPY(NB, B(1, J), 1, B1(1, J), 1)
          END DO
      END DO
!
! Finally, ratio of the norm of the residual to the norm of the  initial 
! matrix
      RESID1 = DSQRT(EPS)/NORM
      DEALLOCATE(B)
      DEALLOCATE(B1)
      DEALLOCATE(L)
      DEALLOCATE(U)
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      REAL*8 FUNCTION RESID2(N, NB, NRHS, DL, D, DU1, X, LDX, B, LDB)
!
! ..Scalar arguments..
!      INTEGER N, NB, NRHS, LDX, LDB
! ..       
! ..Array arguments..
!      REAL*8 DL(NB,*), D(NB,*), DU1(NB,*), X(LDX,*), B(LDB,*)
!
! ..
! Purpose:
! ========  
! Given solution X to a system of linear equations AX=B with tridiagonal 
! coefficient matrix A and multiple right hand sides B function RESID2  
! returns max_(i=1,...,NRHS){||AX(i)-B(i)||/||B(i)||}. This quantity 
! provides info on how good the solution is.
! 
! Arguments:
! ==========  
! N (input) INTEGER
!     The number of block rows of the matrix A.  N > 0.
!
! NB (input) INTEGER
!     The size of blocks.  NB > 0.
!
! NRHS (input) INTEGER
!     The number of right hand sides. NRHS >0.
!
! DL (input) REAL*8 array, dimension (NB,(N-1)*NB)
!     The array stores N-1 subdiagonal blocks (each of size NB by NB) of  
!         the coefficient matrix. The blocks are stored sequentially 
!         block by block.
!
! D (input) REAL*8 array, dimension (NB,N*NB)
!     The array stores N diagonal blocks (each of size NB by NB) 
!         of  the coefficient matrix. The blocks are stored sequentially 
!         block by block.
!
! DU1 (input) REAL*8 array, dimension (NB,(N-1)*NB)
!     The array stores N-1 superdiagonal blocks (each of size NB by NB) 
!         of  the coefficient matrix. The blocks are stored sequentially 
!         block by block.
!
! X (input) REAL*8 array, dimension (LDX,NRHS).
!     The array stores components of the solution to be tested.
!
! LDX (input) INTEGER
!     The leading dimension of the array X. LDX >= N*NB.
!
! B (input) REAL*8 array, dimension (LDB,NRHS).
!     The array stores components of the right hand sides.
!
! LDB (input) INTEGER
!     The leading dimension of the array B. LDB >= N*NB.
! =====================================================================

      
      REAL*8 FUNCTION RESID2(N, NB, NRHS, DL, D, DU1, X, LDX, B, LDB)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER N, NB, LDB, NRHS, LDX
! ..Array arguments..
      REAL*8 DL(NB,*), D(NB,*), DU1(NB,*), X(LDX,*), B(LDB,*)
! =====================================================================
! .. Local Scalars ..
      REAL*8 EPS, S, S1
      INTEGER K, I, J, MEMSTAT
! .. Local Arrays ..
      REAL*8 WORK(1)
      REAL*8, ALLOCATABLE :: NORMS(:)
! .. External Functions ..
      REAL*8 DNRM2
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      IF(N .LE. 0) THEN
          PRINT*,"RESID1: Wrong value of parameter N=",N
          STOP 1
      END IF
      IF(NB .LE. 0) THEN
          PRINT*,"RESID1: Wrong value of parameter NB=",NB
          STOP 1
      END IF
      IF(NRHS .LE. 0) THEN
          PRINT*,"RESID1: Wrong value of parameter NRHS=",NRHS
          STOP 1
      END IF
      IF(LDX .LT. N*NB) THEN
          PRINT*,"RESID1: Wrong value of parameter LDX=",LDX
          STOP 1
      END IF
      IF(LDB .LT. N*NB) THEN
          PRINT*,"RESID1: Wrong value of parameter LDB=",LDB
          STOP 1
      END IF
!
! Initializing return value
      RESID2 = 0D0
      ALLOCATE(NORMS(NRHS), STAT=MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *, "RESID2: Could not allocate memory for norms 
     &            of size=",NRHS
         STOP 1
      END IF
!
! Compute norms of RHS vectors
      DO I = 1, NRHS
          NORMS(I) = DNRM2(NB*N, B(1,I), 1)
      END DO
!      
! Computing B(1)-D(1)*X(1)-DU1(1)*X(2) out of loop     
      CALL DGEMM('N','N',NB, NRHS, NB, -1D0, D, NB, X, LDX, 1D0, B, LDB)
      CALL DGEMM('N','N',NB, NRHS, NB, -1D0, DU1, NB, X(NB+1,1), LDX, 
     &            1D0, B, LDB)
!
! In the loop computing B(K)-DL(K-1)*X(K-1)-D(K)*X(K)-DU1(K)*X(K+1) 
      DO K=2,N-1
          CALL DGEMM('N','N',NB, NRHS, NB, -1D0, DL(1,(K-2)*NB+1), NB,
     &            X((K-2)*NB+1,1), LDX, 1D0, B((K-1)*NB+1,1), LDB)
          
          CALL DGEMM('N','N',NB, NRHS, NB, -1D0, D(1,(K-1)*NB+1), NB,
     &            X((K-1)*NB+1,1), LDX, 1D0, B((K-1)*NB+1,1), LDB)
          
          CALL DGEMM('N','N',NB,NRHS, NB, -1D0, DU1(1,(K-1)*NB+1), NB,
     &            X(K*NB+1,1), LDX, 1D0, B((K-1)*NB+1,1), LDB)
      END DO
!
! Computing B(N)-DL(N-1)*X(N-1)-D(N)*X(N) out of loop     
      CALL DGEMM('N','N',NB, NRHS, NB, -1D0, D(1,(N-1)*NB+1), NB, 
     &            X((N-1)*NB+1,1), LDX, 1D0, B((N-1)*NB+1,1), LDB)
      CALL DGEMM('N','N',NB, NRHS, NB, -1D0, DL(1,(N-2)*NB+1), NB, 
     &            X((N-2)*NB+1,1), LDX, 1D0, B((N-1)*NB+1,1), LDB)
!     
! Compute norms of residual vectors divided by norms of RHS vectors
      DO I = 1, NRHS
          S = DNRM2(NB*N, B(1,I), 1)
          RESID2= MAX(RESID2, S/NORMS(I))
      END DO
!
! Deallocate buffer
      DEALLOCATE(NORMS)
      RETURN
      END
      
