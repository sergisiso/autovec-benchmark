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
!     Example of calculating principal angles between two invariant 
!     subspaces of a triangular block matrix 
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Purpose:
! ======== 
! Given triangular block matrix AA
! ( A F )
! ( 0 B )
! with following structure:
!      A is upper triangular K-by-K-matrix     
!      B is upper triangular (N-K)-by-(N-K)-matrix
!      F is any rectangular K-by-(N-K)-matrix
! principal angles between its two invariant subspaces that correspond to 
! spectra of A and B respectively are computed.
!
      PROGRAM inv_subspace
      IMPLICIT NONE
! ..Scalar parameters used as input data..
! N   INTEGER    
!     The order of the matrix. N > 0.
!
! K   INTEGER
!     The order of square block A. K > 0.
      INTEGER N, K
      PARAMETER (N=7, K=3)
!
! EPS REAL*8
!     The threshold used by subroutine BASIS while defining subspaces
!         dimesions. Initialized by operator DATA.
      REAL*8 EPS
!
! ..Arrays used as input data..
! AA  REAL*8 array, dimension (N,N)
!     The array stores elements of input upper block triangualr matrix.
!         Initialized by operator DATA.
      REAL*8 AA(N,N) 
!      
! ..Local scalars..
      INTEGER I, J
      INTEGER INFO, LWORK, MEMSTAT, K1, KMIN
      REAL*8 ALPHA
!
! ..Local arrays..
      REAL*8 AA_COPY(N,N), T(N,N), U1(N,N-K), WORK1(1) 
      REAL*8, ALLOCATABLE :: S(:), WORK(:)
!
! ..External subroutines..
      REAL*8 DLANGE
      EXTERNAL DCOPY,
     &         DLANGE,
     &         DTRSYL,
     &         DTRMM,
     &         DGESVD,
     &         BASIS,
     &         TEST_BASIS
!      
! Input data initialization
      DATA EPS/1D-12/
      DATA AA/1.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 
     &        2.0D0, 3.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 
     &        4.0D0, 5.0D0, 6.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     &        7.0D0, 8.0D0, 9.0D0, 1.0D1, 0.0D0, 0.0D0, 0.0D0, 
     &        1.1D1, 1.2D1, 1.3D1, 1.4D1, 1.5D1, 0.0D0, 0.0D0, 
     &        1.6D1, 1.7D1, 1.8D1, 1.9D1, 2.0D1, 2.1D1, 0.0D0, 
     &        2.2D1, 2.3D1, 2.4D1, 2.5D1, 2.6D1, 2.7D1, 2.8D1/ 
! ..
! .. Executable Statements ..
! ..
! Copy AA for testing purposes because DTRSYL destroys the matrix     
      CALL DCOPY(N*N, AA, 1, AA_COPY, 1)
!
! Call DTRSYL to solve Sylvester equation A*X - X*B = F with triangular
! coefficient matrices A and B. Here 
! A = AA(1:K,1:K) and B = AA(K+1:N,K+1:N)
! are diagonal blocks of AA,
! F = AA(1:K,K+1:N) is replaced with the solution X on exit from DTRSYL.
      CALL DTRSYL('N', 'N', -1, K, N-K, AA, N, AA(K+1,K+1), N, 
     &             AA(1,K+1), N, ALPHA, INFO)
      IF(INFO .EQ. 0) THEN
          PRINT *,"DTRSYL completed, SCALE=", ALPHA
      ELSE IF(INFO .EQ. 1) THEN
          PRINT *,"DTRSYL solved perturbed equations."
      ELSE
          PRINT *,"DTRSYL failed due to illegal value of ", -INFO, 
     &            "-th argument"     
          STOP 1
      END IF
!      
! Testing the solution of Sylvester equation by computing the product
! (I  X )(A F)(I -X)  
! (0  I )(0 B)(0  I)   
! If no errors the result should be
! (A 0)
! (0 B)
      PRINT *,"Testing the solution of Sylvester equation."
      DO I = 1, N
          T(I,I) = 1D0
          DO J = I+1, N
              T(I,J) = 0D0
              T(J,I) = 0D0
          END DO
      END DO
      DO J = 1, N-K
          DO I = 1, K
              T(I,J+K) = -AA(I,J+K)
          END DO
      END DO
!
! Compute AA_COPY*T -> AA_COPY
      CALL DTRMM('R', 'U', 'N', 'U', N, N, 1D0, T, N, AA_COPY, N)
!
! If T is
! (I  X)
! (0  I)
! its inverse T^{-1} is
! (I -X)
! (0  I)
      DO J = 1, N-K
          DO I = 1, K
              T(I,J+K) = -T(I,J+K)
          END DO
      END DO
!
! Compute T^{-1}*(AA_COPY*T) -> AA_COPY
      CALL DTRMM('L', 'U', 'N', 'U', N, N, 1D0, T, N, AA_COPY, N)
!
! Compute the Frobenius norm of the block (1:K,K+1:N) which had to 
! become zero.      
      PRINT *, "Frobenius norm of the submatrix =", 
     &         DLANGE('F', K, N-K, AA_COPY(1,K+1), N, WORK1 )
!      
!
! Restore (N-K) columns of the invariant subspace
      DO J = 1, N-K
          DO I = 1, K
              T(I,J+K) = -T(I,J+K)
          END DO
      END DO
!
! Orthonormalization of columns and put them in U1.
! K1 is computed dimension of the subspace. It must be equal to N-K.
      CALL BASIS(N, N-K, EPS, T(1,K+1), N, K1, U1, N, INFO)
      IF(INFO .NE. 0) THEN
          PRINT*, "BASIS failed. INFO=", INFO
          STOP 1
      END IF
      IF(K1 .NE. N-K) THEN
          PRINT *, "Failure: K1=", K1, "/=(N-K)=", N-K
          STOP 1
      END IF
!      
! Testing orthogonalization is correct up to some error - orthonormality of columns 
      CALL TEST_BASIS(N, N-K, K1, T(1,K+1), N, U1, N, INFO, EPS)
      IF(INFO .NE. 0) THEN
          PRINT *,"TEST_BASIS returned nonzero INFO=",INFO
          STOP 1
      ELSE
          PRINT *,"Testing the second basis passed."
          PRINT *,"Now compute the angles between invariant subspaces."
      END IF
!
! Allocate memory for singular values      
      KMIN = MIN(K,K1)
      ALLOCATE(S(KMIN), STAT = MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *,"Could not allocate S of size", KMIN
         STOP 1
      END IF
!
! Query DGESVD for optimal workspace      
      LWORK = -1
      CALL DGESVD("N", "N", K, K1, U1, N, S, WORK1, 1, WORK1, 1,
     &     WORK1, LWORK, INFO)
      IF(INFO .EQ. 0) THEN
          LWORK = WORK1(1)
      ELSE
          PRINT *,"DGESVD failed on query"
          STOP 1
      END IF
      ALLOCATE(WORK(LWORK), STAT = MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *,"Could not allocate WORK of size", LWORK
         STOP 1
      END IF
      CALL DGESVD("N", "N", K, K1, U1, N, S, WORK1, 1, WORK1, 1,
     &             WORK, LWORK, INFO)
      IF(INFO .NE. 0) THEN
          PRINT *,"DGESVD returned nonzero INFO=", INFO
          STOP 1
      END IF
!
! Print results  
      PRINT *,"Principal angles between invariant subspaces:"
      DO I = 1, KMIN
          PRINT *,"Principal angle (",I,")=", DACOS(MIN(S(I), 1D0))
      END DO
      DEALLOCATE(S)      
      DEALLOCATE(WORK)
      STOP
      END

