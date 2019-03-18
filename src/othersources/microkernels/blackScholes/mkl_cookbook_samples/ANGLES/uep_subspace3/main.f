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
!     subspaces of a block triangular matrix of big size.
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Purpose:
! ======== 
! Given block triangular matrix AA
! ( A F )
! ( 0 B )
! with following structure:
!      A is K-by-K-matrix     
!      B is (N-K)-by-(N-K)-matrix
!      F is any rectangular K-by-(N-K)-matrix
! principal angles between its two invariant subspaces that correspond 
! to spectra of A and B respectively are computed.
!
! Possible failures:
! 1) Errors in forming the coefficinet matrix (nonzero info in FSYLVOP)
! 2) PARDISO could not solve the system of equations. See PARDISO 
!    diagnostics
! 2) Failure in orthonormalization of the basis in the invariant 
!    subspace. This may happen due to 
!         internal errors via computations (see INFO on exit from BASIS
!         described in BASIS source code)
!    or  
!         if the basis found via solving equations is very ill 
!         conditioned (the vectors span a space of dimension less than 
!         N-K).
! 3) Nonzero INFO returned by TEST_BASIS (see auxi.f)
! 4) Allocation memory failed within this code
! 5) Nonzero INFO returned by DGESVD
      PROGRAM inv_subspace3
      IMPLICIT NONE
! ..Scalar parameters used as input data..
! N   INTEGER    
!     The order of the matrix. N > 0.
!
! K   INTEGER
!     The order of square block A. K > 0.
      INTEGER N,K
      PARAMETER (N=250,K=130)
!
! EPS REAL*8
!     The threshold used by subroutine BASIS while defining subspaces
!     dimesions. Initialized by operator DATA.
      REAL*8 EPS
!
! THRESH REAL*8 
!     The threshold to be used in testing the residual.
!     Initialized by DATA operator
      REAL*8 THRESH
!
! ..Arrays used as input data..
! AA  REAL*8 array, dimension (N,N)
!     The array stores elements of input upper block triangualr matrix.
!     Initialized randomly by call DLARNV.
      REAL*8 AA(N,N)
!      
! ..Local scalars..
! NK INTEGER
!     The order of the coefficient matrix. NK = K*(N-K). 
      INTEGER NK
      PARAMETER (NK = K*(N-K))
!
! NORMA, NORMB, NORMX, NORMRES REAL*8
!     Scalars to store matrices and vectors norms for testing the 
!     residual.
      REAL*8 NORMA, NORMB, NORMX, NORMRES
! ..
! NNZ INTEGER
!     The total number of nonzeros in the coefficient matrix.
      INTEGER NNZ
      PARAMETER (NNZ = NK*(N-1))
!
      INTEGER INFO, LWORK, MEMSTAT, K1, KMIN, I, J, IERR
!
! ..Local arrays..
!
! ISEED INTEGER array, dimension 4.
!     The array stores seed values for DLARNV. 
!     Initailaized by DATA operator.
      INTEGER ISEED(4)
!      
! ROWINDEX INTEGER array, dimension NK+1
!     The array stores row indices of nonzero elements of the 
!     coefficient matrix in CSR format.
      INTEGER ROWINDEX(NK+1)
!
! COLS  INTEGER array, dimesnion NNZ
!     The array stores column indices of nonzero elements of the 
!     coefficient matrix in CSR format.
      INTEGER COLS(NNZ)
!
! VAL REAL*8 array, dimension NNZ.
!     The array stores values of nonzero elements of the 
!     coefficient matrix in CSR format.
      REAL*8 VAL(NNZ)
!
! IPARM INTEGER array, dimension 64
!     This array is used to pass various parameters to Intel MKL 
!     PARDISO and to return some useful information after execution
!     of the solver. The array is filled in by PARDISOINIT with 
!     default values.  
      INTEGER IPARM(64)
!
! PERM INTEGER array, dimension NK
!     Working array for PARDISO.
      INTEGER PERM(NK)
!
! PT INTEGER array, dimesion 64
!     Array containing PARDISO handle.      
      INTEGER PT(64)
!
! T REAL*8 array, dimesion (N,N)
!     The array stores the transformation matrix with block structure
!     (I X)
!     (0 I)
!     wher the fisrt unit matrix is of order K,
!     the second unit matrix is of order N-K,
!     Kx(N-K) matrix X is a solution to the Sylvester equation.
      REAL*8 T(N,N)
!
! F, X REAL*8 arrays, size NK
!     The arrays contain respectively the right hand side and solution 
!     vector (as returened by PARDISO) of the Sylvester equation. 
      REAL*8 FF(NK), X(NK) 
!
! U1  REAL*8 array, size (N,N-K)
!     The array contains orthonormal basis in the second invariant 
!      subspace.
      REAL*8 U1(N,N-K)
!
! WORK1 REAL*8 array, dimension 1
!     Work array for queries.
      REAL*8 WORK1(1) 
!
! S REAL*8 allocatable array, size min(K,N-K)
!     The array stores singular values that define principal angles 
!     between invariant subspaces
      REAL*8, ALLOCATABLE :: S(:)
!
! WORK REAL*8 allocatable array, size defined via query
!     Workspace for DGESVD
      REAL*8, ALLOCATABLE :: WORK(:)
!
! ..External subroutines..
      REAL*8 DLANGE, DNRM2, DLAMCH
      EXTERNAL DLARNV, PARDISOINIT, PARDISO, DTRMM, DGESVD, 
     &         DNRM2, DLAMCH
!      
! Input data initialization
      DATA EPS/1D-14/
! Empirical value of O to be used in testing residual
      DATA THRESH /3D0/
      DATA ISEED/1,2,3,17/
! ..
! .. Executable Statements ..
! ..
! Generate a random block triangular NxN-matrix
      PRINT *,"Generating a random block triangular matrix..."
      DO J=1,N
          CALL DLARNV(2, ISEED, K, AA(1,J))
      END DO
      
      DO J=1,K
          DO I=K+1,N
               AA(I,J)=0D0
          END DO
      END DO
      DO J=K+1,N
          CALL DLARNV(2, ISEED, N-K, AA(K+1,J))
      END DO
      NORMA = DLANGE('F', K, K, AA, N, WORK1 )
      NORMB = DLANGE('F', N-K, N-K, AA(K+1,K+1), N, WORK1 )
! Form the sparse coefficient matrix to solve the Sylvester equation
      PRINT *,"Forming coefficient matrix in CSR format... "
      CALL FSYLVOP(K, AA, N, N-K, AA(K+1,K+1), N, -1D0, 1D0, COLS,
     &            ROWINDEX, VAL, INFO)
      IF(INFO .NE. 0) THEN
          PRINT *,-INFO,"th parameter in FSYLVOP has an illegal value"
          STOP 1
      END IF
      
! Form the right hand side of the Sylvester equation
      DO I=1,K
          DO J=1,N-K
              FF((J-1)*K+I) = AA(I,J+K)
          END DO
      END DO
      PRINT *,"Initializing PARDISO..."
      CALL PARDISOINIT (PT, 1, IPARM)
      CALL PARDISO (PT, 1, 1, 11, 13, NK, VAL, ROWINDEX, 
     &                COLS, PERM, 1, IPARM, 1, FF, X, IERR)
      IF(IERR .NE. 0) THEN
          PRINT *,"PARDISO returned error=",IERR
          STOP 1
      ELSE
          PRINT *,"PARDISO successfully solved the system."
      END IF
      NORMX = DNRM2(NK, X, 1 )

! Testing the solution of Sylvester equation
! Form the transformation matrix
      DO I = 1,N
          T(I,I) = 1D0
          DO J = I+1,N
              T(I,J) = 0D0
          END DO
      END DO
      DO J = 1,N-K
          DO I = 1,K
              T(I,J+K) = X((J-1)*K+I)
          END DO
      END DO
! Compute AA:=AA*T
      CALL DTRMM('R', 'U', 'N', 'U', N, N, 1D0, T, N, AA, N)

! Change the sign to get inv(T)
      DO J = 1,N-K
          DO I = 1,K
              T(I,J+K) = -T(I,J+K)
          END DO
      END DO
! Compute AA:=T^{-1}*AA
      CALL DTRMM('L', 'U', 'N', 'U', N, N, 1D0, T, N, AA, N)
! If no computational errors we would get
! (I -X )(A F)(I X) = (A 0)
! (0  I )(0 B)(0 I)   (0 B)
! So, compute the Frobenius norm of the block which has to anihilate.  
      NORMRES = DLANGE('F', K, N-K, AA(1,K+1), N, WORK1 )
      PRINT *,"Frobenius norm of the submatrix that should be zero=", 
     &        NORMRES
      IF(NORMRES/((NORMA+NORMB)*NORMX*NK*DLAMCH('E')) .LE. THRESH ) THEN
          PRINT *,"Testing residual passed."
      ELSE
          PRINT *,"Testing residual failed."
          STOP 1
      END IF
      
! Restore the signs in T
      DO J = 1,N-K
          DO I = 1,K
              T(I,J+K) = -T(I,J+K)
          END DO
      END DO
! Orthonormalization of columns
      PRINT *,"Forming the basis..."
      CALL BASIS(N, N-K, EPS, T(1,K+1), N, K1, U1, N, INFO)
      IF(INFO .NE. 0) THEN
          PRINT*, "BASIS failed. INFO=",INFO
          STOP 1
      END IF
      IF(K1 .NE. N-K) THEN
          PRINT *, "Failure: K1=",K1,"/=(N-K)=",N-K
          STOP 1
      END IF
      
! Testing orthogonalization is correct up to some error - orthonormality of columns 
      PRINT *,"Testing the basis..."
      CALL TEST_BASIS(N, N-K, K1, T(1,K+1), N, U1, N, INFO, EPS)
      IF(INFO .NE. 0) THEN
          PRINT *,"TEST_BASIS returned nonzero INFO=",INFO
          STOP 1
      ELSE
          PRINT *, "Testing the second basis passed."
          PRINT *, "Now compute the angles between invariant subspaces."
      END IF

      KMIN = MIN(K,K1)
! Allocate memory for singular values      
      ALLOCATE(S(KMIN), STAT=MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *,"Could not allocate S of size",KMIN
         STOP 1
      END IF

! Query DGESVD for optimal workspace  
      PRINT *,"Calling SVD..."
      LWORK = -1
      CALL DGESVD("N", "N", K, K1, U1, N, S, WORK1, 1, WORK1, 1, WORK1,
     &      LWORK, INFO)
      IF(INFO .EQ. 0) THEN
          LWORK = WORK1(1)
      ELSE
          PRINT *,"DGESVD failed on query"
          STOP
      END IF
      ALLOCATE(WORK(LWORK), STAT=MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *,"Could not allocate WORK of size",LWORK
         STOP 1
      END IF
      CALL DGESVD("N", "N", K, K1, U1, N, S, WORK1, 1, WORK1, 1, WORK, 
     &            LWORK, INFO)
      IF(INFO.NE.0) THEN
          PRINT *,"DGESVD returned nonzero INFO=",INFO
          STOP 1
      END IF
! Print results  
      PRINT *,"Principal angles between invariant subspaces:"
      DO I = 1,KMIN
          PRINT *,"Principal angle (",I,")=", DACOS(MIN(S(I),1D0))
      END DO
      DEALLOCATE(S)      
      DEALLOCATE(WORK)
      STOP
      END


