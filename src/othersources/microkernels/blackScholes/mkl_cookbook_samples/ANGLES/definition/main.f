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
!     Example of calculating principal angles between two subspaces 
!     given as linear spans of columns 
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Purpose:
! ======== 
!
! Given two subspaces of the same space the example shows how to 
! compute so called 'principal angles' 
! (http://en.wikipedia.org/wiki/Principal_angles) between them. 
! The principal angles can be used to measure 'distance' between 
! subspaces (all zero angles mean one of subspaces is a proper subspace 
! of another).
!
! The subspaces are given as linear spans of columns of two rectangular 
! matrices. The solution consists of the following steps:
! 1)  Build orthonormal bases U1 and V1 in the subspaces by 
!     orthogonalizing the columns of initial matrices U and V via call 
!     QR factorization with pivoting. Simulataneously, the dimensions 
!     of subspaces are computed.
! 2)  Compute the product W=U1^t*V1      
! 3)  Compute singular values of W which are cos(theta_i) where theta_i
!     are principal angles
      PROGRAM Principal_Angles
      IMPLICIT NONE
!
! ..Scalar parameters used as input data..
! N   INTEGER    
!     The space dimension - number of rows in matrices U and V. N > 0.
!
! KU, KV INTEGER 
!     Numbers of columns in matrices U and V respectively. KU > 0, KV>0.
      INTEGER N, KU, KV
      PARAMETER (N=5, KU=3, KV=2)
!
! ..Arrays used as input data..
! U   REAL*8 array, dimension (N,KU)
!     Stores columnwise components of KU vectors spanning the first 
!     subspace. Initialized by DATA operator.
!
! V   REAL*8 array, dimension (N,KV)
!     Stores columnwise components of KV vectors spanning the second 
!     subspace. Initialized by DATA operator.
      REAL*8 U(N,KU), V(N,KV)
!
! U1   REAL*8 array, dimension (N,KU)
!     Stores columnwise components of orthogonal basis in the first 
!     subspace as computed by BASIS. Actual second dimension is 
!     KU1 <= KU.
!
! V1   REAL*8 array, dimension (N,KV)
!     Stores columnwise components of orthogonal basis in the second 
!     subspace as computed by BASIS. Actual second dimension is 
!     KV1 <= KV.
      REAL*8 U1(N,KU), V1(N,KV)
!      
! ..Local scalars..
! KU1, KV1, KMIN INTEGER
!     Dimension of the linear subspace spanned by columns of U, 
!     dimension of the linear subspace spanned by columns of V and
!     minimum of these dimensions respectively.
!     Dimensions of the subspaces are defined by calling BASIS.      
      INTEGER KU1, KV1, KMIN
!
! INFO INTEGER
!     Output parameter used by subroutines
!     BASIS and DGESVD
!     On exit from BASIS:
!INFO  = 0       if calculation suceeded
!      = -1000   if memory cannot be allocated  
!      = -2000 + INFO1 if DGEQPF failed (INFO1 returned by DGEQPF)
!      = -3000 + INFO1 if DORGQR failed (INFO1 returned by DORGQR)  
!
!     See DGESVD description re possible values of INFO returned by 
!     this subroutine.
      INTEGER INFO
!
! MEMSTAT INTEGER
!     Variable used to check the memory allocation was successful
      INTEGER MEMSTAT
!
! LWORK INTEGER
!     Dimemsion of WORK array for DGESVD. Also used as input parameter
!     to query DGESVD.
      INTEGER LWORK
!
! I, J INTEGER
!     Loop indices
      INTEGER J, I
!
! EPS REAL*8,  1 >> EPS > EPS1 where EPS1 is relative machine precision.
!     The threshold to define dimension of a subspace. Input parameter 
!     for BASIS. Initialized by DATA operator.
      REAL*8 EPS
!
! F   REAL*8
!     Local scalar 
      REAL*8 F
!
! ..Local arrays..
! VU  REAL*8, dimesion (N)
!     Stores components of a computed principal vector from the subspace 
!         spanned by columns of U while for printing results.
!      
! VV  REAL*8, dimesion (N)
!     Stores components of computed principal vector from the subspace 
!         spanned by columns of V while printing results.
      REAL*8 VU(N), VV(N)
!
! ..Local allocatable arrays..
      REAL*8, ALLOCATABLE :: W(:,:), VECL(:,:), VECRT(:,:)
      REAL*8, ALLOCATABLE :: S(:), WORK(:)
!
! ..External subroutines..
      EXTERNAL BASIS,
     &         TEST_BASIS, 
     &         DGEMM,
     &         DGESVD,
     &         DGEMV
!
! ..Intrinsic functions..
      INTRINSIC MIN, DACOS
!      
! Input data initialization
      DATA U/1.0D0,2.0D0,3.0D0,4.0D0,5.0D0,
     &       6.0D0,7.0D0,8.0D0,9.0D0,1.0D1,
     &       1.1D1,1.2D1,1.3D1,1.4D1,1.5D1/
      DATA V/1.6D1,1.7D1,1.8D1,1.9D1,2.0D1,
     &       2.1D1,2.2D1,2.3D1,2.4D1,2.5D1/
      DATA EPS/1E-13/

! ..
! .. Executable Statements ..
! ..
! Build an orthonormal basis U1 in the subspace spanned by columns of U.
! KU1 is found rank of U (subspace dimension). 
      CALL BASIS(N, KU, EPS, U, N, KU1, U1, N, INFO)
      IF(INFO .NE. 0) THEN
          PRINT*, "BASIS failed. INFO=",INFO
          STOP 1
      END IF
      PRINT*,"Dimesion of the first subspace is ",KU1
!
! Test the basis
      CALL TEST_BASIS(N, KU, KU1, U, N, U1, N, INFO, EPS)
      IF(INFO .NE. 0) THEN
          PRINT *,"For the first basis TEST_BASIS returned 
     &             nonzero INFO=",INFO
          STOP 1
      END IF
!      
! Build an orthonormal basis V1 in the subspace spanned by columns of V. 
! KV1 is found rank of V (subspace dimension). 
      CALL BASIS(N, KV, EPS, V, N, KV1, V1, N, INFO)
      IF(INFO .NE. 0) THEN
          PRINT*, "BASIS failed. INFO=",INFO
      END IF
      PRINT *,"Dimesion of the second subspace is ",KV1
!
! Test the basis
      CALL TEST_BASIS(N, KV, KV1, V, N, V1, N, INFO, EPS)
      IF(INFO .NE. 0) THEN
          PRINT *,"For the second basis TEST_BASIS returned 
     &             nonzero INFO=",INFO
          STOP 1
      END IF
!      
! Alocate memory for U^t*V
      ALLOCATE(W(KU1,KV1), STAT = MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *,"Could not allocate W of size",KU1,"x",KV1
         STOP 1
      END IF
      KMIN = MIN(KU1, KV1)
!
! Allocate memory for singular values      
      ALLOCATE(S(KMIN), STAT = MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *,"Could not allocate S of size",KMIN
         STOP 1
      END IF
!
! Allocate memory for left singular vectors      
      ALLOCATE(VECL(KU1,KMIN), STAT = MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *,"Could not allocate VECL of size",KU1,"x",KMIN
         STOP 1
      END IF
!
! Allocate memory for right singular vectors      
      ALLOCATE(VECRT(KMIN,KV1), STAT = MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *,"Could not allocate VECL of size",KU1,"x",KMIN
         STOP 1
      END IF
!
! Form W = U^t*V
      CALL DGEMM("T", "N", KU1, KV1, N, 1D0, U1, N, V1, N, 0D0, W, KU1)
!
! Query DGESVD for optimal workspace      
      LWORK = -1
      CALL DGESVD("S", "S", KU1, KV1, W, KU1, S, VECL, KU1, 
     &            VECRT, KMIN, F, LWORK, INFO)
      IF(INFO .EQ. 0) THEN
          LWORK = F
      ELSE
          PRINT *,"DGESVD failed on query"
          STOP 1
      END IF
!
! Allocate optimal size WORK 
      ALLOCATE(WORK(LWORK), STAT = MEMSTAT)
      IF(MEMSTAT .NE. 0) THEN
         PRINT *,"Could not allocate WORK of size",LWORK
         STOP 1
      END IF
!
! SVD of W = U^t*V
      CALL DGESVD("S", "S", KU1, KV1, W, KU1, S, VECL, KU1, 
     &            VECRT, KMIN, WORK, LWORK, INFO)
      IF(INFO .NE. 0) THEN
          PRINT *,"DGESVD returned nonzero INFO=",INFO
          STOP 1
      END IF
!      
! Print results  
      PRINT *,"Computations completed successfully."
      PRINT *,"Principal vectors and angles:"
      DO I=1,KMIN
          CALL DGEMV("N", N, KU1, 1D0, U1, N, VECL(1,I), 1, 0d0, VU, 1)
          CALL DGEMV("N", N, KV1, 1D0, V1, N, VECRT(1,I), 1, 0d0, VV, 1)
          PRINT *,"Principal angle (",i,")=", DACOS(MIN(S(I),1D0)),"rad"
          PRINT *,"between vector "
          DO J=1,N
              PRINT *,VU(J)
          END DO
          PRINT *," and vector " 
          DO J=1,N
              PRINT *,VV(J)
          END DO
      END DO
!
! Free memory      
      DEALLOCATE(W)
      DEALLOCATE(S)
      DEALLOCATE(WORK)
      DEALLOCATE(VECL)
      DEALLOCATE(VECRT)
      STOP
      END
      
      