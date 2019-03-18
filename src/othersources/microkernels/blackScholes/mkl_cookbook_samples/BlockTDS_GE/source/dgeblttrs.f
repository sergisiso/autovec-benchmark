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
!      Subroutine DGEBLTTRS for solving a system of linear equations 
!      with LU-factored block tridiagonal coefficient matrix and 
!      multiple right hand sides.
!  See respective recipe at: https://software.intel.com/en-us/node/515759 
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Definition:
! ===========
!      SUBROUTINE DGEBLTTRS(N, NB, NRHS, D, DL, DU1, DU2, IPIV, F, LDF,
!     &                        INFO)      
!
! ..Scalar arguments..
!      INTEGER N, NB, NRHS, LDF, INFO
! ..       
! ..Array arguments..
!      INTEGER IPIV(*)       
!      REAL*8 D(NB,*), DL(NB,*), DU1(NB,*), DU2(NB,*)
! ..
! Purpose:
! ========  
! DGEBLTTRS solves system of linear equations AX = F with general block 
! tridiagonal coefficient matrix 
!          (D_1  C_1                          )
!          (B_1  D_2  C_2                     )
!          (     B_2  D_3  C_3                )
!      A=  (           .........              )
!          (              B_N-2  D_N-1  C_N-1 ) 
!          (                     B_N-1  D_N   )
!      
! LU-factored by DGEBLTTRF and multiple RHS F.
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
!     The number of right hand sides. NRHS > 0.
!
! D (input) REAL*8 array, dimension (NB,N*NB)
!     The array stores N diagonal blocks (each of size NB by NB) 
!         of triangular factors L and U as they are returned by 
!         DGEBLTTRF. Diagonal blocks of factors L and U are lower and 
!         upper triangular respectively, and their diagonal blocks 
!         with the same index are stored in a block of D with the same
!         index occupying  respectively lower and upper triangles of a
!         block in D. Unit diagonal elements of factor L are not stored. 
!
! DL (input) REAL*8 array, dimension (NB,(N-1)*NB)
!     The array stores subdiagonal blocks of lower triangular factor L 
!     as they are returned by DGEBLTTRF.
!      
! DU1 (input) REAL*8 array, dimension (NB,(N-1)*NB)
!     The array stores superdiagonal blocks of upper triangular factor U 
!     as they are returned by DGEBLTTRF.
!
! DU2 (input) REAL*8 array, dimension (NB,(N-2)*NB)
!     The array stores blocks of the second superdiagonal of upper 
!          triangular factor U as they are returned by DGEBLTTRF.
!
! IPIV (input) INTEGER array, dimension (NB,N)
!     The array stores pivot 'local' row indices ('local' means indices
!         vary in the range 1..2*NB. Global row index is 
!         IPIV(I,K) + (K-1)*NB ).
!
! F (input/output) REAL*8 array, dimension (LDF,NRHS)
!     On entry, the array stores NRHS columns of right hand F of the
!         system of linear equations AX = F.
!     On exit, the array stores NRHS columns of unknowns X of the system
!         of linear equations AX = F.
!
! LDF (input) INTEGER. LDF >= N*NB
!     Leading dimension of the array F
!
! INFO (output) INTEGER
!     = 0:        successful exit
!     < 0:        if INFO = -i, the i-th argument had an illegal value
! =====================================================================
      SUBROUTINE DGEBLTTRS(N, NB, NRHS, D, DL, DU1, DU2, IPIV, F, LDF,
     &                     INFO)
      IMPLICIT NONE
! ..Scalar arguments..
      INTEGER N, NB, NRHS, LDF, INFO 
! ..Array arguments..
      INTEGER IPIV(NB,*)
      REAL*8 D(NB,*), DL(NB,*), DU1(NB,*), DU2(NB,*), F(LDF,*)
! =====================================================================
! .. Local Scalars ..
      INTEGER K, I
! ..
! .. Executable Statements ..
! ..
!    Test the input arguments.
      INFO=0
      IF(N .LE. 0) THEN
          INFO = -1
      ELSE IF(NB .LE. 0) THEN
          INFO = -2
      ELSE IF(NRHS .LE. 0) THEN
          INFO = -3
      ELSE IF(LDF .LT. N*NB) THEN
          INFO = -10
      END IF
      IF(INFO .NE. 0) THEN
          RETURN
      END IF
!  
! Forward substitution
! In the loop compute components Y_K stored in array F
      DO K = 1, N-2
          DO I = 1, NB
              IF(IPIV(I,K) .NE. I)THEN
                  CALL DSWAP(NRHS, F((K-1)*NB+I,1), LDF,
     &                         F((K-1)*NB+IPIV(I,K),1), LDF)
              END IF
          END DO
          CALL DTRSM('L', 'L', 'N', 'U', NB, NRHS, 1D0, 
     &                    D(1,(K-1)*NB+1), NB, F((K-1)*NB+1,1), LDF)
          CALL DGEMM('N', 'N', NB, NRHS, NB, -1D0, DL(1,(K-1)*NB+1), NB,
     &                 F((K-1)*NB+1,1), LDF, 1D0, F(K*NB+1,1), LDF)
      END DO
!
! Apply two last pivots      
      DO I = 1, NB
           IF(IPIV(I,N-1) .NE. I)THEN
               CALL DSWAP(NRHS, F((N-2)*NB+I,1), LDF,
     &                         F((N-2)*NB+IPIV(I,N-1),1), LDF)
           END IF
      END DO

      DO I = 1, NB
           IF(IPIV(I,N) .NE. I + NB)THEN
               CALL DSWAP(NRHS, F((N-1)*NB+I,1), LDF,
     &                         F((N-2)*NB+IPIV(I,N),1), LDF)
           END IF
      END DO
!
! Computing components Y_N-1 and Y_N out of loop      
      CALL DTRSM('L', 'L', 'N', 'U', NB, NRHS, 1D0, 
     &                    D(1,(N-2)*NB+1), NB, F((N-2)*NB+1,1), LDF)
      
      CALL DGEMM('N', 'N', NB, NRHS, NB, -1D0, DL(1,(N-2)*NB +1), NB,
     &                 F((N-2)*NB+1,1), LDF, 1D0, F((N-1)*NB+1,1), LDF)
      CALL DTRSM('L', 'L', 'N', 'U', NB, NRHS, 1D0, 
     &                    D(1,(N-1)*NB+1), NB, F((N-1)*NB+1,1), LDF)
!
! Backward substitution      
! Computing X_N out of loop and store in array F
      CALL DTRSM('L', 'U', 'N', 'N', NB, NRHS, 1D0, 
     &                    D(1,(N-1)*NB+1), NB, F((N-1)*NB+1,1), LDF)
!
! Computing X_N-1 out of loop and store in array F
      CALL DGEMM('N', 'N', NB, NRHS, NB, -1D0, DU1(1,(N-2)*NB +1), NB,
     &                 F((N-1)*NB+1,1), LDF, 1D0, F((N-2)*NB+1,1), LDF)
      CALL DTRSM('L', 'U', 'N', 'N', NB, NRHS, 1D0, 
     &                    D(1,(N-2)*NB+1), NB, F((N-2)*NB+1,1), LDF)
!
! In the loop computing components X_K stored in array F
      DO K = N-2, 1, -1
          CALL DGEMM('N','N',NB, NRHS, NB, -1D0, DU1(1,(K-1)*NB +1), NB,
     &                 F(K*NB+1,1), LDF, 1D0, F((K-1)*NB+1,1), LDF)
          CALL DGEMM('N','N',NB, NRHS, NB, -1D0, DU2(1,(K-1)*NB +1), NB,
     &                 F((K+1)*NB+1,1), LDF, 1D0, F((K-1)*NB+1,1), LDF)
          CALL DTRSM('L', 'U', 'N', 'N', NB, NRHS, 1D0, 
     &                    D(1,(K-1)*NB+1), NB, F((K-1)*NB+1,1), LDF)
      END DO
      
      RETURN
      END
      
