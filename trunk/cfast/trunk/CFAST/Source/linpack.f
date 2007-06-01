#ifdef pp_double
      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)
      INTEGER LDA, N, IPVT(*), INFO
      DOUBLE PRECISION A(LDA,*)
C
C     dgefa factors a double precision matrix by gaussian elimination.
C
C     dgefa is usually called by dgeco, but it can be called
C     directly with a saving in time if  rcond  is not needed.
C     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
C
C     on entry
C
C        a       double precision(lda, n)
C                the matrix to be factored.
C
C        lda     integer
C                the leading dimension of the array  a .
C
C        n       integer
C                the order of the matrix  a .
C
C     on return
C
C        a       an upper triangular matrix and the multipliers
C                which were used to obtain it.
C                the factorization can be written  a = l*u  where
C                l  is a product of permutation and unit lower
C                triangular matrices and  u  is upper triangular.
C
C        ipvt    integer(n)
C                an integer vector of pivot indices.
C
C        info    integer
C                = 0  normal value.
C                = k  if  u(k,k) .eq. 0.0 .  this is not an error
C                     condition for this subroutine, but it does
C                     indicate that dgesl or dgedi will divide by zero
C                     if called.  use  rcond  in dgeco for a reliable
C                     indication of singularity.
C
C     linpack. this version dated 08/14/78 .
C     cleve moler, university of new mexico, argonne national lab.
C
C     subroutines and functions
C
C     blas daxpy,dscal,idamax
C
C     internal variables
C
      DOUBLE PRECISION T
      INTEGER IDAMAX, J, K, KP1, L, NM1
C
C
C     gaussian elimination with partial pivoting
C
      INFO = 0
      NM1 = N - 1
      IF (NM1.GE.1) THEN
        DO 20 K = 1, NM1
          KP1 = K + 1
C
C        find l = pivot index
C
          L = IDAMAX(N-K+1,A(K,K),1) + K - 1
          IPVT(K) = L
C
C        zero pivot implies this column already triangularized
C
          IF (A(L,K).NE.0.0D0) THEN
C
C           interchange if necessary
C
            IF (L.NE.K) THEN
              T = A(L,K)
              A(L,K) = A(K,K)
              A(K,K) = T
            END IF
C
C           compute multipliers
C
            T = -1.0D0 / A(K,K)
            CALL DSCAL(N-K,T,A(K+1,K),1)
C
C           row elimination with column indexing
C
            DO 10 J = KP1, N
              T = A(L,J)
              IF (L.NE.K) THEN
                A(L,J) = A(K,J)
                A(K,J) = T
              END IF
              CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   10       CONTINUE
          ELSE
            INFO = K
          END IF
   20   CONTINUE
      END IF
      IPVT(N) = N
      IF (A(N,N).EQ.0.0D0) INFO = N
      RETURN
      END
      SUBROUTINE DGESL(A,LDA,N,IPVT,B,JOB)
      INTEGER LDA, N, IPVT(*), JOB
      DOUBLE PRECISION A(LDA,*), B(*)
C
C     dgesl solves the double precision system
C     a * x = b  or  trans(a) * x = b
C     using the factors computed by dgeco or dgefa.
C
C     on entry
C
C        a       double precision(lda, n)
C                the output from dgeco or dgefa.
C
C        lda     integer
C                the leading dimension of the array  a .
C
C        n       integer
C                the order of the matrix  a .
C
C        ipvt    integer(n)
C                the pivot vector from dgeco or dgefa.
C
C        b       double precision(n)
C                the right hand side vector.
C
C        job     integer
C                = 0         to solve  a*x = b ,
C                = nonzero   to solve  trans(a)*x = b  where
C                            trans(a)  is the transpose.
C
C     on return
C
C        b       the solution vector  x .
C
C     error condition
C
C        a division by zero will occur if the input factor contains a
C        zero on the diagonal.  technically this indicates singularity
C        but it is often caused by improper arguments or improper
C        setting of lda .  it will not occur if the subroutines are
C        called correctly and if dgeco has set rcond .gt. 0.0
C        or dgefa has set info .eq. 0 .
C
C     to compute  inverse(a) * c  where  c  is a matrix
C     with  p  columns
C           call dgeco(a,lda,n,ipvt,rcond,z)
C           if (rcond is too small) go to ...
C           do 10 j = 1, p
C              call dgesl(a,lda,n,ipvt,c(1,j),0)
C        10 continue
C
C     linpack. this version dated 08/14/78 .
C     cleve moler, university of new mexico, argonne national lab.
C
C     subroutines and functions
C
C     blas daxpy,ddot
C
C     internal variables
C
      DOUBLE PRECISION DDOT, T
      INTEGER K, KB, L, NM1
C
      NM1 = N - 1
      IF (JOB.EQ.0) THEN
C
C        job = 0 , solve  a * x = b
C        first solve  l*y = b
C
        IF (NM1.GE.1) THEN
          DO 10 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L.NE.K) THEN
              B(L) = B(K)
              B(K) = T
            END IF
            CALL DAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   10     CONTINUE
        END IF
C
C        now solve  u*x = y
C
        DO 20 KB = 1, N
          K = N + 1 - KB
          B(K) = B(K) / A(K,K)
          T = -B(K)
          CALL DAXPY(K-1,T,A(1,K),1,B(1),1)
   20   CONTINUE
      ELSE
C
C        job = nonzero, solve  trans(a) * x = b
C        first solve  trans(u)*y = b
C
        DO 30 K = 1, N
          T = DDOT(K-1,A(1,K),1,B(1),1)
          B(K) = (B(K)-T) / A(K,K)
   30   CONTINUE
C
C        now solve trans(l)*x = y
C
        IF (NM1.GE.1) THEN
          DO 40 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + DDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L.NE.K) THEN
              T = B(L)
              B(L) = B(K)
              B(K) = T
            END IF
   40     CONTINUE
        END IF
      END IF
      RETURN
      END
      SUBROUTINE DGBFA(ABD,LDA,N,ML,MU,IPVT,INFO)
      INTEGER LDA, N, ML, MU, IPVT(*), INFO
      DOUBLE PRECISION ABD(LDA,*)
C
C     dgbfa factors a double precision band matrix by elimination.
C
C     dgbfa is usually called by dgbco, but it can be called
C     directly with a saving in time if  rcond  is not needed.
C
C     on entry
C
C        abd     double precision(lda, n)
C                contains the matrix in band storage.  the columns
C                of the matrix are stored in the columns of  abd  and
C                the diagonals of the matrix are stored in rows
C                ml+1 through 2*ml+mu+1 of  abd .
C                see the comments below for details.
C
C        lda     integer
C                the leading dimension of the array  abd .
C                lda must be .ge. 2*ml + mu + 1 .
C
C        n       integer
C                the order of the original matrix.
C
C        ml      integer
C                number of diagonals below the main diagonal.
C                0 .le. ml .lt. n .
C
C        mu      integer
C                number of diagonals above the main diagonal.
C                0 .le. mu .lt. n .
C                more efficient if  ml .le. mu .
C     on return
C
C        abd     an upper triangular matrix in band storage and
C                the multipliers which were used to obtain it.
C                the factorization can be written  a = l*u  where
C                l  is a product of permutation and unit lower
C                triangular matrices and  u  is upper triangular.
C
C        ipvt    integer(n)
C                an integer vector of pivot indices.
C
C        info    integer
C                = 0  normal value.
C                = k  if  u(k,k) .eq. 0.0 .  this is not an error
C                     condition for this subroutine, but it does
C                     indicate that dgbsl will divide by zero if
C                     called.  use  rcond  in dgbco for a reliable
C                     indication of singularity.
C
C     band storage
C
C           if  a  is a band matrix, the following program segment
C           will set up the input.
C
C                   ml = (band width below the diagonal)
C                   mu = (band width above the diagonal)
C                   m = ml + mu + 1
C                   do 20 j = 1, n
C                      i1 = max0(1, j-mu)
C                      i2 = min0(n, j+ml)
C                      do 10 i = i1, i2
C                         k = i - j + m
C                         abd(k,j) = a(i,j)
C                10    continue
C                20 continue
C
C           this uses rows  ml+1  through  2*ml+mu+1  of  abd .
C           in addition, the first  ml  rows in  abd  are used for
C           elements generated during the triangularization.
C           the total number of rows needed in  abd  is  2*ml+mu+1 .
C           the  ml+mu by ml+mu  upper left triangle and the
C           ml by ml  lower right triangle are not referenced.
C
C     linpack. this version dated 08/14/78 .
C     cleve moler, university of new mexico, argonne national lab.
C
C     subroutines and functions
C
C     blas daxpy,dscal,idamax
C     fortran max0,min0
C
C     internal variables
C
      DOUBLE PRECISION T
      INTEGER I, IDAMAX, I0, J, JU, JZ, J0, J1, K, KP1, L, LM, M, MM, 
     +    NM1
C
C
      M = ML + MU + 1
      INFO = 0
C
C     zero initial fill-in columns
C
      J0 = MU + 2
      J1 = MIN0(N,M) - 1
      IF (J1.GE.J0) THEN
        DO 20 JZ = J0, J1
          I0 = M + 1 - JZ
          DO 10 I = I0, ML
            ABD(I,JZ) = 0.0D0
   10     CONTINUE
   20   CONTINUE
      END IF
      JZ = J1
      JU = 0
C
C     gaussian elimination with partial pivoting
C
      NM1 = N - 1
      IF (NM1.GE.1) THEN
        DO 50 K = 1, NM1
          KP1 = K + 1
C
C        zero next fill-in column
C
          JZ = JZ + 1
          IF (JZ.LE.N) THEN
            IF (ML.GE.1) THEN
              DO 30 I = 1, ML
                ABD(I,JZ) = 0.0D0
   30         CONTINUE
            END IF
          END IF
C
C        find l = pivot index
C
          LM = MIN0(ML,N-K)
          L = IDAMAX(LM+1,ABD(M,K),1) + M - 1
          IPVT(K) = L + K - M
C
C        zero pivot implies this column already triangularized
C
          IF (ABD(L,K).NE.0.0D0) THEN
C
C           interchange if necessary
C
            IF (L.NE.M) THEN
              T = ABD(L,K)
              ABD(L,K) = ABD(M,K)
              ABD(M,K) = T
            END IF
C
C           compute multipliers
C
            T = -1.0D0 / ABD(M,K)
            CALL DSCAL(LM,T,ABD(M+1,K),1)
C
C           row elimination with column indexing
C
            JU = MIN0(MAX0(JU,MU+IPVT(K)),N)
            MM = M
            IF (JU.GE.KP1) THEN
              DO 40 J = KP1, JU
                L = L - 1
                MM = MM - 1
                T = ABD(L,J)
                IF (L.NE.MM) THEN
                  ABD(L,J) = ABD(MM,J)
                  ABD(MM,J) = T
                END IF
                CALL DAXPY(LM,T,ABD(M+1,K),1,ABD(MM+1,J),1)
   40         CONTINUE
            END IF
          ELSE
            INFO = K
          END IF
   50   CONTINUE
      END IF
      IPVT(N) = N
      IF (ABD(M,N).EQ.0.0D0) INFO = N
      RETURN
      END
      SUBROUTINE DGBSL(ABD,LDA,N,ML,MU,IPVT,B,JOB)
      INTEGER LDA, N, ML, MU, IPVT(*), JOB
      DOUBLE PRECISION ABD(LDA,*), B(*)
C
C     dgbsl solves the double precision band system
C     a * x = b  or  trans(a) * x = b
C     using the factors computed by dgbco or dgbfa.
C
C     on entry
C
C        abd     double precision(lda, n)
C                the output from dgbco or dgbfa.
C
C        lda     integer
C                the leading dimension of the array  abd .
C
C        n       integer
C                the order of the original matrix.
C
C        ml      integer
C                number of diagonals below the main diagonal.
C
C        mu      integer
C                number of diagonals above the main diagonal.
C
C        ipvt    integer(n)
C                the pivot vector from dgbco or dgbfa.
C
C        b       double precision(n)
C                the right hand side vector.
C
C        job     integer
C                = 0         to solve  a*x = b ,
C                = nonzero   to solve  trans(a)*x = b , where
C                            trans(a)  is the transpose.
C
C     on return
C
C        b       the solution vector  x .
C
C     error condition
C
C        a division by zero will occur if the input factor contains a
C        zero on the diagonal.  technically this indicates singularity
C        but it is often caused by improper arguments or improper
C        setting of lda .  it will not occur if the subroutines are
C        called correctly and if dgbco has set rcond .gt. 0.0
C        or dgbfa has set info .eq. 0 .
C
C     to compute  inverse(a) * c  where  c  is a matrix
C     with  p  columns
C           call dgbco(abd,lda,n,ml,mu,ipvt,rcond,z)
C           if (rcond is too small) go to ...
C           do 10 j = 1, p
C              call dgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0)
C        10 continue
C
C     linpack. this version dated 08/14/78 .
C     cleve moler, university of new mexico, argonne national lab.
C
C     subroutines and functions
C
C     blas daxpy,ddot
C     fortran min0
C
C     internal variables
C
      DOUBLE PRECISION DDOT, T
      INTEGER K, KB, L, LA, LB, LM, M, NM1
C
      M = MU + ML + 1
      NM1 = N - 1
      IF (JOB.EQ.0) THEN
C
C        job = 0 , solve  a * x = b
C        first solve l*y = b
C
        IF (ML.NE.0) THEN
          IF (NM1.GE.1) THEN
            DO 10 K = 1, NM1
              LM = MIN0(ML,N-K)
              L = IPVT(K)
              T = B(L)
              IF (L.NE.K) THEN
                B(L) = B(K)
                B(K) = T
              END IF
              CALL DAXPY(LM,T,ABD(M+1,K),1,B(K+1),1)
   10       CONTINUE
          END IF
        END IF
C
C        now solve  u*x = y
C
        DO 20 KB = 1, N
          K = N + 1 - KB
          B(K) = B(K) / ABD(M,K)
          LM = MIN0(K,M) - 1
          LA = M - LM
          LB = K - LM
          T = -B(K)
          CALL DAXPY(LM,T,ABD(LA,K),1,B(LB),1)
   20   CONTINUE
      ELSE
C
C        job = nonzero, solve  trans(a) * x = b
C        first solve  trans(u)*y = b
C
        DO 30 K = 1, N
          LM = MIN0(K,M) - 1
          LA = M - LM
          LB = K - LM
          T = DDOT(LM,ABD(LA,K),1,B(LB),1)
          B(K) = (B(K)-T) / ABD(M,K)
   30   CONTINUE
C
C        now solve trans(l)*x = y
C
        IF (ML.NE.0) THEN
          IF (NM1.GE.1) THEN
            DO 40 KB = 1, NM1
              K = N - KB
              LM = MIN0(ML,N-K)
              B(K) = B(K) + DDOT(LM,ABD(M+1,K),1,B(K+1),1)
              L = IPVT(K)
              IF (L.NE.K) THEN
                T = B(L)
                B(L) = B(K)
                B(K) = T
              END IF
   40       CONTINUE
          END IF
        END IF
      END IF
      RETURN
      END
#else
      SUBROUTINE SGEFA(A,LDA,N,IPVT,INFO)
      INTEGER LDA, N, IPVT(*), INFO
      REAL A(LDA,*)
C
C     sgefa factors a real matrix by gaussian elimination.
C
C     sgefa is usually called by sgeco, but it can be called
C     directly with a saving in time if  rcond  is not needed.
C     (time for sgeco) = (1 + 9/n)*(time for sgefa) .
C
C     on entry
C
C        a       real(lda, n)
C                the matrix to be factored.
C
C        lda     integer
C                the leading dimension of the array  a .
C
C        n       integer
C                the order of the matrix  a .
C
C     on return
C
C        a       an upper triangular matrix and the multipliers
C                which were used to obtain it.
C                the factorization can be written  a = l*u  where
C                l  is a product of permutation and unit lower
C                triangular matrices and  u  is upper triangular.
C
C        ipvt    integer(n)
C                an integer vector of pivot indices.
C
C        info    integer
C                = 0  normal value.
C                = k  if  u(k,k) .eq. 0.0 .  this is not an error
C                     condition for this subroutine, but it does
C                     indicate that sgesl or sgedi will divide by zero
C                     if called.  use  rcond  in sgeco for a reliable
C                     indication of singularity.
C
C     linpack. this version dated 08/14/78 .
C     cleve moler, university of new mexico, argonne national lab.
C
C     subroutines and functions
C
C     blas saxpy,sscal,isamax
C
C     internal variables
C
      REAL T
      INTEGER ISAMAX, J, K, KP1, L, NM1
C
C
C     gaussian elimination with partial pivoting
C
      INFO = 0
      NM1 = N - 1
      IF (NM1.GE.1) THEN
        DO 20 K = 1, NM1
          KP1 = K + 1
C
C        find l = pivot index
C
          L = ISAMAX(N-K+1,A(K,K),1) + K - 1
          IPVT(K) = L
C
C        zero pivot implies this column already triangularized
C
          IF (A(L,K).NE.0.0E0) THEN
C
C           interchange if necessary
C
            IF (L.NE.K) THEN
              T = A(L,K)
              A(L,K) = A(K,K)
              A(K,K) = T
            END IF
C
C           compute multipliers
C
            T = -1.0E0 / A(K,K)
            CALL SSCAL(N-K,T,A(K+1,K),1)
C
C           row elimination with column indexing
C
            DO 10 J = KP1, N
              T = A(L,J)
              IF (L.NE.K) THEN
                A(L,J) = A(K,J)
                A(K,J) = T
              END IF
              CALL SAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   10       CONTINUE
          ELSE
            INFO = K
          END IF
   20   CONTINUE
      END IF
      IPVT(N) = N
      IF (A(N,N).EQ.0.0E0) INFO = N
      RETURN
      END
      SUBROUTINE SGESL(A,LDA,N,IPVT,B,JOB)
      INTEGER LDA, N, IPVT(*), JOB
      REAL A(LDA,*), B(*)
C
C     sgesl solves the real system
C     a * x = b  or  trans(a) * x = b
C     using the factors computed by sgeco or sgefa.
C
C     on entry
C
C        a       real(lda, n)
C                the output from sgeco or sgefa.
C
C        lda     integer
C                the leading dimension of the array  a .
C
C        n       integer
C                the order of the matrix  a .
C
C        ipvt    integer(n)
C                the pivot vector from sgeco or sgefa.
C
C        b       real(n)
C                the right hand side vector.
C
C        job     integer
C                = 0         to solve  a*x = b ,
C                = nonzero   to solve  trans(a)*x = b  where
C                            trans(a)  is the transpose.
C
C     on return
C
C        b       the solution vector  x .
C
C     error condition
C
C        a division by zero will occur if the input factor contains a
C        zero on the diagonal.  technically this indicates singularity
C        but it is often caused by improper arguments or improper
C        setting of lda .  it will not occur if the subroutines are
C        called correctly and if sgeco has set rcond .gt. 0.0
C        or sgefa has set info .eq. 0 .
C
C     to compute  inverse(a) * c  where  c  is a matrix
C     with  p  columns
C           call sgeco(a,lda,n,ipvt,rcond,z)
C           if (rcond is too small) go to ...
C           do 10 j = 1, p
C              call sgesl(a,lda,n,ipvt,c(1,j),0)
C        10 continue
C
C     linpack. this version dated 08/14/78 .
C     cleve moler, university of new mexico, argonne national lab.
C
C     subroutines and functions
C
C     blas saxpy,sdot
C
C     internal variables
C
      REAL SDOT, T
      INTEGER K, KB, L, NM1
C
      NM1 = N - 1
      IF (JOB.EQ.0) THEN
C
C        job = 0 , solve  a * x = b
C        first solve  l*y = b
C
        IF (NM1.GE.1) THEN
          DO 10 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L.NE.K) THEN
              B(L) = B(K)
              B(K) = T
            END IF
            CALL SAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   10     CONTINUE
        END IF
C
C        now solve  u*x = y
C
        DO 20 KB = 1, N
          K = N + 1 - KB
          B(K) = B(K) / A(K,K)
          T = -B(K)
          CALL SAXPY(K-1,T,A(1,K),1,B(1),1)
   20   CONTINUE
      ELSE
C
C        job = nonzero, solve  trans(a) * x = b
C        first solve  trans(u)*y = b
C
        DO 30 K = 1, N
          T = SDOT(K-1,A(1,K),1,B(1),1)
          B(K) = (B(K)-T) / A(K,K)
   30   CONTINUE
C
C        now solve trans(l)*x = y
C
        IF (NM1.GE.1) THEN
          DO 40 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + SDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L.NE.K) THEN
              T = B(L)
              B(L) = B(K)
              B(K) = T
            END IF
   40     CONTINUE
        END IF
      END IF
      RETURN
      END
      SUBROUTINE SGBFA(ABD,LDA,N,ML,MU,IPVT,INFO)
      INTEGER LDA, N, ML, MU, IPVT(*), INFO
      REAL ABD(LDA,*)
C
C     sgbfa factors a real band matrix by elimination.
C
C     sgbfa is usually called by sgbco, but it can be called
C     directly with a saving in time if  rcond  is not needed.
C
C     on entry
C
C        abd     real(lda, n)
C                contains the matrix in band storage.  the columns
C                of the matrix are stored in the columns of  abd  and
C                the diagonals of the matrix are stored in rows
C                ml+1 through 2*ml+mu+1 of  abd .
C                see the comments below for details.
C
C        lda     integer
C                the leading dimension of the array  abd .
C                lda must be .ge. 2*ml + mu + 1 .
C
C        n       integer
C                the order of the original matrix.
C
C        ml      integer
C                number of diagonals below the main diagonal.
C                0 .le. ml .lt. n .
C
C        mu      integer
C                number of diagonals above the main diagonal.
C                0 .le. mu .lt. n .
C                more efficient if  ml .le. mu .
C     on return
C
C        abd     an upper triangular matrix in band storage and
C                the multipliers which were used to obtain it.
C                the factorization can be written  a = l*u  where
C                l  is a product of permutation and unit lower
C                triangular matrices and  u  is upper triangular.
C
C        ipvt    integer(n)
C                an integer vector of pivot indices.
C
C        info    integer
C                = 0  normal value.
C                = k  if  u(k,k) .eq. 0.0 .  this is not an error
C                     condition for this subroutine, but it does
C                     indicate that sgbsl will divide by zero if
C                     called.  use  rcond  in sgbco for a reliable
C                     indication of singularity.
C
C     band storage
C
C           if  a  is a band matrix, the following program segment
C           will set up the input.
C
C                   ml = (band width below the diagonal)
C                   mu = (band width above the diagonal)
C                   m = ml + mu + 1
C                   do 20 j = 1, n
C                      i1 = max0(1, j-mu)
C                      i2 = min0(n, j+ml)
C                      do 10 i = i1, i2
C                         k = i - j + m
C                         abd(k,j) = a(i,j)
C                10    continue
C                20 continue
C
C           this uses rows  ml+1  through  2*ml+mu+1  of  abd .
C           in addition, the first  ml  rows in  abd  are used for
C           elements generated during the triangularization.
C           the total number of rows needed in  abd  is  2*ml+mu+1 .
C           the  ml+mu by ml+mu  upper left triangle and the
C           ml by ml  lower right triangle are not referenced.
C
C     linpack. this version dated 08/14/78 .
C     cleve moler, university of new mexico, argonne national lab.
C
C     subroutines and functions
C
C     blas saxpy,sscal,isamax
C     fortran max0,min0
C
C     internal variables
C
      REAL T
      INTEGER I, ISAMAX, I0, J, JU, JZ, J0, J1, K, KP1, L, LM, M, MM, 
     +    NM1
C
C
      M = ML + MU + 1
      INFO = 0
C
C     zero initial fill-in columns
C
      J0 = MU + 2
      J1 = MIN0(N,M) - 1
      IF (J1.GE.J0) THEN
        DO 20 JZ = J0, J1
          I0 = M + 1 - JZ
          DO 10 I = I0, ML
            ABD(I,JZ) = 0.0E0
   10     CONTINUE
   20   CONTINUE
      END IF
      JZ = J1
      JU = 0
C
C     gaussian elimination with partial pivoting
C
      NM1 = N - 1
      IF (NM1.GE.1) THEN
        DO 50 K = 1, NM1
          KP1 = K + 1
C
C        zero next fill-in column
C
          JZ = JZ + 1
          IF (JZ.LE.N) THEN
            IF (ML.GE.1) THEN
              DO 30 I = 1, ML
                ABD(I,JZ) = 0.0E0
   30         CONTINUE
            END IF
          END IF
C
C        find l = pivot index
C
          LM = MIN0(ML,N-K)
          L = ISAMAX(LM+1,ABD(M,K),1) + M - 1
          IPVT(K) = L + K - M
C
C        zero pivot implies this column already triangularized
C
          IF (ABD(L,K).NE.0.0E0) THEN
C
C           interchange if necessary
C
            IF (L.NE.M) THEN
              T = ABD(L,K)
              ABD(L,K) = ABD(M,K)
              ABD(M,K) = T
            END IF
C
C           compute multipliers
C
            T = -1.0E0 / ABD(M,K)
            CALL SSCAL(LM,T,ABD(M+1,K),1)
C
C           row elimination with column indexing
C
            JU = MIN0(MAX0(JU,MU+IPVT(K)),N)
            MM = M
            IF (JU.GE.KP1) THEN
              DO 40 J = KP1, JU
                L = L - 1
                MM = MM - 1
                T = ABD(L,J)
                IF (L.NE.MM) THEN
                  ABD(L,J) = ABD(MM,J)
                  ABD(MM,J) = T
                END IF
                CALL SAXPY(LM,T,ABD(M+1,K),1,ABD(MM+1,J),1)
   40         CONTINUE
            END IF
          ELSE
            INFO = K
          END IF
   50   CONTINUE
      END IF
      IPVT(N) = N
      IF (ABD(M,N).EQ.0.0E0) INFO = N
      RETURN
      END
      SUBROUTINE SGBSL(ABD,LDA,N,ML,MU,IPVT,B,JOB)
      INTEGER LDA, N, ML, MU, IPVT(*), JOB
      REAL ABD(LDA,*), B(*)
C
C     sgbsl solves the real band system
C     a * x = b  or  trans(a) * x = b
C     using the factors computed by sgbco or sgbfa.
C
C     on entry
C
C        abd     real(lda, n)
C                the output from sgbco or sgbfa.
C
C        lda     integer
C                the leading dimension of the array  abd .
C
C        n       integer
C                the order of the original matrix.
C
C        ml      integer
C                number of diagonals below the main diagonal.
C
C        mu      integer
C                number of diagonals above the main diagonal.
C
C        ipvt    integer(n)
C                the pivot vector from sgbco or sgbfa.
C
C        b       real(n)
C                the right hand side vector.
C
C        job     integer
C                = 0         to solve  a*x = b ,
C                = nonzero   to solve  trans(a)*x = b , where
C                            trans(a)  is the transpose.
C
C     on return
C
C        b       the solution vector  x .
C
C     error condition
C
C        a division by zero will occur if the input factor contains a
C        zero on the diagonal.  technically this indicates singularity
C        but it is often caused by improper arguments or improper
C        setting of lda .  it will not occur if the subroutines are
C        called correctly and if sgbco has set rcond .gt. 0.0
C        or sgbfa has set info .eq. 0 .
C
C     to compute  inverse(a) * c  where  c  is a matrix
C     with  p  columns
C           call sgbco(abd,lda,n,ml,mu,ipvt,rcond,z)
C           if (rcond is too small) go to ...
C           do 10 j = 1, p
C              call sgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0)
C        10 continue
C
C     linpack. this version dated 08/14/78 .
C     cleve moler, university of new mexico, argonne national lab.
C
C     subroutines and functions
C
C     blas saxpy,sdot
C     fortran min0
C
C     internal variables
C
      REAL SDOT, T
      INTEGER K, KB, L, LA, LB, LM, M, NM1
C
      M = MU + ML + 1
      NM1 = N - 1
      IF (JOB.EQ.0) THEN
C
C        job = 0 , solve  a * x = b
C        first solve l*y = b
C
        IF (ML.NE.0) THEN
          IF (NM1.GE.1) THEN
            DO 10 K = 1, NM1
              LM = MIN0(ML,N-K)
              L = IPVT(K)
              T = B(L)
              IF (L.NE.K) THEN
                B(L) = B(K)
                B(K) = T
              END IF
              CALL SAXPY(LM,T,ABD(M+1,K),1,B(K+1),1)
   10       CONTINUE
          END IF
        END IF
C
C        now solve  u*x = y
C
        DO 20 KB = 1, N
          K = N + 1 - KB
          B(K) = B(K) / ABD(M,K)
          LM = MIN0(K,M) - 1
          LA = M - LM
          LB = K - LM
          T = -B(K)
          CALL SAXPY(LM,T,ABD(LA,K),1,B(LB),1)
   20   CONTINUE
      ELSE
C
C        job = nonzero, solve  trans(a) * x = b
C        first solve  trans(u)*y = b
C
        DO 30 K = 1, N
          LM = MIN0(K,M) - 1
          LA = M - LM
          LB = K - LM
          T = SDOT(LM,ABD(LA,K),1,B(LB),1)
          B(K) = (B(K)-T) / ABD(M,K)
   30   CONTINUE
C
C        now solve trans(l)*x = y
C
        IF (ML.NE.0) THEN
          IF (NM1.GE.1) THEN
            DO 40 KB = 1, NM1
              K = N - KB
              LM = MIN0(ML,N-K)
              B(K) = B(K) + SDOT(LM,ABD(M+1,K),1,B(K+1),1)
              L = IPVT(K)
              IF (L.NE.K) THEN
                T = B(L)
                B(L) = B(K)
                B(K) = T
              END IF
   40       CONTINUE
          END IF
        END IF
      END IF
      RETURN
      END
#endif
C
