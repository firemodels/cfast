      SUBROUTINE DDASSL(RES,NEQ,T,Y,YPRIME,TOUT,INFO,RTOL,ATOL,IDID,
     *   RWORK,LRW,IWORK,LIW,RPAR,IPAR,JAC)
C
C***BEGIN PROLOGUE  DDASSL
C***DATE WRITTEN   830315   (YYMMDD)
C***REVISION DATE  891228   (YYMMDD)
C***CATEGORY NO.  D2A2
C***KEYWORDS  DIFFERENTIAL/ALGEBRAIC,BACKWARD DIFFERENTIATION FORMULAS
C             IMPLICIT DIFFERENTIAL SYSTEMS
C***AUTHOR  PETZOLD,LINDA R.,COMPUTING AND MATHEMATICS RESEARCH DIVISION
C             LAWRENCE LIVERMORE NATIONAL LABORATORY
C             L - 316, P.O. Box 808,
C             LIVERMORE, CA.    94550
C***PURPOSE  This code solves a system of differential/algebraic
C            equations of the form G(T,Y,YPRIME) = 0.
C***DESCRIPTION
C
C *Usage:
C
C      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C      EXTERNAL RES, JAC
C      INTEGER NEQ, INFO(N), IDID, LRW, LIW, IWORK(LIW), IPAR
C      DOUBLE PRECISION T, Y(NEQ), YPRIME(NEQ), TOUT, RTOL, ATOL,
C     *   RWORK(LRW), RPAR
C
C      CALL DDASSL (RES, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL, ATOL,
C     *   IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR, JAC)
C
C
C
C *Arguments:
C
C  RES:EXT  This is a subroutine which you provide to define the
C           differential/algebraic system.
C
C  NEQ:IN  This is the number of equations to be solved.
C
C  T:INOUT  This is the current value of the independent variable.
C
C  Y(*):INOUT  This array contains the solution components at T.
C
C  YPRIME(*):INOUT  This array contains the derivatives of the solution
C                   components at T.
C
C  TOUT:IN  This is a point at which a solution is desired.
C
C  INFO(N):IN  The basic task of the code is to solve the system from T
C              to TOUT and return an answer at TOUT.  INFO is an integer
C              array which is used to communicate exactly how you want
C              this task to be carried out.  N must be greater than or
C              equal to 15.
C
C  RTOL,ATOL:INOUT  These quantities represent absolute and relative
C                   error tolerances which you provide to indicate how
C                   accurately you wish the solution to be computed.
C                   You may choose them to be both scalars or else
C                   both vectors.
C
C  IDID:OUT  This scalar quantity is an indicator reporting what the
C            code did.  You must monitor this integer variable to decide
C            what action to take next.
C
C  RWORK:WORK  A real work array of length LRW which provides the
C               code with needed storage space.
C
C  LRW:IN  The length of RWORK.
C
C  IWORK:WORK  An integer work array of length LIW which probides the
C               code with needed storage space.
C
C  LIW:IN  The length of IWORK.
C
C  RPAR,IPAR:IN  These are real and integer parameter arrays which
C                you can use for communication between your calling
C                program and the RES subroutine (and the JAC subroutine)
C
C  JAC:EXT  This is the name of a subroutine which you may choose to
C           provide for defining a matrix of partial derivatives
C           described below.
C
C
C *Description
C  QUANTITIES WHICH MAY BE ALTERED BY THE CODE ARE
C     T,Y(*),YPRIME(*),INFO(1),RTOL,ATOL,
C     IDID,RWORK(*) AND IWORK(*)
C
C  Subroutine DDASSL uses the backward differentiation formulas of
C  orders one through five to solve a system of the above form for Y and
C  YPRIME.  Values for Y and YPRIME at the initial time must be given as
C  input.  These values must be consistent, (that is, if T,Y,YPRIME are
C  the given initial values, they must satisfy G(T,Y,YPRIME) = 0.).  The
C  subroutine solves the system from T to TOUT.  It is easy to continue
C  the solution to get results at additional TOUT.  This is the interval
C  mode of operation.  Intermediate results can also be obtained easily
C  by using the intermediate-output capability.
C
C  ----------INPUT-WHAT TO DO ON THE FIRST CALL TO DDASSL---------------
C
C
C  The first call of the code is defined to be the start of each new
C  problem. Read through the descriptions of all the following items,
C  provide sufficient storage space for designated arrays, set
C  appropriate variables for the initialization of the problem, and
C  give information about how you want the problem to be solved.
C
C
C  RES -- Provide a subroutine of the form
C             SUBROUTINE RES(T,Y,YPRIME,DELTA,IRES,RPAR,IPAR)
C         to define the system of differential/algebraic
C         equations which is to be solved. For the given values
C         of T,Y and YPRIME, the subroutine should
C         return the residual of the defferential/algebraic
C         system
C             DELTA = G(T,Y,YPRIME)
C         (DELTA(*) is a vector of length NEQ which is
C         output for RES.)
C
C         Subroutine RES must not alter T,Y or YPRIME.
C         You must declare the name RES in an external
C         statement in your program that calls DDASSL.
C         You must dimension Y,YPRIME and DELTA in RES.
C
C         IRES is an integer flag which is always equal to
C         zero on input. Subroutine RES should alter IRES
C         only if it encounters an illegal value of Y or
C         a stop condition. Set IRES = -1 if an input value
C         is illegal, and DDASSL will try to solve the problem
C         without getting IRES = -1. If IRES = -2, DDASSL
C         will return control to the calling program
C         with IDID = -11.
C
C         RPAR and IPAR are real and integer parameter arrays which
C         you can use for communication between your calling program
C         and subroutine RES. They are not altered by DDASSL. If you
C         do not need RPAR or IPAR, ignore these parameters by treat-
C         ing them as dummy arguments. If you do choose to use them,
C         dimension them in your calling program and in RES as arrays
C         of appropriate length.
C
C  NEQ -- Set it to the number of differential equations.
C         (NEQ .GE. 1)
C
C  T -- Set it to the initial point of the integration.
C       T must be defined as a variable.
C
C  Y(*) -- Set this vector to the initial values of the NEQ solution
C          components at the initial point. You must dimension Y of
C          length at least NEQ in your calling program.
C
C  YPRIME(*) -- Set this vector to the initial values of
C               the NEQ first derivatives of the solution
C               components at the initial point. You
C               must dimension YPRIME at least NEQ
C               in your calling program. If you do not
C               know initial values of some of the solution
C               components, see the explanation of INFO(11).
C
C  TOUT - Set it to the first point at which a solution
C         is desired. You can not take TOUT = T.
C         integration either forward in T (TOUT .GT. T) or
C         backward in T (TOUT .LT. T) is permitted.
C
C         The code advances the solution from T to TOUT using
C         step sizes which are automatically selected so as to
C         achieve the desired accuracy. If you wish, the code will
C         return with the solution and its derivative at
C         intermediate steps (intermediate-output mode) so that
C         you can monitor them, but you still must provide TOUT in
C         accord with the basic aim of the code.
C
C         the first step taken by the code is a critical one
C         because it must reflect how fast the solution changes near
C         the initial point. The code automatically selects an
C         initial step size which is practically always suitable for
C         the problem. By using the fact that the code will not step
C         past TOUT in the first step, you could, if necessary,
C         restrict the length of the initial step size.
C
C         For some problems it may not be permissable to integrate
C         past a point TSTOP because a discontinuity occurs there
C         or the solution or its derivative is not defined beyond
C         TSTOP. When you have declared a TSTOP point (SEE INFO(4)
C         and RWORK(1)), you have told the code not to integrate
C         past TSTOP. In this case any TOUT beyond TSTOP is invalid
C         input.
C
C  INFO(*) - Use the INFO array to give the code more details about
C            how you want your problem solved. This array should be
C            dimensioned of length 15, though DDASSL uses
C            only the first eleven entries. You must respond to all of
C            the following items which are arranged as questions. The
C            simplest use of the code corresponds to answering all
C            questions as yes, i.e. setting all entries of INFO to 0.
C
C       INFO(1) - This parameter enables the code to initialize
C              itself. You must set it to indicate the start of every
C              new problem.
C
C          **** Is this the first call for this problem ...
C                Yes - Set INFO(1) = 0
C                 No - Not applicable here.
C                      See below for continuation calls.  ****
C
C       INFO(2) - How much accuracy you want of your solution
C              is specified by the error tolerances RTOL and ATOL.
C              The simplest use is to take them both to be scalars.
C              To obtain more flexibility, they can both be vectors.
C              The code must be told your choice.
C
C          **** Are both error tolerances RTOL, ATOL scalars ...
C                Yes - Set INFO(2) = 0
C                      and input scalars for both RTOL and ATOL
C                 No - Set INFO(2) = 1
C                      and input arrays for both RTOL and ATOL ****
C
C       INFO(3) - The code integrates from T in the direction
C              of TOUT by steps. If you wish, it will return the
C              computed solution and derivative at the next
C              intermediate step (the intermediate-output mode) or
C              TOUT, whichever comes first. This is a good way to
C              proceed if you want to see the behavior of the solution.
C              If you must have solutions at a great many specific
C              TOUT points, this code will compute them efficiently.
C
C          **** Do you want the solution only at
C                TOUT (and not at the next intermediate step) ...
C                 Yes - Set INFO(3) = 0
C                  No - Set INFO(3) = 1 ****
C
C       INFO(4) - To handle solutions at a great many specific
C              values TOUT efficiently, this code may integrate past
C              TOUT and interpolate to obtain the result at TOUT.
C              Sometimes it is not possible to integrate beyond some
C              point TSTOP because the equation changes there or it is
C              not defined past TSTOP. Then you must tell the code
C              not to go past.
C
C           **** Can the integration be carried out without any
C                restrictions on the independent variable T ...
C                 Yes - Set INFO(4)=0
C                  No - Set INFO(4)=1
C                       and define the stopping point TSTOP by
C                       setting RWORK(1)=TSTOP ****
C
C       INFO(5) - To solve differential/algebraic problems it is
C              necessary to use a matrix of partial derivatives of the
C              system of differential equations. If you do not
C              provide a subroutine to evaluate it analytically (see
C              description of the item JAC in the call list), it will
C              be approximated by numerical differencing in this code.
C              although it is less trouble for you to have the code
C              compute partial derivatives by numerical differencing,
C              the solution will be more reliable if you provide the
C              derivatives via JAC. Sometimes numerical differencing
C              is cheaper than evaluating derivatives in JAC and
C              sometimes it is not - this depends on your problem.
C
C           **** Do you want the code to evaluate the partial
C                derivatives automatically by numerical differences ...
C                   Yes - Set INFO(5)=0
C                    No - Set INFO(5)=1
C                  and provide subroutine JAC for evaluating the
C                  matrix of partial derivatives ****
C
C       INFO(6) - DDASSL will perform much better if the matrix of
C              partial derivatives, DG/DY + CJ*DG/DYPRIME,
C              (here CJ is a scalar determined by DDASSL)
C              is banded and the code is told this. In this
C              case, the storage needed will be greatly reduced,
C              numerical differencing will be performed much cheaper,
C              and a number of important algorithms will execute much
C              faster. The differential equation is said to have
C              half-bandwidths ML (lower) and MU (upper) if equation i
C              involves only unknowns Y(J) with
C                             I-ML .LE. J .LE. I+MU
C              for all I=1,2,...,NEQ. Thus, ML and MU are the widths
C              of the lower and upper parts of the band, respectively,
C              with the main diagonal being excluded. If you do not
C              indicate that the equation has a banded matrix of partial
C              derivatives, the code works with a full matrix of NEQ**2
C              elements (stored in the conventional way). Computations
C              with banded matrices cost less time and storage than with
C              full matrices if 2*ML+MU .LT. NEQ. If you tell the
C              code that the matrix of partial derivatives has a banded
C              structure and you want to provide subroutine JAC to
C              compute the partial derivatives, then you must be careful
C              to store the elements of the matrix in the special form
C              indicated in the description of JAC.
C
C          **** Do you want to solve the problem using a full
C               (dense) matrix (and not a special banded
C               structure) ...
C                Yes - Set INFO(6)=0
C                 No - Set INFO(6)=1
C                       and provide the lower (ML) and upper (MU)
C                       bandwidths by setting
C                       IWORK(1)=ML
C                       IWORK(2)=MU ****
C
C
C        INFO(7) -- You can specify a maximum (absolute value of)
C              stepsize, so that the code
C              will avoid passing over very
C              large regions.
C
C          ****  Do you want the code to decide
C                on its own maximum stepsize?
C                Yes - Set INFO(7)=0
C                 No - Set INFO(7)=1
C                      and define HMAX by setting
C                      RWORK(2)=HMAX ****
C
C        INFO(8) -- Differential/algebraic problems
C              may occaisionally suffer from
C              severe scaling difficulties on the
C              first step. If you know a great deal
C              about the scaling of your problem, you can
C              help to alleviate this problem by
C              specifying an initial stepsize HO.
C
C          ****  Do you want the code to define
C                its own initial stepsize?
C                Yes - Set INFO(8)=0
C                 No - Set INFO(8)=1
C                      and define HO by setting
C                      RWORK(3)=HO ****
C
C        INFO(9) -- If storage is a severe problem,
C              you can save some locations by
C              restricting the maximum order MAXORD.
C              the default value is 5. for each
C              order decrease below 5, the code
C              requires NEQ fewer locations, however
C              it is likely to be slower. In any
C              case, you must have 1 .LE. MAXORD .LE. 5
C          ****  Do you want the maximum order to
C                default to 5?
C                Yes - Set INFO(9)=0
C                 No - Set INFO(9)=1
C                      and define MAXORD by setting
C                      IWORK(3)=MAXORD ****
C
C        INFO(10) --If you know that the solutions to your equations
C               will always be nonnegative, it may help to set this
C               parameter. However, it is probably best to
C               try the code without using this option first,
C               and only to use this option if that doesn't
C               work very well.
C           ****  Do you want the code to solve the problem without
C                 invoking any special nonnegativity constraints?
C                  Yes - Set INFO(10)=0
C                   No - Set INFO(10)=1
C
C        INFO(11) --DDASSL normally requires the initial T,
C               Y, and YPRIME to be consistent. That is,
C               you must have G(T,Y,YPRIME) = 0 at the initial
C               time. If you do not know the initial
C               derivative precisely, you can let DDASSL try
C               to compute it.
C          ****   Are the initialHE INITIAL T, Y, YPRIME consistent?
C                 Yes - Set INFO(11) = 0
C                  No - Set INFO(11) = 1,
C                       and set YPRIME to an initial approximation
C                       to YPRIME.  (If you have no idea what
C                       YPRIME should be, set it to zero. Note
C                       that the initial Y should be such
C                       that there must exist a YPRIME so that
C                       G(T,Y,YPRIME) = 0.)
C
C   RTOL, ATOL -- You must assign relative (RTOL) and absolute (ATOL
C               error tolerances to tell the code how accurately you
C               want the solution to be computed. They must be defined
C               as variables because the code may change them. You
C               have two choices --
C                     Both RTOL and ATOL are scalars. (INFO(2)=0)
C                     Both RTOL and ATOL are vectors. (INFO(2)=1)
C               in either case all components must be non-negative.
C
C               The tolerances are used by the code in a local error
C               test at each step which requires roughly that
C                     ABS(LOCAL ERROR) .LE. RTOL*ABS(Y)+ATOL
C               for each vector component.
C               (More specifically, a root-mean-square norm is used to
C               measure the size of vectors, and the error test uses the
C               magnitude of the solution at the beginning of the step.)
C
C               the true (global) error is the difference between the
C               true solution of the initial value problem and the
C               computed approximation. Practically all present day
C               codes, including this one, control the local error at
C               each step and do not even attempt to control the global
C               error directly.
C               Usually, but not always, the true accuracy of the
C               computed Y is comparable to the error tolerances. This
C               code will usually, but not always, deliver a more
C               accurate solution if you reduce the tolerances and
C               integrate again. By comparing two such solutions you
C               can get a fairly reliable idea of the true error in the
C               solution at the bigger tolerances.
C
C               Setting ATOL=0. results in a pure relative error test on
C               that component. Setting RTOL=0. results in a pure
C               absolute error test on that component. A mixed test
C               with non-zero RTOL and ATOL corresponds roughly to a
C               relative error test when the solution component is much
C               bigger than ATOL and to an absolute error test when the
C               solution component is smaller than the threshhold ATOL.
C
C               The code will not attempt to compute a solution at an
C               accuracy unreasonable for the machine being used. It
C               will advise you if you ask for too much accuracy and
C               inform you as to the maximum accuracy it believes
C               possible.
C
C  RWORK(*) --  Dimension this real work array of length LRW in your
C               calling program.
C
C  LRW -- Set it to the declared length of the RWORK array.
C               You must have
C                    LRW .GE. 40+(MAXORD+4)*NEQ+NEQ**2
C               for the full (dense) JACOBIAN case (when INFO(6)=0), or
C                    LRW .GE. 40+(MAXORD+4)*NEQ+(2*ML+MU+1)*NEQ
C               for the banded user-defined JACOBIAN case
C               (when INFO(5)=1 and INFO(6)=1), or
C                     LRW .GE. 40+(MAXORD+4)*NEQ+(2*ML+MU+1)*NEQ
C                           +2*(NEQ/(ML+MU+1)+1)
C               for the banded finite-difference-generated JACOBIAN case
C               (when INFO(5)=0 and INFO(6)=1)
C
C  IWORK(*) --  Dimension this integer work array of length LIW in
C               your calling program.
C
C  LIW -- Set it to the declared length of the IWORK array.
C               you must have LIW .GE. 20+NEQ
C
C  RPAR, IPAR -- These are parameter arrays, of real and integer
C               type, respectively. You can use them for communication
C               between your program that calls DDASSL and the
C               RES subroutine (and the JAC subroutine). They are not
C               altered by DDASSL. If you do not need RPAR or IPAR,
C               ignore these parameters by treating them as dummy
C               arguments. If you do choose to use them, dimension
C               them in your calling program and in RES (and in JAC)
C               as arrays of appropriate length.
C
C  JAC -- If you have set INFO(5)=0, you can ignore this parameter
C               by treating it as a dummy argument. Otherwise, you must
C               provide a subroutine of the form
C               JAC(T,Y,YPRIME,PD,CJ,RPAR,IPAR)
C               to define the matrix of partial derivatives
C               PD=DG/DY+CJ*DG/DYPRIME
C               CJ is a scalar which is input to JAC.
C               For the given values of T,Y,YPRIME, the
C               subroutine must evaluate the non-zero partial
C               derivatives for each equation and each solution
C               component, and store these values in the
C               matrix PD. The elements of PD are set to zero
C               before each call to JAC so only non-zero elements
C               need to be defined.
C
C               Subroutine JAC must not alter T,Y,(*),YPRIME(*), or CJ.
C               You must declare the name JAC in an
C               EXTERNAL STATEMENT in your program that calls
C               DDASSL. You must dimension Y, YPRIME and PD
C               in JAC.
C
C               The way you must store the elements into the PD matrix
C               depends on the structure of the matrix which you
C               indicated by INFO(6).
C               *** INFO(6)=0 -- Full (dense) matrix ***
C                   Give PD a first dimension of NEQ.
C                   When you evaluate the (non-zero) partial derivative
C                   of equation I with respect to variable J, you must
C                   store it in PD according to
C                   PD(I,J) = * DG(I)/DY(J)+CJ*DG(I)/DYPRIME(J)*
C               *** INFO(6)=1 -- Banded JACOBIAN with ML lower and MU
C                   upper diagonal bands (refer to INFO(6) description
C                   of ML and MU) ***
C                   Give PD a first dimension of 2*ML+MU+1.
C                   when you evaluate the (non-zero) partial derivative
C                   of equation I with respect to variable J, you must
C                   store it in PD according to
C                   IROW = I - J + ML + MU + 1
C                   PD(IROW,J) = *DG(I)/DY(J)+CJ*DG(I)/DYPRIME(J)*
C               RPAR and IPAR are real and integer parameter arrays
C               which you can use for communication between your calling
C               program and your JACOBIAN subroutine JAC. They are not
C               altered by DDASSL. If you do not need RPAR or IPAR,
C               ignore these parameters by treating them as dummy
C               arguments. If you do choose to use them, dimension
C               them in your calling program and in JAC as arrays of
C               appropriate length.
C
C
C
C  OPTIONALLY REPLACEABLE NORM ROUTINE:
C  DDASSL uses a weighted norm DDANRM to measure the size
C  of vectors such as the estimated error in each step.
C  A FUNCTION subprogram
C    DOUBLE PRECISION FUNCTION DDANRM(NEQ,V,WT,RPAR,IPAR)
C    DIMENSION V(NEQ),WT(NEQ)
C  is used to define this norm. Here, V is the vector
C  whose norm is to be computed, and WT is a vector of
C  weights.  A DDANRM routine has been included with DDASSL
C  which computes the weighted root-mean-square norm
C  given by
C    DDANRM=SQRT((1/NEQ)*SUM(V(I)/WT(I))**2)
C  this norm is suitable for most problems. In some
C  special cases, it may be more convenient and/or
C  efficient to define your own norm by writing a function
C  subprogram to be called instead of DDANRM. This should
C  ,however, be attempted only after careful thought and
C  consideration.
C
C
C------OUTPUT-AFTER ANY RETURN FROM DDASSL----
C
C  The principal aim of the code is to return a computed solution at
C  TOUT, although it is also possible to obtain intermediate results
C  along the way. To find out whether the code achieved its goal
C  or if the integration process was interrupted before the task was
C  completed, you must check the IDID parameter.
C
C
C   T -- The solution was successfully advanced to the
C               output value of T.
C
C     Y(*) -- Contains the computed solution approximation at T.
C
C     YPRIME(*) -- Contains the computed derivative
C               approximation at T.
C
C     IDID -- Reports what the code did.
C
C                     *** Task completed ***
C                Reported by positive values of IDID
C
C     IDID = 1 -- A step was successfully taken in the
C       intermediate-output mode. The code has not yet reached TOUT.
C
C     IDID = 2 -- The integration to TOUT was successfully
C       completed (T=TOUT) by stepping exactly to TOUT.
C
C     IDID = 3 -- The integration to TOUT was successfully completed (T=TOUT) by 
!       stepping past TOUT. Y(*) is obtained by interpolation. YPRIME(*) is obtained by interpolation.
C
C                    *** Task interrupted ***
C                Reported by negative values of IDID
C
!     IDID = -1, The code has taken about 500 steps. If you want to continue, set INFO(1) = 1 and
!       call the code again. An additional 500 steps will be allowed.
!
!     IDID = -2, The error tolerances are too stringent. The error tolerances RTOL, ATOL have been
!       increased to values the code estimates appropriate for continuing. You may want to 
!	change them yourself. If you are sure you want to continue with relaxed error 
!	tolerances, set INFO(1)=1 and call the code again.
!
!     IDID = -3  The local error test cannot be satisfied because you specified a zero component in ATOL
!       and the corresponding computed solution component is zero. Thus, a pure relative error
!       test is impossible for this component. A solution component is zero and you set the
!       corresponding component of ATOL to zero. If you are sure you want to continue, you must 
!	first alter the error criterion to use positive values for those components of ATOL 
!	corresponding to zero solution components, then set INFO(1)=1 and call the code again.
!
!     IDID = -6, Repeated error test failures occurred on the last attempted step in DDASSL. A singularity 
!     	in the solution may be present. If you are absolutely certain you want to continue, you 
!	should restart the integration. (Provide initial values of Y and YPRIME which are consistent)
!
!     IDID = -7, Repeated convergence test failures occurred on the last attempted step in DDASSL. An 
!     	inaccurate or ill-conditioned JACOBIAN may be the problem. If you are absolutely certain 
!	you want to continue, you should restart the integration.
!
!     IDID = -8, The matrix of partial derivatives is singular. Some of your equations may be redundant.
!       could be removed, and then DDASSL could solve the problem. It is also possible
!       that a solution to your problem either does not exist or is not unique.
!
!     IDID = -9, DDASSL had multiple convergence test (corrector) failures, preceeded by multiple error
!       test failures, on the last attempted step. It is possible that your problem is ill-posed, 
!	and cannot be solved using this code. Or, there may be a discontinuity or a singularity in 
!	the solution. If you are absolutely certain you want to continue, you should restart the integration.
!
!     IDID =-10, DDASSL had multiple convergence test failures because IRES was equal to minus one.
!       If you are absolutely certain you want to continue, you should restart the integration.
!
!     IDID =-11, IRES=-2 was encountered, and control is being returned to the calling program.
!
!     IDID =-12, DDASSL failed to compute the initial YPRIME. This could happen because the initial 
!       approximation to YPRIME was not very good, or if a YPRIME consistent with the initial Y does not 
!       exist. The problem could also be caused by an inaccurate or singular iteration matrix.
C
C
C     IDID = -13,..,-32 -- Not applicable for this code
C
C                    *** Task terminated ***
C                Reported by the value of IDID=-33
C
C     IDID = -33 -- The code has encountered trouble from which
C       it cannot recover. A message is printed explaining the trouble and control is returned
C       to the calling program. For example, this occurs when invalid input is detected.
C
C     RTOL, ATOL -- These quantities remain unchanged except when IDID = -2. In this case, 
!       the error tolerances have been increased by the code to values which are estimated to
C       be appropriate for continuing the integration. However, the reported solution at T was obtained using 
!       the input values of RTOL and ATOL.
C
C   RWORK, IWORK -- Contain information which is usually of no
C               interest to the user but necessary for subsequent calls.
C               However, you may find use for
C
C               RWORK(3)--Which contains the step size H to be
C                       attempted on the next step.
C
C               RWORK(4)--Which contains the current value of the
C                       independent variable, i.e., the farthest point
C                       integration has reached. This will be different
C                       from T only when interpolation has been
C                       performed (IDID=3).
C
C               RWORK(7)--Which contains the stepsize used
C                       on the last successful step.
C
C               IWORK(7)--Which contains the order of the method to
C                       be attempted on the next step.
C
C               IWORK(8)--Which contains the order of the method used
C                       on the last step.
C
C               IWORK(11)--Which contains the number of steps taken so
C                        far.
C
C               IWORK(12)--Which contains the number of calls to RES
C                        so far.
C
C               IWORK(13)--Which contains the number of evaluations of
C                        the matrix of partial derivatives needed so
C                        far.
C
C               IWORK(14)--Which contains the total number
C                        of error test failures so far.
C
C               IWORK(15)--Which contains the total number
C                        of convergence test failures so far.
C                        (includes singular iteration matrix
C                        failures.)
C
C
C
C   INPUT -- What to do to continue the integration
C            (calls after the first)                **
C
C     This code is organized so that subsequent calls to continue the
C     integration involve little (if any) additional effort on your
C     part. You must monitor the IDID parameter in order to determine
C     what to do next.
C
C     Recalling that the principal task of the code is to integrate
C     from T to TOUT (the interval mode), usually all you will need
C     to do is specify a new TOUT upon reaching the current TOUT.
C
C     Do not alter any quantity not specifically permitted below,
C     in particular do not alter NEQ,T,Y(*),YPRIME(*),RWORK(*),IWORK(*)
C     or the differential equation in subroutine RES. Any such
C     alteration constitutes a new problem and must be treated as such,
C     i.e., you must start afresh.
C
C     You cannot change from vector to scalar error control or vice
C     versa (INFO(2)), but you can change the size of the entries of
C     RTOL, ATOL. Increasing a tolerance makes the equation easier
C     to integrate. Decreasing a tolerance will make the equation
C     harder to integrate and should generally be avoided.
C
C     You can switch from the intermediate-output mode to the
C     interval mode (INFO(3)) or vice versa at any time.
C
C     If it has been necessary to prevent the integration from going
C     past a point TSTOP (INFO(4), RWORK(1)), keep in mind that the
C     code will not integrate to any TOUT beyond the currently
C     specified TSTOP. Once TSTOP has been reached you must change
C     the value of TSTOP or set INFO(4)=0. You may change INFO(4)
C     or TSTOP at any time but you must supply the value of TSTOP in
C     RWORK(1) whenever you set INFO(4)=1.
C
C     Do not change INFO(5), INFO(6), IWORK(1), or IWORK(2)
C     unless you are going to restart the code.
C

C  ---------------------------------------------------------------------
C
C***REFERENCES  A DESCRIPTION OF DASSL: A DIFFERENTIAL/ALGEBRAIC
C                 SYSTEM SOLVER, L. R. PETZOLD, SAND82-8637,
C                 SANDIA NATIONAL LABORATORIES, SEPTEMBER 1982.
C***ROUTINES CALLED  DDASTP,DDAINI,DDANRM,DDAWTS,DDATRP,XERRWV,D1MACH
C***END PROLOGUE  DDASSL
C
C**End
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL DONE
      EXTERNAL RES,JAC
      DIMENSION Y(*),YPRIME(*)
      DIMENSION INFO(15)
      DIMENSION RWORK(*),IWORK(*)
      DIMENSION RTOL(*),ATOL(*)
      DIMENSION RPAR(*),IPAR(*)
      CHARACTER MSG*80, mesg*128
C
C     SET POINTERS INTO IWORK
      PARAMETER (LML=1, LMU=2, LMXORD=3, LMTYPE=4, LNST=11,
     *  LNRE=12, LNJE=13, LETF=14, LCTF=15, LNPD=16,
     *  LIPVT=21, LJCALC=5, LPHASE=6, LK=7, LKOLD=8,
     *  LNS=9, LNSTL=10, LIWM=1)
C
C     SET RELATIVE OFFSET INTO RWORK
      PARAMETER (NPD=1)
C
C     SET POINTERS INTO RWORK
      PARAMETER (LTSTOP=1, LHMAX=2, LH=3, LTN=4,
     *  LCJ=5, LCJOLD=6, LHOLD=7, LS=8, LROUND=9,
     *  LALPHA=11, LBETA=17, LGAMMA=23,
     *  LPSI=29, LSIGMA=35, LDELTA=41)
C
C*** ADDED BY GPF 11/21/91 IN CASE THE "NO REMEMBER"
C
      SAVE
C
C***FIRST EXECUTABLE STATEMENT  DDASSL
      IF(INFO(1).NE.0)GO TO 100
C
C-----------------------------------------------------------------------
C     THIS BLOCK IS EXECUTED FOR THE INITIAL CALL ONLY.
C     IT CONTAINS CHECKING OF INPUTS AND INITIALIZATIONS.
C-----------------------------------------------------------------------
C
C     FIRST CHECK INFO ARRAY TO MAKE SURE ALL ELEMENTS OF INFO
C     ARE EITHER ZERO OR ONE.
      DO 10 I=2,11
C
C     ADDED BY PAR 01/08/93 TO ALLOW AND ADDED OPTION FOR SOLVING THE JACOBIAN
C
         IF (I.EQ.5) THEN
            IF (INFO(5).NE.0.AND.INFO(5).NE.1.AND.INFO(5).NE.2) 
     .                GO TO 701
            GO TO 10
         END IF
         IF(INFO(I).NE.0.AND.INFO(I).NE.1)GO TO 701
10       CONTINUE
C
      IF(NEQ.LE.0)GO TO 702
C
C     CHECK AND COMPUTE MAXIMUM ORDER
      MXORD=5
      IF(INFO(9).EQ.0)GO TO 20
         MXORD=IWORK(LMXORD)
         IF(MXORD.LT.1.OR.MXORD.GT.5)GO TO 703
20       IWORK(LMXORD)=MXORD
C
C     COMPUTE MTYPE,LENPD,LENRW.CHECK ML AND MU.
      IF(INFO(6).NE.0)GO TO 40
         LENPD=NEQ**2
         LENRW=40+(IWORK(LMXORD)+4)*NEQ+LENPD
         IF(INFO(5).NE.0)GO TO 30
            IWORK(LMTYPE)=2
            GO TO 60
C
C     MODIFIED BY PAR 01/08/93 TO ALLOW FOR CUSTOM SOLUTION OF JACOBIAN
C
30          IF (INFO(5).EQ.1) THEN
               IWORK(LMTYPE)=1
            ELSE
               IWORK(LMTYPE)=6
               JACDIM = JACD()
               LENPD = JACDIM**2
               LENRW=40+(IWORK(LMXORD)+4)*NEQ+LENPD
            END IF
            GO TO 60
40    IF(IWORK(LML).LT.0.OR.IWORK(LML).GE.NEQ)GO TO 717
      IF(IWORK(LMU).LT.0.OR.IWORK(LMU).GE.NEQ)GO TO 718
      LENPD=(2*IWORK(LML)+IWORK(LMU)+1)*NEQ
      IF(INFO(5).NE.0)GO TO 50
         IWORK(LMTYPE)=5
         MBAND=IWORK(LML)+IWORK(LMU)+1
         MSAVE=(NEQ/MBAND)+1
         LENRW=40+(IWORK(LMXORD)+4)*NEQ+LENPD+2*MSAVE
         GO TO 60
50       IWORK(LMTYPE)=4
         LENRW=40+(IWORK(LMXORD)+4)*NEQ+LENPD
C
C     CHECK LENGTHS OF RWORK AND IWORK
60    LENIW=20+NEQ
      IWORK(LNPD)=LENPD
      IF(LRW.LT.LENRW)GO TO 704
      IF(LIW.LT.LENIW)GO TO 705
C
C     CHECK TO SEE THAT TOUT IS DIFFERENT FROM T
      IF(TOUT .EQ. T)GO TO 719
C
C     CHECK HMAX
      IF(INFO(7).EQ.0)GO TO 70
         HMAX=RWORK(LHMAX)
         IF(HMAX.LE.0.0D0)GO TO 710
70    CONTINUE
C
C     INITIALIZE COUNTERS
      IWORK(LNST)=0
      IWORK(LNRE)=0
      IWORK(LNJE)=0
C
      IWORK(LNSTL)=0
      IDID=1
      GO TO 200
C
C-----------------------------------------------------------------------
C     THIS BLOCK IS FOR CONTINUATION CALLS
C     ONLY. HERE WE CHECK INFO(1),AND IF THE
C     LAST STEP WAS INTERRUPTED WE CHECK WHETHER
C     APPROPRIATE ACTION WAS TAKEN.
C-----------------------------------------------------------------------
C
100   CONTINUE
      IF(INFO(1).EQ.1)GO TO 110
      IF(INFO(1).NE.-1)GO TO 701
C     IF WE ARE HERE, THE LAST STEP WAS INTERRUPTED
C     BY AN ERROR CONDITION FROM DDASTP,AND
C     APPROPRIATE ACTION WAS NOT TAKEN. THIS
C     IS A FATAL ERROR.
      MSG = 'DASSL--  THE LAST STEP TERMINATED WITH A NEGATIVE'
      CALL XERRWV(MSG,49,201,0,0,0,0,0,0.0D0,0.0D0)
      MSG = 'DASSL--  VALUE (=I1) OF IDID AND NO APPROPRIATE'
      CALL XERRWV(MSG,47,202,0,1,IDID,0,0,0.0D0,0.0D0)
      MSG = 'DASSL--  ACTION WAS TAKEN. RUN TERMINATED'
      CALL XERRWV(MSG,41,203,1,0,0,0,0,0.0D0,0.0D0)
      RETURN
110   CONTINUE
      IWORK(LNSTL)=IWORK(LNST)
C
C-----------------------------------------------------------------------
C     THIS BLOCK IS EXECUTED ON ALL CALLS.
C     THE ERROR TOLERANCE PARAMETERS ARE
C     CHECKED, AND THE WORK ARRAY POINTERS
C     ARE SET.
C-----------------------------------------------------------------------
C
200   CONTINUE
C     CHECK RTOL,ATOL
      NZFLG=0
      RTOLI=RTOL(1)
      ATOLI=ATOL(1)
      DO 210 I=1,NEQ
         IF(INFO(2).EQ.1)RTOLI=RTOL(I)
         IF(INFO(2).EQ.1)ATOLI=ATOL(I)
         IF(RTOLI.GT.0.0D0.OR.ATOLI.GT.0.0D0)NZFLG=1
         IF(RTOLI.LT.0.0D0)GO TO 706
         IF(ATOLI.LT.0.0D0)GO TO 707
210      CONTINUE
      IF(NZFLG.EQ.0)GO TO 708
C
C     SET UP RWORK STORAGE.IWORK STORAGE IS FIXED
C     IN DATA STATEMENT.
      LE=LDELTA+NEQ
      LWT=LE+NEQ
      LPHI=LWT+NEQ
      LPD=LPHI+(IWORK(LMXORD)+1)*NEQ
      LWM=LPD
      NTEMP=NPD+IWORK(LNPD)
      IF(INFO(1).EQ.1)GO TO 400
C
C-----------------------------------------------------------------------
C     THIS BLOCK IS EXECUTED ON THE INITIAL CALL
C     ONLY. SET THE INITIAL STEP SIZE, AND
C     THE ERROR WEIGHT VECTOR, AND PHI.
C     COMPUTE INITIAL YPRIME, IF NECESSARY.
C-----------------------------------------------------------------------
C
300   CONTINUE
      TN=T
      IDID=1
C
C     SET ERROR WEIGHT VECTOR WT
      CALL DDAWTS(NEQ,INFO(2),RTOL,ATOL,Y,RWORK(LWT),RPAR,IPAR)
      DO 305 I = 1,NEQ
         IF(RWORK(LWT+I-1).LE.0.0D0) GO TO 713
305      CONTINUE
C
C     COMPUTE UNIT ROUNDOFF AND HMIN
      UROUND = D1MACH(4)
      RWORK(LROUND) = UROUND
      HMIN = 4.0D0*UROUND*DMAX1(DABS(T),DABS(TOUT))
C
C     CHECK INITIAL INTERVAL TO SEE THAT IT IS LONG ENOUGH
      TDIST = DABS(TOUT - T)
      IF(TDIST .LT. HMIN) GO TO 714
C
C     CHECK HO, IF THIS WAS INPUT
      IF (INFO(8) .EQ. 0) GO TO 310
         HO = RWORK(LH)
         IF ((TOUT - T)*HO .LT. 0.0D0) GO TO 711
         IF (HO .EQ. 0.0D0) GO TO 712
         GO TO 320
310    CONTINUE
C
C     COMPUTE INITIAL STEPSIZE, TO BE USED BY EITHER
C     DDASTP OR DDAINI, DEPENDING ON INFO(11)
      HO = 0.001D0*TDIST
      YPNORM = DDANRM(NEQ,YPRIME,RWORK(LWT),RPAR,IPAR)
      IF (YPNORM .GT. 0.5D0/HO) HO = 0.5D0/YPNORM
      HO = DSIGN(HO,TOUT-T)
C     ADJUST HO IF NECESSARY TO MEET HMAX BOUND
320   IF (INFO(7) .EQ. 0) GO TO 330
         RH = DABS(HO)/RWORK(LHMAX)
         IF (RH .GT. 1.0D0) HO = HO/RH
C     COMPUTE TSTOP, IF APPLICABLE
330   IF (INFO(4) .EQ. 0) GO TO 340
         TSTOP = RWORK(LTSTOP)
         IF ((TSTOP - T)*HO .LT. 0.0D0) GO TO 715
         IF ((T + HO - TSTOP)*HO .GT. 0.0D0) HO = TSTOP - T
         IF ((TSTOP - TOUT)*HO .LT. 0.0D0) GO TO 709
C
C     COMPUTE INITIAL DERIVATIVE, UPDATING TN AND Y, IF APPLICABLE
340   IF (INFO(11) .EQ. 0) GO TO 350
      CALL DDAINI(TN,Y,YPRIME,NEQ,
     *  RES,JAC,HO,RWORK(LWT),IDID,RPAR,IPAR,
     *  RWORK(LPHI),RWORK(LDELTA),RWORK(LE),
     *  RWORK(LWM),IWORK(LIWM),HMIN,RWORK(LROUND),
     *  INFO(10),NTEMP)
      IF (IDID .LT. 0) GO TO 390
C
C     LOAD H WITH HO.  STORE H IN RWORK(LH)
350   H = HO
      RWORK(LH) = H
C
C     LOAD Y AND H*YPRIME INTO PHI(*,1) AND PHI(*,2)
360   ITEMP = LPHI + NEQ
      DO 370 I = 1,NEQ
         RWORK(LPHI + I - 1) = Y(I)
370      RWORK(ITEMP + I - 1) = H*YPRIME(I)
C
390   GO TO 500
C
C-------------------------------------------------------
C     THIS BLOCK IS FOR CONTINUATION CALLS ONLY. ITS
C     PURPOSE IS TO CHECK STOP CONDITIONS BEFORE
C     TAKING A STEP.
C     ADJUST H IF NECESSARY TO MEET HMAX BOUND
C-------------------------------------------------------
C
400   CONTINUE
      UROUND=RWORK(LROUND)
      DONE = .FALSE.
      TN=RWORK(LTN)
      H=RWORK(LH)
      IF(INFO(7) .EQ. 0) GO TO 410
         RH = DABS(H)/RWORK(LHMAX)
         IF(RH .GT. 1.0D0) H = H/RH
410   CONTINUE
      IF(T .EQ. TOUT) GO TO 719
      IF((T - TOUT)*H .GT. 0.0D0) GO TO 711
      IF(INFO(4) .EQ. 1) GO TO 430
      IF(INFO(3) .EQ. 1) GO TO 420
      IF((TN-TOUT)*H.LT.0.0D0)GO TO 490
      CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T=TOUT
      IDID = 3
      DONE = .TRUE.
      GO TO 490
420   IF((TN-T)*H .LE. 0.0D0) GO TO 490
      IF((TN - TOUT)*H .GT. 0.0D0) GO TO 425
      CALL DDATRP(TN,TN,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T = TN
      IDID = 1
      DONE = .TRUE.
      GO TO 490
425   CONTINUE
      CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T = TOUT
      IDID = 3
      DONE = .TRUE.
      GO TO 490
430   IF(INFO(3) .EQ. 1) GO TO 440
      TSTOP=RWORK(LTSTOP)
      IF((TN-TSTOP)*H.GT.0.0D0) GO TO 715
      IF((TSTOP-TOUT)*H.LT.0.0D0)GO TO 709
      IF((TN-TOUT)*H.LT.0.0D0)GO TO 450
      CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,IWORK(LKOLD),
     *   RWORK(LPHI),RWORK(LPSI))
      T=TOUT
      IDID = 3
      DONE = .TRUE.
      GO TO 490
440   TSTOP = RWORK(LTSTOP)
      IF((TN-TSTOP)*H .GT. 0.0D0) GO TO 715
      IF((TSTOP-TOUT)*H .LT. 0.0D0) GO TO 709
      IF((TN-T)*H .LE. 0.0D0) GO TO 450
      IF((TN - TOUT)*H .GT. 0.0D0) GO TO 445
      CALL DDATRP(TN,TN,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T = TN
      IDID = 1
      DONE = .TRUE.
      GO TO 490
445   CONTINUE
      CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T = TOUT
      IDID = 3
      DONE = .TRUE.
      GO TO 490
450   CONTINUE
C     CHECK WHETHER WE ARE WITH IN ROUNDOFF OF TSTOP
      IF(DABS(TN-TSTOP).GT.100.0D0*UROUND*
     *   (DABS(TN)+DABS(H)))GO TO 460
      IDID=2
      T=TSTOP
      DONE = .TRUE.
      GO TO 490
460   TNEXT=TN+H*(1.0D0+4.0D0*UROUND)
      IF((TNEXT-TSTOP)*H.LE.0.0D0)GO TO 490
      H=(TSTOP-TN)*(1.0D0-4.0D0*UROUND)
      RWORK(LH)=H
C
490   IF (DONE) GO TO 590
C
C-------------------------------------------------------
C     THE NEXT BLOCK CONTAINS THE CALL TO THE
C     ONE-STEP INTEGRATOR DDASTP.
C     THIS IS A LOOPING POINT FOR THE INTEGRATION STEPS.
C     CHECK FOR TOO MANY STEPS.
C     UPDATE WT.
C     CHECK FOR TOO MUCH ACCURACY REQUESTED.
C     COMPUTE MINIMUM STEPSIZE.
C-------------------------------------------------------
C
500   CONTINUE
C     CHECK FOR FAILURE TO COMPUTE INITIAL YPRIME
      IF (IDID .EQ. -12) GO TO 527
C
C     CHECK FOR TOO MANY STEPS
      IF((IWORK(LNST)-IWORK(LNSTL)).LT.500)
     *   GO TO 510
           IDID=-1
           GO TO 527
C
C     UPDATE WT
510   CALL DDAWTS(NEQ,INFO(2),RTOL,ATOL,RWORK(LPHI),
     *  RWORK(LWT),RPAR,IPAR)
      DO 520 I=1,NEQ
         IF(RWORK(I+LWT-1).GT.0.0D0)GO TO 520
           IDID=-3
           GO TO 527
520   CONTINUE
C
C     TEST FOR TOO MUCH ACCURACY REQUESTED.
      R=DDANRM(NEQ,RWORK(LPHI),RWORK(LWT),RPAR,IPAR)*
     *   100.0D0*UROUND
      IF(R.LE.1.0D0)GO TO 525
C     MULTIPLY RTOL AND ATOL BY R AND RETURN
      IF(INFO(2).EQ.1)GO TO 523
           RTOL(1)=R*RTOL(1)
           ATOL(1)=R*ATOL(1)
           IDID=-2
           GO TO 527
523   DO 524 I=1,NEQ
           RTOL(I)=R*RTOL(I)
524        ATOL(I)=R*ATOL(I)
      IDID=-2
      GO TO 527
525   CONTINUE
C
C     COMPUTE MINIMUM STEPSIZE
      HMIN=4.0D0*UROUND*DMAX1(DABS(TN),DABS(TOUT))
C
      CALL DDASTP(TN,Y,YPRIME,NEQ,
     *   RES,JAC,H,RWORK(LWT),INFO(1),IDID,RPAR,IPAR,
     *   RWORK(LPHI),RWORK(LDELTA),RWORK(LE),
     *   RWORK(LWM),IWORK(LIWM),
     *   RWORK(LALPHA),RWORK(LBETA),RWORK(LGAMMA),
     *   RWORK(LPSI),RWORK(LSIGMA),
     *   RWORK(LCJ),RWORK(LCJOLD),RWORK(LHOLD),
     *   RWORK(LS),HMIN,RWORK(LROUND),
     *   IWORK(LPHASE),IWORK(LJCALC),IWORK(LK),
     *   IWORK(LKOLD),IWORK(LNS),INFO(10),NTEMP)
527   IF(IDID.LT.0)GO TO 600
C
C--------------------------------------------------------
C     THIS BLOCK HANDLES THE CASE OF A SUCCESSFUL RETURN
C     FROM DDASTP (IDID=1).  TEST FOR STOP CONDITIONS.
C--------------------------------------------------------
C
      IF(INFO(4).NE.0)GO TO 540
           IF(INFO(3).NE.0)GO TO 530
             IF((TN-TOUT)*H.LT.0.0D0)GO TO 500
             CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,
     *         IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
             IDID=3
             T=TOUT
             GO TO 580
530          IF((TN-TOUT)*H.GE.0.0D0)GO TO 535
             T=TN
             IDID=1
             GO TO 580
535          CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,
     *         IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
             IDID=3
             T=TOUT
             GO TO 580
540   IF(INFO(3).NE.0)GO TO 550
      IF((TN-TOUT)*H.LT.0.0D0)GO TO 542
         CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,
     *     IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
         T=TOUT
         IDID=3
         GO TO 580
542   IF(DABS(TN-TSTOP).LE.100.0D0*UROUND*
     *   (DABS(TN)+DABS(H)))GO TO 545
      TNEXT=TN+H*(1.0D0+4.0D0*UROUND)
      IF((TNEXT-TSTOP)*H.LE.0.0D0)GO TO 500
      H=(TSTOP-TN)*(1.0D0-4.0D0*UROUND)
      GO TO 500
545   IDID=2
      T=TSTOP
      GO TO 580
550   IF((TN-TOUT)*H.GE.0.0D0)GO TO 555
      IF(DABS(TN-TSTOP).LE.100.0D0*UROUND*(DABS(TN)+DABS(H)))GO TO 552
      T=TN
      IDID=1
      GO TO 580
552   IDID=2
      T=TSTOP
      GO TO 580
555   CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,
     *   IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
      T=TOUT
      IDID=3
580   CONTINUE

C-----------------------------------------------------------------
C     ALL SUCCESSFUL RETURNS FROM DDASSL ARE MADE FROM THIS BLOCK.
C-----------------------------------------------------------------

590   CONTINUE
      RWORK(LTN)=TN
      RWORK(LH)=H
      RETURN

C------------------------------------------------------------------------------
C     THIS BLOCK HANDLES ALL UNSUCCESSFUL RETURNS OTHER THAN FOR ILLEGAL INPUT.
C------------------------------------------------------------------------------

600   CONTINUE
      ITEMP= iabs(IDID)
      GO TO (610,620,630,690,690,640,650,660,670,675,
     *  680,685), ITEMP

C     THE MAXIMUM NUMBER OF STEPS WAS TAKEN BEFORE REACHING TOUT

610   mesg = 'AT CURRENT T (=R1) 500 STEPS TAKEN ON THIS CALL'
     . // ' BEFORE REACHING TOUT'
      CALL XERRmod(mesg,38,610,1,TN,0.0D0)
      GO TO 690

C     TOO MUCH ACCURACY FOR MACHINE PRECISION
620   mesg = 'AT T (=R1) TOO MUCH ACCURACY REQUESTED'
     . // ' FOR PRECISION OF MACHINE. RTOL AND ATOL'
     . // ' WERE INCREASED TO APPROPRIATE VALUES'
      CALL XERRmod(MESG,47,620,1,TN,0.0D0)
      GO TO 690

630   mesg = 'AT T (=R1) SOME ELEMENT OF WT'
     . // ' HAS BECOME .LE. 0.0'
      CALL XERRmod(mesg,38,630,1,TN,0.0D0)
      GO TO 690

C     ERROR TEST FAILED REPEATEDLY OR WITH H=HMIN
640   mesg = 'AT T (=R1) AND STEPSIZE H (=R2) THE'
     . // ' ERROR TEST FAILED REPEATEDLY OR WITH ABS(H)=HMIN'
      CALL XErrmod(mesg,44,640,2,TN,H)
      GO TO 690

C     CORRECTOR CONVERGENCE FAILED REPEATEDLY OR WITH H=HMIN
650   mesg = 'AT T (=R1) AND STEPSIZE H (=R2) THE'
     . // ' CORRECTOR FAILED TO CONVERGE REPEATEDLY'
     . // ' OR WITH ABS(H)=HMIN'
      CALL XERRmod(mesg,44,650,2,TN,H)
      GO TO 690

C     THE ITERATION MATRIX IS SINGULAR
660   mesg = 'AT T (=R1) AND STEPSIZE H (=R2) THE'
     . // ' ITERATION MATRIX IS SINGULAR'
      CALL XERRmod(mesg,44,660,2,TN,H)
      GO TO 690

C     CORRECTOR FAILURE PRECEEDED BY ERROR TEST FAILURES.
670   mesg = 'AT T (=R1) AND STEPSIZE H (=R2) THE'
     . // ' CORRECTOR COULD NOT CONVERGE.  ALSO, THE'
     . // ' ERROR TEST FAILED REPEATEDLY.'
      CALL XERRmod(MESG,44,670,2,TN,H)
      GO TO 690

C     CORRECTOR FAILURE BECAUSE IRES = -1
675   mesg = 'AT T (=R1) AND STEPSIZE H (=R2) THE'
     . // ' CORRECTOR COULD NOT CONVERGE BECAUSE'
     . // ' IRES WAS EQUAL TO MINUS ONE'
      CALL XERRmod(mesg,44,675,2,TN,H)
      GO TO 690

C     FAILURE BECAUSE IRES = -2
680   mesg = 'AT T (=R1) AND STEPSIZE H (=R2)'
     . // ' IRES WAS EQUAL TO MINUS TWO'
      CALL XERRmod(mesg,40,680,2,TN,H)
      GO TO 690

C     FAILED TO COMPUTE INITIAL YPRIME
685   mesg = 'AT T (=R1) AND STEPSIZE H (=R2) THE INITIAL '
     . // 'YPRIME COULD NOT BE COMPUTED'
      CALL XERRmod(mesg,80,685,2,TN,H)
      GO TO 690

!     collective return from errors
690   CONTINUE
      INFO(1)=-1
      T=TN
      RWORK(LTN)=TN
      RWORK(LH)=H
      RETURN

C-------------------------------------------------------------------------
C     THIS BLOCK HANDLES ALL ERROR RETURNS DUE TO ILLEGAL INPUT, AS 
!     DETECTED BEFORE CALLING DDASTP. FIRST THE ERROR MESSAGE ROUTINE IS
C     CALLED. IF THIS HAPPENS TWICE IN SUCCESSION, EXECUTION IS TERMINATED
C-------------------------------------------------------------------------

701   MSG = 'DASSL--  SOME ELEMENT OF INFO VECTOR IS NOT ZERO OR ONE'
      CALL XERRWV(MSG,55,1,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
702   MSG = 'DASSL--  NEQ (=I1) .LE. 0'
      CALL XERRWV(MSG,25,2,0,1,NEQ,0,0,0.0D0,0.0D0)
      GO TO 750
703   MSG = 'DASSL--  MAXORD (=I1) NOT IN RANGE'
      CALL XERRWV(MSG,34,3,0,1,MXORD,0,0,0.0D0,0.0D0)
      GO TO 750
704   MSG='DASSL--  RWORK LENGTH NEEDED, LENRW (=I1), EXCEEDS LRW (=I2)'
      CALL XERRWV(MSG,60,4,0,2,LENRW,LRW,0,0.0D0,0.0D0)
      GO TO 750
705   MSG='DASSL--  IWORK LENGTH NEEDED, LENIW (=I1), EXCEEDS LIW (=I2)'
      CALL XERRWV(MSG,60,5,0,2,LENIW,LIW,0,0.0D0,0.0D0)
      GO TO 750
706   MSG = 'DASSL--  SOME ELEMENT OF RTOL IS .LT. 0'
      CALL XERRWV(MSG,39,6,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
707   MSG = 'DASSL--  SOME ELEMENT OF ATOL IS .LT. 0'
      CALL XERRWV(MSG,39,7,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
708   MSG = 'DASSL--  ALL ELEMENTS OF RTOL AND ATOL ARE ZERO'
      CALL XERRWV(MSG,47,8,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
709   MSG='DASSL--  INFO(4) = 1 AND TSTOP (=R1) BEHIND TOUT (=R2)'
      CALL XERRWV(MSG,54,9,0,0,0,0,2,TSTOP,TOUT)
      GO TO 750
710   MSG = 'DASSL--  HMAX (=R1) .LT. 0.0'
      CALL XERRWV(MSG,28,10,0,0,0,0,1,HMAX,0.0D0)
      GO TO 750
711   MSG = 'DASSL--  TOUT (=R1) BEHIND T (=R2)'
      CALL XERRWV(MSG,34,11,0,0,0,0,2,TOUT,T)
      GO TO 750
712   MSG = 'DASSL--  INFO(8)=1 AND H0=0.0'
      CALL XERRWV(MSG,29,12,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
713   MSG = 'DASSL--  SOME ELEMENT OF WT IS .LE. 0.0'
      CALL XERRWV(MSG,39,13,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
714   MSG='DASSL-- TOUT (=R1) TOO CLOSE TO T (=R2) TO START INTEGRATION'
      CALL XERRWV(MSG,60,14,0,0,0,0,2,TOUT,T)
      GO TO 750
715   MSG = 'DASSL--  INFO(4)=1 AND TSTOP (=R1) BEHIND T (=R2)'
      CALL XERRWV(MSG,49,15,0,0,0,0,2,TSTOP,T)
      GO TO 750
717   MSG = 'DASSL--  ML (=I1) ILLEGAL. EITHER .LT. 0 OR .GT. NEQ'
      CALL XERRWV(MSG,52,17,0,1,IWORK(LML),0,0,0.0D0,0.0D0)
      GO TO 750
718   MSG = 'DASSL--  MU (=I1) ILLEGAL. EITHER .LT. 0 OR .GT. NEQ'
      CALL XERRWV(MSG,52,18,0,1,IWORK(LMU),0,0,0.0D0,0.0D0)
      GO TO 750
719   MSG = 'DASSL--  TOUT (=R1) IS EQUAL TO T (=R2)'
      CALL XERRWV(MSG,39,19,0,0,0,0,2,TOUT,T)
      GO TO 750
750   IF(INFO(1).EQ.-1) GO TO 760
      INFO(1)=-1
      IDID=-33
      RETURN
760   MSG = 'DASSL--  REPEATED OCCURRENCES OF ILLEGAL INPUT'
      CALL XERRWV(MSG,46,801,0,0,0,0,0,0.0D0,0.0D0)
770   MSG = 'DASSL--  RUN TERMINATED. APPARENT INFINITE LOOP'
      CALL XERRWV(MSG,47,802,1,0,0,0,0,0.0D0,0.0D0)
      RETURN
C-----------END OF SUBROUTINE DDASSL------------------------------------
      END

      SUBROUTINE DDAWTS(NEQ,IWT,RTOL,ATOL,Y,WT,RPAR,IPAR)

C***BEGIN PROLOGUE  DDAWTS
C***REFER TO  DDASSL
C***ROUTINES CALLED  (NONE)
C***DATE WRITTEN   830315   (YYMMDD)
C***REVISION DATE  891228   (YYMMDD)
C***END PROLOGUE  DDAWTS
C-----------------------------------------------------------------------
C     THIS SUBROUTINE SETS THE ERROR WEIGHT VECTOR
C     WT ACCORDING TO WT(I)=RTOL(I)*ABS(Y(I))+ATOL(I),
C     I=1,-,N.
C     RTOL AND ATOL ARE SCALARS IF IWT = 0,
C     AND VECTORS IF IWT = 1.
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RTOL(*),ATOL(*),Y(*),WT(*)
      DIMENSION RPAR(*),IPAR(*)
C
C*** ADDED BY GPF 11/21/91 IN CASE THE "NO REMEMBER"
C
      SAVE
C
      RTOLI=RTOL(1)
      ATOLI=ATOL(1)
      DO 20 I=1,NEQ
         IF (IWT .EQ.0) GO TO 10
           RTOLI=RTOL(I)
           ATOLI=ATOL(I)
10         WT(I)=RTOLI*DABS(Y(I))+ATOLI
20         CONTINUE
      RETURN
C-----------END OF SUBROUTINE DDAWTS------------------------------------
      END
      DOUBLE PRECISION FUNCTION DDANRM(NEQ,V,WT,RPAR,IPAR)
C
C***BEGIN PROLOGUE  DDANRM
C***REFER TO  DDASSL
C***ROUTINES CALLED  (NONE)
C***DATE WRITTEN   830315   (YYMMDD)
C***REVISION DATE  891228   (YYMMDD)
C***END PROLOGUE  DDANRM
C-----------------------------------------------------------------------
C     THIS FUNCTION ROUTINE COMPUTES THE WEIGHTED
C     ROOT-MEAN-SQUARE NORM OF THE VECTOR OF LENGTH
C     NEQ CONTAINED IN THE ARRAY V,WITH WEIGHTS
C     CONTAINED IN THE ARRAY WT OF LENGTH NEQ.
C        DDANRM=SQRT((1/NEQ)*SUM(V(I)/WT(I))**2)
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION V(NEQ),WT(NEQ)
      DIMENSION RPAR(*),IPAR(*)
      DDANRM = 0.0D0
      VMAX = 0.0D0
      IPAR(3) = 1
      DO 10 I = 1,NEQ
        IF(DABS(V(I)/WT(I)) .GT. VMAX) THEN
           VMAX = DABS(V(I)/WT(I))
           IPAR(3) = I
        ENDIF
10      CONTINUE
      IF(VMAX .LE. 0.0D0) GO TO 30
      SUM = 0.0D0
      DO 20 I = 1,NEQ
20      SUM = SUM + ((V(I)/WT(I))/VMAX)**2
      DDANRM = VMAX*DSQRT(SUM/DFLOAT(NEQ))
30    CONTINUE
      RETURN
C------END OF FUNCTION DDANRM------
      END
      SUBROUTINE DDAINI(X,Y,YPRIME,NEQ,
     *   RES,JAC,H,WT,IDID,RPAR,IPAR,
     *   PHI,DELTA,E,WM,IWM,
     *   HMIN,UROUND,NONNEG,NTEMP)
C
C***BEGIN PROLOGUE  DDAINI
C***REFER TO  DDASSL
C***ROUTINES CALLED  DDANRM,DDAJAC,DDASLV
C***DATE WRITTEN   830315   (YYMMDD)
C***REVISION DATE  891228   (YYMMDD)
C***END PROLOGUE DDAINI
C
C-----------------------------------------------------------------
C     DDAINI TAKES ONE STEP OF SIZE H OR SMALLER
C     WITH THE BACKWARD EULER METHOD, TO
C     FIND YPRIME.  X AND Y ARE UPDATED TO BE CONSISTENT WITH THE
C     NEW STEP.  A MODIFIED DAMPED NEWTON ITERATION IS USED TO
C     SOLVE THE CORRECTOR ITERATION.
C
C     THE INITIAL GUESS FOR YPRIME IS USED IN THE
C     PREDICTION, AND IN FORMING THE ITERATION
C     MATRIX, BUT IS NOT INVOLVED IN THE
C     ERROR TEST. THIS MAY HAVE TROUBLE
C     CONVERGING IF THE INITIAL GUESS IS NO
C     GOOD, OR IF G(X,Y,YPRIME) DEPENDS
C     NONLINEARLY ON YPRIME.
C
C     THE PARAMETERS REPRESENT:
C     X --         INDEPENDENT VARIABLE
C     Y --         SOLUTION VECTOR AT X
C     YPRIME --    DERIVATIVE OF SOLUTION VECTOR
C     NEQ --       NUMBER OF EQUATIONS
C     H --         STEPSIZE. IMDER MAY USE A STEPSIZE
C                  SMALLER THAN H.
C     WT --        VECTOR OF WEIGHTS FOR ERROR
C                  CRITERION
C     IDID --      COMPLETION CODE WITH THE FOLLOWING MEANINGS
C                  IDID= 1 -- YPRIME WAS FOUND SUCCESSFULLY
C                  IDID=-12 -- DDAINI FAILED TO FIND YPRIME
C     RPAR,IPAR -- REAL AND INTEGER PARAMETER ARRAYS
C                  THAT ARE NOT ALTERED BY DDAINI
C     PHI --       WORK SPACE FOR DDAINI
C     DELTA,E --   WORK SPACE FOR DDAINI
C     WM,IWM --    REAL AND INTEGER ARRAYS STORING
C                  MATRIX INFORMATION
C
C-----------------------------------------------------------------
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL CONVGD
      DIMENSION Y(*),YPRIME(*),WT(*)
      DIMENSION PHI(NEQ,*),DELTA(*),E(*)
      DIMENSION WM(*),IWM(*)
      DIMENSION RPAR(*),IPAR(*)
      EXTERNAL RES,JAC
C
      PARAMETER (LNRE=12)
      PARAMETER (LNJE=13)
C
C*** ADDED BY GPF 11/21/91 IN CASE THE "NO REMEMBER"
C
      SAVE
C
C
      DATA MAXIT/10/,MJAC/5/
      DATA DAMP/0.75D0/
C
C
C---------------------------------------------------
C     BLOCK 1.
C     INITIALIZATIONS.
C---------------------------------------------------
C
      IDID=1
      NEF=0
      NCF=0
      NSF=0
      XOLD=X
      YNORM=DDANRM(NEQ,Y,WT,RPAR,IPAR)
C
C     SAVE Y AND YPRIME IN PHI
      DO 100 I=1,NEQ
         PHI(I,1)=Y(I)
100      PHI(I,2)=YPRIME(I)
C
C
C----------------------------------------------------
C     BLOCK 2.
C     DO ONE BACKWARD EULER STEP.
C----------------------------------------------------
C
C     SET UP FOR START OF CORRECTOR ITERATION
200   CJ=1.0D0/H
      X=X+H
C
C     PREDICT SOLUTION AND DERIVATIVE
      DO 250 I=1,NEQ
250     Y(I)=Y(I)+H*YPRIME(I)
C
      JCALC=-1
      M=0
      CONVGD=.TRUE.
C
C
C     CORRECTOR LOOP.
300   IWM(LNRE)=IWM(LNRE)+1
      IRES=0
C
C*** WE'RE ABOUT TO COMPUTE A JACOBIAN SO SET THE IDERV
C    FLAG, TELLING RES TO COMPUTE JACOBIAN ELEMENTS
C
      CALL SETDERV(0)
C
      CALL RES(X,Y,YPRIME,DELTA,IRES,RPAR,IPAR)
      CALL SETDERV(0)

      IF (IRES.LT.0) GO TO 430
C
C
C     EVALUATE THE ITERATION MATRIX
      IF (JCALC.NE.-1) GO TO 310
      IWM(LNJE)=IWM(LNJE)+1
      JCALC=0
      CALL DDAJAC(NEQ,X,Y,YPRIME,DELTA,CJ,H,
     *   IER,WT,E,WM,IWM,RES,IRES,
     *   UROUND,JAC,RPAR,IPAR,NTEMP)
C
      S=1000000.D0
      IF (IRES.LT.0) GO TO 430
      IF (IER.NE.0) GO TO 430
      NSF=0
C
C
C
C     MULTIPLY RESIDUAL BY DAMPING FACTOR
310   CONTINUE
      DO 320 I=1,NEQ
320      DELTA(I)=DELTA(I)*DAMP
C
C     COMPUTE A NEW ITERATE (BACK SUBSTITUTION)
C     STORE THE CORRECTION IN DELTA
C
      CALL DDASLV(NEQ,DELTA,WM,IWM)
C
C     UPDATE Y AND YPRIME
      DO 330 I=1,NEQ
         Y(I)=Y(I)-DELTA(I)
330      YPRIME(I)=YPRIME(I)-CJ*DELTA(I)
C
C     TEST FOR CONVERGENCE OF THE ITERATION.
C
      DELNRM=DDANRM(NEQ,DELTA,WT,RPAR,IPAR)
      IF (DELNRM.LE.100.D0*UROUND*YNORM)
     *   GO TO 400
C
      IF (M.GT.0) GO TO 340
         OLDNRM=DELNRM
         GO TO 350
C
340   RATE=(DELNRM/OLDNRM)**(1.0D0/DFLOAT(M))
      IF (RATE.GT.0.90D0) GO TO 430
      S=RATE/(1.0D0-RATE)
C
350   IF (S*DELNRM .LE. 0.33D0) GO TO 400
C
C
C     THE CORRECTOR HAS NOT YET CONVERGED. UPDATE
C     M AND AND TEST WHETHER THE MAXIMUM
C     NUMBER OF ITERATIONS HAVE BEEN TRIED.
C     EVERY MJAC ITERATIONS, GET A NEW
C     ITERATION MATRIX.
C
      M=M+1
      IF (M.GE.MAXIT) GO TO 430
C
      IF ((M/MJAC)*MJAC.EQ.M) JCALC=-1
      GO TO 300
C
C
C     THE ITERATION HAS CONVERGED.
C     CHECK NONNEGATIVITY CONSTRAINTS
400   IF (NONNEG.EQ.0) GO TO 450
      DO 410 I=1,NEQ
410      DELTA(I)=DMIN1(Y(I),0.0D0)
C
      DELNRM=DDANRM(NEQ,DELTA,WT,RPAR,IPAR)
      IF (DELNRM.GT.0.33D0) GO TO 430
C
      DO 420 I=1,NEQ
         Y(I)=Y(I)-DELTA(I)
420      YPRIME(I)=YPRIME(I)-CJ*DELTA(I)
      GO TO 450
C
C
C     EXITS FROM CORRECTOR LOOP.
430   CONVGD=.FALSE.
450   IF (.NOT.CONVGD) GO TO 600
C
C
C
C-----------------------------------------------------
C     BLOCK 3.
C     THE CORRECTOR ITERATION CONVERGED.
C     DO ERROR TEST.
C-----------------------------------------------------
C
      DO 510 I=1,NEQ
510      E(I)=Y(I)-PHI(I,1)
      ERR=DDANRM(NEQ,E,WT,RPAR,IPAR)
C
      IF (ERR.LE.1.0D0) RETURN
C
C
C
C--------------------------------------------------------
C     BLOCK 4.
C     THE BACKWARD EULER STEP FAILED. RESTORE X, Y
C     AND YPRIME TO THEIR ORIGINAL VALUES.
C     REDUCE STEPSIZE AND TRY AGAIN, IF
C     POSSIBLE.
C---------------------------------------------------------
C
600   CONTINUE
      X = XOLD
      DO 610 I=1,NEQ
         Y(I)=PHI(I,1)
610      YPRIME(I)=PHI(I,2)
C
      IF (CONVGD) GO TO 640
      IF (IER.EQ.0) GO TO 620
         NSF=NSF+1
         H=H*0.25D0
         IF (NSF.LT.3.AND.DABS(H).GE.HMIN) GO TO 690
         IDID=-12
         RETURN
620   IF (IRES.GT.-2) GO TO 630
         IDID=-12
         RETURN
630   NCF=NCF+1
      H=H*0.25D0
      IF (NCF.LT.10.AND.DABS(H).GE.HMIN) GO TO 690
         IDID=-12
         RETURN
C
640   NEF=NEF+1
      R=0.90D0/(2.0D0*ERR+0.0001D0)
      R=DMAX1(0.1D0,DMIN1(0.5D0,R))
      H=H*R
      IF (DABS(H).GE.HMIN.AND.NEF.LT.10) GO TO 690
         IDID=-12
         RETURN
690      GO TO 200
C
C-------------END OF SUBROUTINE DDAINI----------------------
      END
      SUBROUTINE DDATRP(X,XOUT,YOUT,YPOUT,NEQ,KOLD,PHI,PSI)
C
C***BEGIN PROLOGUE  DDATRP
C***REFER TO  DDASSL
C***ROUTINES CALLED  (NONE)
C***DATE WRITTEN   830315   (YYMMDD)
C***REVISION DATE  891228   (YYMMDD)
C***END PROLOGUE  DDATRP
C
C-----------------------------------------------------------------------
C     THE METHODS IN SUBROUTINE DDASTP USE POLYNOMIALS
C     TO APPROXIMATE THE SOLUTION. DDATRP APPROXIMATES THE
C     SOLUTION AND ITS DERIVATIVE AT TIME XOUT BY EVALUATING
C     ONE OF THESE POLYNOMIALS,AND ITS DERIVATIVE,THERE.
C     INFORMATION DEFINING THIS POLYNOMIAL IS PASSED FROM
C     DDASTP, SO DDATRP CANNOT BE USED ALONE.
C
C     THE PARAMETERS ARE:
C     X     THE CURRENT TIME IN THE INTEGRATION.
C     XOUT  THE TIME AT WHICH THE SOLUTION IS DESIRED
C     YOUT  THE INTERPOLATED APPROXIMATION TO Y AT XOUT
C           (THIS IS OUTPUT)
C     YPOUT THE INTERPOLATED APPROXIMATION TO YPRIME AT XOUT
C           (THIS IS OUTPUT)
C     NEQ   NUMBER OF EQUATIONS
C     KOLD  ORDER USED ON LAST SUCCESSFUL STEP
C     PHI   ARRAY OF SCALED DIVIDED DIFFERENCES OF Y
C     PSI   ARRAY OF PAST STEPSIZE HISTORY
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION YOUT(*),YPOUT(*)
      DIMENSION PHI(NEQ,*),PSI(*)
C
C*** ADDED BY GPF 11/21/91 IN CASE THE "NO REMEMBER"
C
      SAVE
C
      KOLDP1=KOLD+1
      TEMP1=XOUT-X
      DO 10 I=1,NEQ
         YOUT(I)=PHI(I,1)
10       YPOUT(I)=0.0D0
      C=1.0D0
      D=0.0D0
      GAMMA=TEMP1/PSI(1)
      DO 30 J=2,KOLDP1
         D=D*GAMMA+C/PSI(J-1)
         C=C*GAMMA
         GAMMA=(TEMP1+PSI(J-1))/PSI(J)
         DO 20 I=1,NEQ
            YOUT(I)=YOUT(I)+C*PHI(I,J)
20          YPOUT(I)=YPOUT(I)+D*PHI(I,J)
30       CONTINUE
      RETURN
C
C------END OF SUBROUTINE DDATRP------
      END
      SUBROUTINE DDASTP(X,Y,YPRIME,NEQ,
     *  RES,JAC,H,WT,JSTART,IDID,RPAR,IPAR,
     *  PHI,DELTA,E,WM,IWM,
     *  ALPHA,BETA,GAMMA,PSI,SIGMA,
     *  CJ,CJOLD,HOLD,S,HMIN,UROUND,
     *  IPHASE,JCALC,K,KOLD,NS,NONNEG,NTEMP)
C
C***BEGIN PROLOGUE  DDASTP
C***REFER TO  DDASSL
C***ROUTINES CALLED  DDANRM,DDAJAC,DDASLV,DDATRP
C***DATE WRITTEN   830315   (YYMMDD)
C***REVISION DATE  891228   (YYMMDD)
C***END PROLOGUE  DDASTP
C
C
C-----------------------------------------------------------------------
C     DDASTP SOLVES A SYSTEM OF DIFFERENTIAL/
C     ALGEBRAIC EQUATIONS OF THE FORM
C     G(X,Y,YPRIME) = 0,  FOR ONE STEP (NORMALLY
C     FROM X TO X+H).
C
C     THE METHODS USED ARE MODIFIED DIVIDED
C     DIFFERENCE,FIXED LEADING COEFFICIENT
C     FORMS OF BACKWARD DIFFERENTIATION
C     FORMULAS. THE CODE ADJUSTS THE STEPSIZE
C     AND ORDER TO CONTROL THE LOCAL ERROR PER
C     STEP.
C
C
C     THE PARAMETERS REPRESENT
C     X  --        INDEPENDENT VARIABLE
C     Y  --        SOLUTION VECTOR AT X
C     YPRIME --    DERIVATIVE OF SOLUTION VECTOR
C                  AFTER SUCCESSFUL STEP
C     NEQ --       NUMBER OF EQUATIONS TO BE INTEGRATED
C     RES --       EXTERNAL USER-SUPPLIED SUBROUTINE
C                  TO EVALUATE THE RESIDUAL.  THE CALL IS
C                  CALL RES(X,Y,YPRIME,DELTA,IRES,RPAR,IPAR)
C                  X,Y,YPRIME ARE INPUT.  DELTA IS OUTPUT.
C                  ON INPUT, IRES=0.  RES SHOULD ALTER IRES ONLY
C                  IF IT ENCOUNTERS AN ILLEGAL VALUE OF Y OR A
C                  STOP CONDITION.  SET IRES=-1 IF AN INPUT VALUE
C                  OF Y IS ILLEGAL, AND DDASTP WILL TRY TO SOLVE
C                  THE PROBLEM WITHOUT GETTING IRES = -1.  IF
C                  IRES=-2, DDASTP RETURNS CONTROL TO THE CALLING
C                  PROGRAM WITH IDID = -11.
C     JAC --       EXTERNAL USER-SUPPLIED ROUTINE TO EVALUATE
C                  THE ITERATION MATRIX (THIS IS OPTIONAL)
C                  THE CALL IS OF THE FORM
C                  CALL JAC(X,Y,YPRIME,PD,CJ,RPAR,IPAR)
C                  PD IS THE MATRIX OF PARTIAL DERIVATIVES,
C                  PD=DG/DY+CJ*DG/DYPRIME
C     H --         APPROPRIATE STEP SIZE FOR NEXT STEP.
C                  NORMALLY DETERMINED BY THE CODE
C     WT --        VECTOR OF WEIGHTS FOR ERROR CRITERION.
C     JSTART --    INTEGER VARIABLE SET 0 FOR
C                  FIRST STEP, 1 OTHERWISE.
C     IDID --      COMPLETION CODE WITH THE FOLLOWING MEANINGS:
C                  IDID= 1 -- THE STEP WAS COMPLETED SUCCESSFULLY
C                  IDID=-6 -- THE ERROR TEST FAILED REPEATEDLY
C                  IDID=-7 -- THE CORRECTOR COULD NOT CONVERGE
C                  IDID=-8 -- THE ITERATION MATRIX IS SINGULAR
C                  IDID=-9 -- THE CORRECTOR COULD NOT CONVERGE.
C                             THERE WERE REPEATED ERROR TEST
C                             FAILURES ON THIS STEP.
C                  IDID=-10-- THE CORRECTOR COULD NOT CONVERGE
C                             BECAUSE IRES WAS EQUAL TO MINUS ONE
C                  IDID=-11-- IRES EQUAL TO -2 WAS ENCOUNTERED,
C                             AND CONTROL IS BEING RETURNED TO
C                             THE CALLING PROGRAM
C     RPAR,IPAR -- REAL AND INTEGER PARAMETER ARRAYS THAT
C                  ARE USED FOR COMMUNICATION BETWEEN THE
C                  CALLING PROGRAM AND EXTERNAL USER ROUTINES
C                  THEY ARE NOT ALTERED BY DDASTP
C     PHI --       ARRAY OF DIVIDED DIFFERENCES USED BY
C                  DDASTP. THE LENGTH IS NEQ*(K+1),WHERE
C                  K IS THE MAXIMUM ORDER
C     DELTA,E --   WORK VECTORS FOR DDASTP OF LENGTH NEQ
C     WM,IWM --    REAL AND INTEGER ARRAYS STORING
C                  MATRIX INFORMATION SUCH AS THE MATRIX
C                  OF PARTIAL DERIVATIVES,PERMUTATION
C                  VECTOR,AND VARIOUS OTHER INFORMATION.
C
C     THE OTHER PARAMETERS ARE INFORMATION
C     WHICH IS NEEDED INTERNALLY BY DDASTP TO
C     CONTINUE FROM STEP TO STEP.
C
C-----------------------------------------------------------------------
C
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL CONVGD
      DIMENSION Y(*),YPRIME(*),WT(*)
      DIMENSION PHI(NEQ,*),DELTA(*),E(*)
      DIMENSION WM(*),IWM(*)
      DIMENSION PSI(*),ALPHA(*),BETA(*),GAMMA(*),SIGMA(*)
      DIMENSION RPAR(*),IPAR(*)
      EXTERNAL RES,JAC
C
      PARAMETER (LMXORD=3)
      PARAMETER (LNST=11)
      PARAMETER (LNRE=12)
      PARAMETER (LNJE=13)
      PARAMETER (LETF=14)
      PARAMETER (LCTF=15)
C
C
C*** ADDED BY GPF 11/21/91 IN CASE THE "NO REMEMBER"
C
      SAVE
C
      DATA MAXIT/4/
      DATA XRATE/0.25D0/
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 1.
C     INITIALIZE. ON THE FIRST CALL,SET
C     THE ORDER TO 1 AND INITIALIZE
C     OTHER VARIABLES.
C-----------------------------------------------------------------------
C
C     INITIALIZATIONS FOR ALL CALLS
      IDID=1
      XOLD=X
      NCF=0
      NSF=0
      NEF=0
      IF(JSTART .NE. 0) GO TO 120
C
C     IF THIS IS THE FIRST STEP,PERFORM
C     OTHER INITIALIZATIONS
      IWM(LETF) = 0
      IWM(LCTF) = 0
      K=1
      KOLD=0
      HOLD=0.0D0
      JSTART=1
      PSI(1)=H
      CJOLD = 1.0D0/H
      CJ = CJOLD
      S = 100.D0
      JCALC = -1
      DELNRM=1.0D0
      IPHASE = 0
      NS=0
120   CONTINUE
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 2
C     COMPUTE COEFFICIENTS OF FORMULAS FOR
C     THIS STEP.
C-----------------------------------------------------------------------
200   CONTINUE
      KP1=K+1
      KP2=K+2
      KM1=K-1
      XOLD=X
      IF(H.NE.HOLD.OR.K .NE. KOLD) NS = 0
      NS=MIN0(NS+1,KOLD+2)
      NSP1=NS+1
      IF(KP1 .LT. NS)GO TO 230
C
      BETA(1)=1.0D0
      ALPHA(1)=1.0D0
      TEMP1=H
      GAMMA(1)=0.0D0
      SIGMA(1)=1.0D0
      DO 210 I=2,KP1
         TEMP2=PSI(I-1)
         PSI(I-1)=TEMP1
         BETA(I)=BETA(I-1)*PSI(I-1)/TEMP2
         TEMP1=TEMP2+H
         ALPHA(I)=H/TEMP1
         SIGMA(I)=DFLOAT(I-1)*SIGMA(I-1)*ALPHA(I)
         GAMMA(I)=GAMMA(I-1)+ALPHA(I-1)/H
210      CONTINUE
      PSI(KP1)=TEMP1
230   CONTINUE
C
C     COMPUTE ALPHAS, ALPHA0
      ALPHAS = 0.0D0
      ALPHA0 = 0.0D0
      DO 240 I = 1,K
        ALPHAS = ALPHAS - 1.0D0/DFLOAT(I)
        ALPHA0 = ALPHA0 - ALPHA(I)
240     CONTINUE
C
C     COMPUTE LEADING COEFFICIENT CJ
      CJLAST = CJ
      CJ = -ALPHAS/H
C
C     COMPUTE VARIABLE STEPSIZE ERROR COEFFICIENT CK
      CK = DABS(ALPHA(KP1) + ALPHAS - ALPHA0)
      CK = DMAX1(CK,ALPHA(KP1))
C
C     DECIDE WHETHER NEW JACOBIAN IS NEEDED
      TEMP1 = (1.0D0 - XRATE)/(1.0D0 + XRATE)
      TEMP2 = 1.0D0/TEMP1
      IF (CJ/CJOLD .LT. TEMP1 .OR. CJ/CJOLD .GT. TEMP2) JCALC = -1
      IF (CJ .NE. CJLAST) S = 100.D0
C
C     CHANGE PHI TO PHI STAR
      IF(KP1 .LT. NSP1) GO TO 280
      DO 270 J=NSP1,KP1
         DO 260 I=1,NEQ
260         PHI(I,J)=BETA(J)*PHI(I,J)
270      CONTINUE
280   CONTINUE
C
C     UPDATE TIME
      X=X+H
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 3
C     PREDICT THE SOLUTION AND DERIVATIVE,
C     AND SOLVE THE CORRECTOR EQUATION
C-----------------------------------------------------------------------
C
C     FIRST,PREDICT THE SOLUTION AND DERIVATIVE
300   CONTINUE
      DO 310 I=1,NEQ
         Y(I)=PHI(I,1)
310      YPRIME(I)=0.0D0
      DO 330 J=2,KP1
         DO 320 I=1,NEQ
            Y(I)=Y(I)+PHI(I,J)
320         YPRIME(I)=YPRIME(I)+GAMMA(J)*PHI(I,J)
330   CONTINUE
      PNORM = DDANRM (NEQ,Y,WT,RPAR,IPAR)
C
C
C
C     SOLVE THE CORRECTOR EQUATION USING A
C     MODIFIED NEWTON SCHEME.
      CONVGD= .TRUE.
      M=0
      IWM(LNRE)=IWM(LNRE)+1
      IRES = 0
C
C*** WE'RE ABOUT TO COMPUTE A JACOBIAN SO SET THE IDERV
C    FLAG, TELLING RES TO COMPUTE JACOBIAN ELEMENTS
C
      CALL SETDERV(0)
C
      CALL RES(X,Y,YPRIME,DELTA,IRES,RPAR,IPAR)
      CALL SETDERV(0)
      IF (IRES .LT. 0) GO TO 380
C
C
C     IF INDICATED,REEVALUATE THE
C     ITERATION MATRIX PD = DG/DY + CJ*DG/DYPRIME
C     (WHERE G(X,Y,YPRIME)=0). SET
C     JCALC TO 0 AS AN INDICATOR THAT
C     THIS HAS BEEN DONE.
      IF(JCALC .NE. -1)GO TO 340
      IWM(LNJE)=IWM(LNJE)+1
      JCALC=0
      CALL DDAJAC(NEQ,X,Y,YPRIME,DELTA,CJ,H,
     * IER,WT,E,WM,IWM,RES,IRES,UROUND,JAC,RPAR,
     * IPAR,NTEMP)
      CJOLD=CJ
      S = 100.D0
      IF (IRES .LT. 0) GO TO 380
      IF(IER .NE. 0)GO TO 380
      NSF=0
C
C
C     INITIALIZE THE ERROR ACCUMULATION VECTOR E.
340   CONTINUE
      DO 345 I=1,NEQ
345      E(I)=0.0D0
C
C
C     CORRECTOR LOOP.
350   CONTINUE
C
C     MULTIPLY RESIDUAL BY TEMP1 TO ACCELERATE CONVERGENCE
      TEMP1 = 2.0D0/(1.0D0 + CJ/CJOLD)
      DO 355 I = 1,NEQ
355     DELTA(I) = DELTA(I) * TEMP1
C
C     COMPUTE A NEW ITERATE (BACK-SUBSTITUTION).
C     STORE THE CORRECTION IN DELTA.
      CALL DDASLV(NEQ,DELTA,WM,IWM)
C
C     UPDATE Y,E,AND YPRIME
      DO 360 I=1,NEQ
         Y(I)=Y(I)-DELTA(I)
         E(I)=E(I)-DELTA(I)
360      YPRIME(I)=YPRIME(I)-CJ*DELTA(I)
C
C     TEST FOR CONVERGENCE OF THE ITERATION
      DELNRM=DDANRM(NEQ,DELTA,WT,RPAR,IPAR)
      IF (DELNRM .LE. 100.D0*UROUND*PNORM) GO TO 375
      IF (M .GT. 0) GO TO 365
         OLDNRM = DELNRM
         GO TO 367
365   RATE = (DELNRM/OLDNRM)**(1.0D0/DFLOAT(M))
      IF (RATE .GT. 0.90D0) GO TO 370
      S = RATE/(1.0D0 - RATE)
367   IF (S*DELNRM .LE. 0.33D0) GO TO 375
C
C     THE CORRECTOR HAS NOT YET CONVERGED.
C     UPDATE M AND TEST WHETHER THE
C     MAXIMUM NUMBER OF ITERATIONS HAVE
C     BEEN TRIED.
      M=M+1
      IF(M.GE.MAXIT)GO TO 370
C
C     EVALUATE THE RESIDUAL
C     AND GO BACK TO DO ANOTHER ITERATION
      IWM(LNRE)=IWM(LNRE)+1
      IRES = 0
      CALL RES(X,Y,YPRIME,DELTA,IRES,
     *  RPAR,IPAR)
      IF (IRES .LT. 0) GO TO 380
      GO TO 350
C
C
C     THE CORRECTOR FAILED TO CONVERGE IN MAXIT
C     ITERATIONS. IF THE ITERATION MATRIX
C     IS NOT CURRENT,RE-DO THE STEP WITH
C     A NEW ITERATION MATRIX.
370   CONTINUE
      IF(JCALC.EQ.0)GO TO 380
      JCALC=-1
      GO TO 300
C
C
C     THE ITERATION HAS CONVERGED.  IF NONNEGATIVITY OF SOLUTION IS
C     REQUIRED, SET THE SOLUTION NONNEGATIVE, IF THE PERTURBATION
C     TO DO IT IS SMALL ENOUGH.  IF THE CHANGE IS TOO LARGE, THEN
C     CONSIDER THE CORRECTOR ITERATION TO HAVE FAILED.
375   IF(NONNEG .EQ. 0) GO TO 390
      DO 377 I = 1,NEQ
377      DELTA(I) = DMIN1(Y(I),0.0D0)
      DELNRM = DDANRM(NEQ,DELTA,WT,RPAR,IPAR)
      IF(DELNRM .GT. 0.33D0) GO TO 380
      DO 378 I = 1,NEQ
378      E(I) = E(I) - DELTA(I)
      GO TO 390
C
C
C     EXITS FROM BLOCK 3
C     NO CONVERGENCE WITH CURRENT ITERATION
C     MATRIX,OR SINGULAR ITERATION MATRIX
380   CONVGD= .FALSE.
390   JCALC = 1
      IF(.NOT.CONVGD)GO TO 600
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 4
C     ESTIMATE THE ERRORS AT ORDERS K,K-1,K-2
C     AS IF CONSTANT STEPSIZE WAS USED. ESTIMATE
C     THE LOCAL ERROR AT ORDER K AND TEST
C     WHETHER THE CURRENT STEP IS SUCCESSFUL.
C-----------------------------------------------------------------------
C
C     ESTIMATE ERRORS AT ORDERS K,K-1,K-2
      ENORM = DDANRM(NEQ,E,WT,RPAR,IPAR)
      ERK = SIGMA(K+1)*ENORM
      TERK = FLOAT(K+1)*ERK
      EST = ERK
      KNEW=K
      IF(K .EQ. 1)GO TO 430
      DO 405 I = 1,NEQ
405     DELTA(I) = PHI(I,KP1) + E(I)
      ERKM1=SIGMA(K)*DDANRM(NEQ,DELTA,WT,RPAR,IPAR)
      TERKM1 = FLOAT(K)*ERKM1
      IF(K .GT. 2)GO TO 410
      IF(TERKM1 .LE. 0.5D0*TERK)GO TO 420
      GO TO 430
410   CONTINUE
      DO 415 I = 1,NEQ
415     DELTA(I) = PHI(I,K) + DELTA(I)
      ERKM2=SIGMA(K-1)*DDANRM(NEQ,DELTA,WT,RPAR,IPAR)
      TERKM2 = FLOAT(K-1)*ERKM2
      IF(DMAX1(TERKM1,TERKM2).GT.TERK)GO TO 430
C     LOWER THE ORDER
420   CONTINUE
      KNEW=K-1
      EST = ERKM1
C
C
C     CALCULATE THE LOCAL ERROR FOR THE CURRENT STEP
C     TO SEE IF THE STEP WAS SUCCESSFUL
430   CONTINUE
      ERR = CK * ENORM
      IF(ERR .GT. 1.0D0)GO TO 600
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 5
C     THE STEP IS SUCCESSFUL. DETERMINE
C     THE BEST ORDER AND STEPSIZE FOR
C     THE NEXT STEP. UPDATE THE DIFFERENCES
C     FOR THE NEXT STEP.
C-----------------------------------------------------------------------
      IDID=1
      IWM(LNST)=IWM(LNST)+1
      KDIFF=K-KOLD
      KOLD=K
      HOLD=H
C
C
C     ESTIMATE THE ERROR AT ORDER K+1 UNLESS:
C        ALREADY DECIDED TO LOWER ORDER, OR
C        ALREADY USING MAXIMUM ORDER, OR
C        STEPSIZE NOT CONSTANT, OR
C        ORDER RAISED IN PREVIOUS STEP
      IF(KNEW.EQ.KM1.OR.K.EQ.IWM(LMXORD))IPHASE=1
      IF(IPHASE .EQ. 0)GO TO 545
      IF(KNEW.EQ.KM1)GO TO 540
      IF(K.EQ.IWM(LMXORD)) GO TO 550
      IF(KP1.GE.NS.OR.KDIFF.EQ.1)GO TO 550
      DO 510 I=1,NEQ
510      DELTA(I)=E(I)-PHI(I,KP2)
      ERKP1 = (1.0D0/DFLOAT(K+2))*DDANRM(NEQ,DELTA,WT,RPAR,IPAR)
      TERKP1 = FLOAT(K+2)*ERKP1
      IF(K.GT.1)GO TO 520
      IF(TERKP1.GE.0.5D0*TERK)GO TO 550
      GO TO 530
520   IF(TERKM1.LE.DMIN1(TERK,TERKP1))GO TO 540
      IF(TERKP1.GE.TERK.OR.K.EQ.IWM(LMXORD))GO TO 550
C
C     RAISE ORDER
530   K=KP1
      EST = ERKP1
      GO TO 550
C
C     LOWER ORDER
540   K=KM1
      EST = ERKM1
      GO TO 550
C
C     IF IPHASE = 0, INCREASE ORDER BY ONE AND MULTIPLY STEPSIZE BY
C     FACTOR TWO
545   K = KP1
      HNEW = H*2.0D0
      H = HNEW
      GO TO 575
C
C
C     DETERMINE THE APPROPRIATE STEPSIZE FOR
C     THE NEXT STEP.
550   HNEW=H
      TEMP2=K+1
      R=(2.0D0*EST+0.0001D0)**(-1.0D0/TEMP2)
      IF(R .LT. 2.0D0) GO TO 555
      HNEW = 2.0D0*H
      GO TO 560
555   IF(R .GT. 1.0D0) GO TO 560
      R = DMAX1(0.5D0,DMIN1(0.9D0,R))
      HNEW = H*R
560   H=HNEW
C
C
C     UPDATE DIFFERENCES FOR NEXT STEP
575   CONTINUE
      IF(KOLD.EQ.IWM(LMXORD))GO TO 585
      DO 580 I=1,NEQ
580      PHI(I,KP2)=E(I)
585   CONTINUE
      DO 590 I=1,NEQ
590      PHI(I,KP1)=PHI(I,KP1)+E(I)
      DO 595 J1=2,KP1
         J=KP1-J1+1
         DO 595 I=1,NEQ
595      PHI(I,J)=PHI(I,J)+PHI(I,J+1)
      RETURN
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 6
C     THE STEP IS UNSUCCESSFUL. RESTORE X,PSI,PHI
C     DETERMINE APPROPRIATE STEPSIZE FOR
C     CONTINUING THE INTEGRATION, OR EXIT WITH
C     AN ERROR FLAG IF THERE HAVE BEEN MANY
C     FAILURES.
C-----------------------------------------------------------------------
600   IPHASE = 1
C
C     RESTORE X,PHI,PSI
      X=XOLD
      IF(KP1.LT.NSP1)GO TO 630
      DO 620 J=NSP1,KP1
         TEMP1=1.0D0/BETA(J)
         DO 610 I=1,NEQ
610         PHI(I,J)=TEMP1*PHI(I,J)
620      CONTINUE
630   CONTINUE
      DO 640 I=2,KP1
640      PSI(I-1)=PSI(I)-H
C
C
C     TEST WHETHER FAILURE IS DUE TO CORRECTOR ITERATION
C     OR ERROR TEST
      IF(CONVGD)GO TO 660
      IWM(LCTF)=IWM(LCTF)+1
C
C
C     THE NEWTON ITERATION FAILED TO CONVERGE WITH
C     A CURRENT ITERATION MATRIX.  DETERMINE THE CAUSE
C     OF THE FAILURE AND TAKE APPROPRIATE ACTION.
      IF(IER.EQ.0)GO TO 650
C
C     THE ITERATION MATRIX IS SINGULAR. REDUCE
C     THE STEPSIZE BY A FACTOR OF 4. IF
C     THIS HAPPENS THREE TIMES IN A ROW ON
C     THE SAME STEP, RETURN WITH AN ERROR FLAG
      NSF=NSF+1
      R = 0.25D0
      H=H*R
      IF (NSF .LT. 3 .AND. DABS(H) .GE. HMIN) GO TO 690
      IDID=-8
      GO TO 675
C
C
C     THE NEWTON ITERATION FAILED TO CONVERGE FOR A REASON
C     OTHER THAN A SINGULAR ITERATION MATRIX.  IF IRES = -2, THEN
C     RETURN.  OTHERWISE, REDUCE THE STEPSIZE AND TRY AGAIN, UNLESS
C     TOO MANY FAILURES HAVE OCCURED.
650   CONTINUE
      IF (IRES .GT. -2) GO TO 655
      IDID = -11
      GO TO 675
655   NCF = NCF + 1
      R = 0.25D0
      H = H*R
      IF (NCF .LT. 10 .AND. DABS(H) .GE. HMIN) GO TO 690
      IDID = -7
      IF (IRES .LT. 0) IDID = -10
      IF (NEF .GE. 3) IDID = -9
      GO TO 675
C
C
C     THE NEWTON SCHEME CONVERGED,AND THE CAUSE
C     OF THE FAILURE WAS THE ERROR ESTIMATE
C     EXCEEDING THE TOLERANCE.
660   NEF=NEF+1
      IWM(LETF)=IWM(LETF)+1
      IF (NEF .GT. 1) GO TO 665
C
C     ON FIRST ERROR TEST FAILURE, KEEP CURRENT ORDER OR LOWER
C     ORDER BY ONE.  COMPUTE NEW STEPSIZE BASED ON DIFFERENCES
C     OF THE SOLUTION.
      K = KNEW
      TEMP2 = K + 1
      R = 0.90D0*(2.0D0*EST+0.0001D0)**(-1.0D0/TEMP2)
      R = DMAX1(0.25D0,DMIN1(0.9D0,R))
      H = H*R
      IF (DABS(H) .GE. HMIN) GO TO 690
      IDID = -6
      GO TO 675
C
C     ON SECOND ERROR TEST FAILURE, USE THE CURRENT ORDER OR
C     DECREASE ORDER BY ONE.  REDUCE THE STEPSIZE BY A FACTOR OF
C     ONE QUARTER.
665   IF (NEF .GT. 2) GO TO 670
      K = KNEW
      H = 0.25D0*H
      IF (DABS(H) .GE. HMIN) GO TO 690
      IDID = -6
      GO TO 675
C
C     ON THIRD AND SUBSEQUENT ERROR TEST FAILURES, SET THE ORDER TO
C     ONE AND REDUCE THE STEPSIZE BY A FACTOR OF ONE QUARTER
670   K = 1
      H = 0.25D0*H
      IF (DABS(H) .GE. HMIN) GO TO 690
      IDID = -6
      GO TO 675
C
C
C
C
C     FOR ALL CRASHES, RESTORE Y TO ITS LAST VALUE,
C     INTERPOLATE TO FIND YPRIME AT LAST X, AND RETURN
675   CONTINUE
      CALL DDATRP(X,X,Y,YPRIME,NEQ,K,PHI,PSI)
      RETURN
C
C
C     GO BACK AND TRY THIS STEP AGAIN
690   GO TO 200
C
C------END OF SUBROUTINE DDASTP------
      END
      SUBROUTINE DDAJAC(NEQ,X,Y,YPRIME,DELTA,CJ,H,
     *  IER,WT,E,WM,IWM,RES,IRES,UROUND,JAC,RPAR,
     *  IPAR,NTEMP)
C
C***BEGIN PROLOGUE  DDAJAC
C***REFER TO  DDASSL
C***ROUTINES CALLED  DGEFA,DGBFA
C***DATE WRITTEN   830315   (YYMMDD)
C***REVISION DATE  891228   (YYMMDD)
C***END PROLOGUE  DDAJAC
C-----------------------------------------------------------------------
C     THIS ROUTINE COMPUTES THE ITERATION MATRIX
C     PD=DG/DY+CJ*DG/DYPRIME (WHERE G(X,Y,YPRIME)=0).
C     HERE PD IS COMPUTED BY THE USER-SUPPLIED
C     ROUTINE JAC IF IWM(MTYPE) IS 1 OR 4, AND
C     IT IS COMPUTED BY NUMERICAL FINITE DIFFERENCING
C     IF IWM(MTYPE)IS 2 OR 5
C     THE PARAMETERS HAVE THE FOLLOWING MEANINGS.
C     Y        = ARRAY CONTAINING PREDICTED VALUES
C     YPRIME   = ARRAY CONTAINING PREDICTED DERIVATIVES
C     DELTA    = RESIDUAL EVALUATED AT (X,Y,YPRIME)
C                (USED ONLY IF IWM(MTYPE)=2 OR 5)
C     CJ       = SCALAR PARAMETER DEFINING ITERATION MATRIX
C     H        = CURRENT STEPSIZE IN INTEGRATION
C     IER      = VARIABLE WHICH IS .NE. 0
C                IF ITERATION MATRIX IS SINGULAR,
C                AND 0 OTHERWISE.
C     WT       = VECTOR OF WEIGHTS FOR COMPUTING NORMS
C     E        = WORK SPACE (TEMPORARY) OF LENGTH NEQ
C     WM       = REAL WORK SPACE FOR MATRICES. ON
C                OUTPUT IT CONTAINS THE LU DECOMPOSITION
C                OF THE ITERATION MATRIX.
C     IWM      = INTEGER WORK SPACE CONTAINING
C                MATRIX INFORMATION
C     RES      = NAME OF THE EXTERNAL USER-SUPPLIED ROUTINE
C                TO EVALUATE THE RESIDUAL FUNCTION G(X,Y,YPRIME)
C     IRES     = FLAG WHICH IS EQUAL TO ZERO IF NO ILLEGAL VALUES
C                IN RES, AND LESS THAN ZERO OTHERWISE.  (IF IRES
C                IS LESS THAN ZERO, THE MATRIX WAS NOT COMPLETED)
C                IN THIS CASE (IF IRES .LT. 0), THEN IER = 0.
C     UROUND   = THE UNIT ROUNDOFF ERROR OF THE MACHINE BEING USED.
C     JAC      = NAME OF THE EXTERNAL USER-SUPPLIED ROUTINE
C                TO EVALUATE THE ITERATION MATRIX (THIS ROUTINE
C                IS ONLY USED IF IWM(MTYPE) IS 1 OR 4)
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      EXTERNAL RES,JAC
      DIMENSION Y(*),YPRIME(*),DELTA(*),WT(*),E(*)
      DIMENSION WM(*),IWM(*),RPAR(*),IPAR(*)
C
      PARAMETER (NPD=1)
      PARAMETER (LML=1)
      PARAMETER (LMU=2)
      PARAMETER (LMTYPE=4)
      PARAMETER (LIPVT=21)
C
C*** ADDED BY GPF 11/21/91 IN CASE THE "NO REMEMBER"
C
      SAVE
C
C
      IER = 0
      NPDM1=NPD-1
      MTYPE=IWM(LMTYPE)
C
C     MODIFIED BY PAR 01/08/93 TO ALLOW CUSTOM SOLUTION FOR CFAST
C
      GO TO (100,200,300,400,500,600),MTYPE
C
C
C     DENSE USER-SUPPLIED MATRIX
100   LENPD=NEQ*NEQ
      DO 110 I=1,LENPD
110      WM(NPDM1+I)=0.0D0
      CALL JAC(X,Y,YPRIME,WM(NPD),CJ,RPAR,IPAR)
      GO TO 230
C
C
C     DENSE FINITE-DIFFERENCE-GENERATED MATRIX
200   IRES=0
      NROW=NPDM1
      SQUR = DSQRT(UROUND)
      DO 210 I=1,NEQ
         DEL=SQUR*DMAX1(DABS(Y(I)),DABS(H*YPRIME(I)),
     *     DABS(WT(I)))
         DEL=DSIGN(DEL,H*YPRIME(I))
         DEL=(Y(I)+DEL)-Y(I)
         YSAVE=Y(I)
         YPSAVE=YPRIME(I)
         Y(I)=Y(I)+DEL
         YPRIME(I)=YPRIME(I)+CJ*DEL
         CALL SETDERV(I)
         CALL RES(X,Y,YPRIME,E,IRES,RPAR,IPAR)
         IF (IRES .LT. 0) RETURN
         DELINV=1.0D0/DEL
         DO 220 L=1,NEQ
220      WM(NROW+L)=(E(L)-DELTA(L))*DELINV
      NROW=NROW+NEQ
      Y(I)=YSAVE
      YPRIME(I)=YPSAVE
210   CONTINUE
      CALL SETDERV(-1)
C
C*** CODE ADDED BY GPF AND PAR TO INCREMENT JACOBIAN COUNT
C    AND PRINT OUT JACOBIAN
C
      CALL INCJAC
      CALL OUTJAC(X,WM(NPD),NEQ)
C
C
C
C     DO DENSE-MATRIX LU DECOMPOSITION ON PD
230      CALL DGEFA(WM(NPD),NEQ,NEQ,IWM(LIPVT),IER)
      RETURN
C
C
C     DUMMY SECTION FOR IWM(MTYPE)=3
300   RETURN
C
C
C     BANDED USER-SUPPLIED MATRIX
400   LENPD=(2*IWM(LML)+IWM(LMU)+1)*NEQ
      DO 410 I=1,LENPD
410      WM(NPDM1+I)=0.0D0
      CALL JAC(X,Y,YPRIME,WM(NPD),CJ,RPAR,IPAR)
      MEBAND=2*IWM(LML)+IWM(LMU)+1
      GO TO 550
C
C
C     BANDED FINITE-DIFFERENCE-GENERATED MATRIX
500   MBAND=IWM(LML)+IWM(LMU)+1
      MBA=MIN0(MBAND,NEQ)
      MEBAND=MBAND+IWM(LML)
      MEB1=MEBAND-1
      MSAVE=(NEQ/MBAND)+1
      ISAVE=NTEMP-1
      IPSAVE=ISAVE+MSAVE
      IRES=0
      SQUR=DSQRT(UROUND)
      DO 540 J=1,MBA
         DO 510 N=J,NEQ,MBAND
          K= (N-J)/MBAND + 1
          WM(ISAVE+K)=Y(N)
          WM(IPSAVE+K)=YPRIME(N)
          DEL=SQUR*DMAX1(DABS(Y(N)),DABS(H*YPRIME(N)),
     *      DABS(WT(N)))
          DEL=DSIGN(DEL,H*YPRIME(N))
          DEL=(Y(N)+DEL)-Y(N)
          Y(N)=Y(N)+DEL
510       YPRIME(N)=YPRIME(N)+CJ*DEL
      CALL RES(X,Y,YPRIME,E,IRES,RPAR,IPAR)
      IF (IRES .LT. 0) RETURN
      DO 530 N=J,NEQ,MBAND
          K= (N-J)/MBAND + 1
          Y(N)=WM(ISAVE+K)
          YPRIME(N)=WM(IPSAVE+K)
          DEL=SQUR*DMAX1(DABS(Y(N)),DABS(H*YPRIME(N)),
     *      DABS(WT(N)))
          DEL=DSIGN(DEL,H*YPRIME(N))
          DEL=(Y(N)+DEL)-Y(N)
          DELINV=1.0D0/DEL
          I1=MAX0(1,(N-IWM(LMU)))
          I2=MIN0(NEQ,(N+IWM(LML)))
          II=N*MEB1-IWM(LML)+NPDM1
          DO 520 I=I1,I2
520         WM(II+I)=(E(I)-DELTA(I))*DELINV
530      CONTINUE
540   CONTINUE
C
C
C     DO LU DECOMPOSITION OF BANDED PD
550   CALL DGBFA(WM(NPD),MEBAND,NEQ,
     *    IWM(LML),IWM(LMU),IWM(LIPVT),IER)
      RETURN
C
C
C     CUSTOM GENERATION OF JACOBIAN FOR CFAST
C     ADDED BY PAR 01/08/93
C     REMOVED BY GPF 2/2/96 (obsolete)
600   CONTINUE
      RETURN
C------END OF SUBROUTINE DDAJAC------
      END
      SUBROUTINE DDASLV(NEQ,DELTA,WM,IWM)
C
C***BEGIN PROLOGUE  DDASLV
C***REFER TO  DDASSL
C***ROUTINES CALLED DGESL,DGBSL
C***DATE WRITTEN   830315   (YYMMDD)
C***REVISION DATE  891228   (YYMMDD)
C***END PROLOGUE  DDASLV
C-----------------------------------------------------------------------
C     THIS ROUTINE MANAGES THE SOLUTION OF THE LINEAR
C     SYSTEM ARISING IN THE NEWTON ITERATION.
C     MATRICES AND REAL TEMPORARY STORAGE AND
C     REAL INFORMATION ARE STORED IN THE ARRAY WM.
C     INTEGER MATRIX INFORMATION IS STORED IN
C     THE ARRAY IWM.
C     FOR A DENSE MATRIX, THE LINPACK ROUTINE
C     DGESL IS CALLED.
C     FOR A BANDED MATRIX,THE LINPACK ROUTINE
C     DGBSL IS CALLED.
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DELTA(*),WM(*),IWM(*)
C
      PARAMETER (NPD=1)
      PARAMETER (LML=1)
      PARAMETER (LMU=2)
      PARAMETER (LMTYPE=4)
      PARAMETER (LIPVT=21)
C
C
C*** ADDED BY GPF 11/21/91 IN CASE THE "NO REMEMBER"
C
      SAVE
C
      MTYPE=IWM(LMTYPE)
C
C     MODIFIED BY PAR 01/08/93 FOR CUSTOM SOLUTION FOR CFAST
C
      GO TO(100,100,300,400,400,600),MTYPE
C
C     DENSE MATRIX
100   CALL DGESL(WM(NPD),NEQ,NEQ,IWM(LIPVT),DELTA,0)
      RETURN
C
C     DUMMY SECTION FOR MTYPE=3
300   CONTINUE
      RETURN
C
C     BANDED MATRIX
400   MEBAND=2*IWM(LML)+IWM(LMU)+1
      CALL DGBSL(WM(NPD),MEBAND,NEQ,IWM(LML),
     *  IWM(LMU),IWM(LIPVT),DELTA,0)
      RETURN
C
C     ADDED BY PAR 01/08/93 FOR CUSTOM SOLUTION OF CFAST
600   CONTINUE
      RETURN
C------END OF SUBROUTINE DDASLV------
      END

      SUBROUTINE SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA)
C***BEGIN PROLOGUE  SNSQE
C***DATE WRITTEN   800301   (YYMMDD)
C***REVISION DATE  840405   (YYMMDD)
C***CATEGORY NO.  F2A
C***KEYWORDS  EASY-TO-USE,NONLINEAR SQUARE SYSTEM,POWELL HYBRID METHOD,
C             ZERO
C***AUTHOR  HIEBERT, K. L., (SNLA)
C***PURPOSE  SNSQE is the easy-to-use version of SNSQ which finds a zero
C            of a system of N nonlinear functions in N variables by a
C            modification of Powell hybrid method.  This code is the
C            combination of the MINPACK codes(Argonne) HYBRD1 and HYBRJ1
C***DESCRIPTION
C
C 1. Purpose.
C
C
C       The purpose of SNSQE is to find a zero of a system of N non-
C       linear functions in N variables by a modification of the Powell
C       hybrid method.  This is done by using the more general nonlinear
C       equation solver SNSQ.  The user must provide a subroutine which
C       calculates the functions.  The user has the option of either to
C       provide a subroutine which calculates the Jacobian or to let the
C       code calculate it by a forward-difference approximation.  This
C       code is the combination of the MINPACK codes (Argonne) HYBRD1
C       and HYBRJ1.
C
C
C 2. Subroutine and Type Statements.
C
C       SUBROUTINE SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,
C      *                  WA,LWA)
C       INTEGER IOPT,N,NPRINT,INFO,LWA
C       REAL TOL
C       REAL X(N),FVEC(N),WA(LWA)
C       EXTERNAL FCN,JAC
C
C
C 3. Parameters.
C
C       Parameters designated as input parameters must be specified on
C       entry to SNSQE and are not changed on exit, while parameters
C       designated as output parameters need not be specified on entry
C       and are set to appropriate values on exit from SNSQE.
C
C       FCN is the name of the user-supplied subroutine which calculates
C         the functions.  FCN must be declared in an EXTERNAL statement
C         in the user calling program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         REAL X(N),FVEC(N)
C         ----------
C         Calculate the functions at X and
C         return this vector in FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless the
C         user wants to terminate execution of SNSQE.  In this case, set
C         IFLAG to a negative integer.
C
C       JAC is the name of the user-supplied subroutine which calculates
C         the Jacobian.  If IOPT=1, then JAC must be declared in an
C         EXTERNAL statement in the user calling program, and should be
C         written as follows.
C
C         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
C         INTEGER N,LDFJAC,IFLAG
C         REAL X(N),FVEC(N),FJAC(LDFJAC,N)
C         ----------
C         Calculate the Jacobian at X and return this
C         matrix in FJAC.  FVEC contains the function
C         values at X and should not be altered.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by JAC unless the
C         user wants to terminate execution of SNSQE.  In this case, set
C         IFLAG to a negative integer.
C
C         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
C
C       IOPT is an input variable which specifies how the Jacobian will
C         be calculated.  If IOPT=1, then the user must supply the
C         Jacobian through the subroutine JAC.  If IOPT=2, then the
C         code will approximate the Jacobian by forward-differencing.
C
C       N is a positive integer input variable set to the number of
C         functions and variables.
C
C       X is an array of length N.  On input, X must contain an initial
C         estimate of the solution vector.  On output, X contains the
C         final estimate of the solution vector.
C
C       FVEC is an output array of length N which contains the functions
C         evaluated at the output X.
C
C       TOL is a non-negative input variable.  Termination occurs when
C         the algorithm estimates that the relative error between X and
C         the solution is at most TOL.  Section 4 contains more details
C         about TOL.
C
C       NPRINT is an integer input variable that enables controlled
C         printing of iterates if it is positive.  In this case, FCN is
C         called with IFLAG = 0 at the beginning of the first iteration
C         and every NPRINT iteration thereafter and immediately prior
C         to return, with X and FVEC available for printing. Appropriate
C         print statements must be added to FCN (see example). If NPRINT
C         is not positive, no special calls of FCN with IFLAG = 0 are
C         made.
C
C       INFO is an integer output variable.  If the user has terminated
C         execution, INFO is set to the (negative) value of IFLAG.  See
C         description of FCN and JAC. Otherwise, INFO is set as follows.
C
C         INFO = 0  improper input parameters.
C
C         INFO = 1  algorithm estimates that the relative error between
C                   X and the solution is at most TOL.
C
C         INFO = 2  number of calls to FCN has reached or exceeded
C                   100*(N+1) for IOPT=1 or 200*(N+1) for IOPT=2.
C
C         INFO = 3  TOL is too small.  No further improvement in the
C                   approximate solution X is possible.
C
C         INFO = 4  iteration is not making good progress.
C
C         Sections 4 and 5 contain more details about INFO.
C
C       WA is a work array of length LWA.
C
C       LWA is a positive integer input variable not less than
C         (3*N**2+13*N))/2.
C
C
C 4. Successful Completion.
C
C       The accuracy of SNSQE is controlled by the convergence parame-
C       ter TOL.  This parameter is used in a test which makes a compar-
C       ison between the approximation X and a solution XSOL.  SNSQE
C       terminates when the test is satisfied.  If TOL is less than the
C       machine precision (as defined by the function R1MACH(4)), then
C       SNSQE attemps only to satisfy the test defined by the machine
C       precision.  Further progress is not usually possible.  Unless
C       high precision solutions are required, the recommended value
C       for TOL is the square root of the machine precision.
C
C       The test assumes that the functions are reasonably well behaved,
C       and, if the Jacobian is supplied by the user, that the functions
C       and the Jacobian  coded consistently.  If these conditions
C       are not satisfied, SNSQE may incorrectly indicate convergence.
C       The coding of the Jacobian can be checked by the subroutine
C       CHKDER.  If the Jacobian is coded correctly or IOPT=2, then
C       the validity of the answer can be checked, for example, by
C       rerunning SNSQE with a tighter tolerance.
C
C       Convergence Test.  If ENORM(Z) denotes the Euclidean norm of a
C         vector Z, then this test attempts to guarantee that
C
C               ENORM(X-XSOL) .LE.  TOL*ENORM(XSOL).
C
C         If this condition is satisfied with TOL = 10**(-K), then the
C         larger components of X have K significant decimal digits and
C         INFO is set to 1.  There is a danger that the smaller compo-
C         nents of X may have large relative errors, but the fast rate
C         of convergence of SNSQE usually avoids this possibility.
C
C
C 5. Unsuccessful Completion.
C
C       Unsuccessful termination of SNSQE can be due to improper input
C       parameters, arithmetic interrupts, an excessive number of func-
C       tion evaluations, errors in the functions, or lack of good prog-
C       ress.
C
C       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1, or
C         IOPT .GT. 2, or N .LE. 0, or TOL .LT. 0.E0, or
C         LWA .LT. (3*N**2+13*N)/2.
C
C       Arithmetic Interrupts.  If these interrupts occur in the FCN
C         subroutine during an early stage of the computation, they may
C         be caused by an unacceptable choice of X by SNSQE.  In this
C         case, it may be possible to remedy the situation by not evalu-
C         ating the functions here, but instead setting the components
C         of FVEC to numbers that exceed those in the initial FVEC.
C
C       Excessive Number of Function Evaluations.  If the number of
C         calls to FCN reaches 100*(N+1) for IOPT=1 or 200*(N+1) for
C         IOPT=2, then this indicates that the routine is converging
C         very slowly as measured by the progress of FVEC, and INFO is
C         set to 2.  This situation should be unusual because, as
C         indicated below, lack of good progress is usually diagnosed
C         earlier by SNSQE, causing termination with INFO = 4.
C
C       Errors in the Functions.  When IOPT=2, the choice of step length
C         in the forward-difference approximation to the Jacobian
C         assumes that the relative errors in the functions are of the
C         order of the machine precision.  If this is not the case,
C         SNSQE may fail (usually with INFO = 4).  The user should
C         then either use SNSQ and set the step length or use IOPT=1
C         and supply the Jacobian.
C
C       Lack of Good Progress.  SNSQE searches for a zero of the system
C         by minimizing the sum of the squares of the functions.  In so
C         doing, it can become trapped in a region where the minimum
C         does not correspond to a zero of the system and, in this situ-
C         ation, the iteration eventually fails to make good progress.
C         In particular, this will happen if the system does not have a
C         zero.  If the system has a zero, rerunning SNSQE from a dif-
C         ferent starting point may be helpful.
C
C
C 6. Characteristics of the Algorithm.
C
C       SNSQE is a modification of the Powell hybrid method.  Two of
C       its main characteristics involve the choice of the correction as
C       a convex combination of the Newton and scaled gradient direc-
C       tions, and the updating of the Jacobian by the rank-1 method of
C       Broyden.  The choice of the correction guarantees (under reason-
C       able conditions) global convergence for starting points far from
C       the solution and a fast rate of convergence.  The Jacobian is
C       calculated at the starting point by either the user-supplied
C       subroutine or a forward-difference approximation, but it is not
C       recalculated until the rank-1 method fails to produce satis-
C       factory progress.
C
C       Timing.  The time required by SNSQE to solve a given problem
C         depends on N, the behavior of the functions, the accuracy
C         requested, and the starting point.  The number of arithmetic
C         operations needed by SNSQE is about 11.5*(N**2) to process
C         each evaluation of the functions (call to FCN) and 1.3*(N**3)
C         to process each evaluation of the Jacobian (call to JAC,
C         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
C         the timing of SNSQE will be strongly influenced by the time
C         spent in FCN and JAC.
C
C       Storage.  SNSQE requires (3*N**2 + 17*N)/2 single precision
C         storage locations, in addition to the storage required by the
C         program.  There are no internally declared storage arrays.
C
C
C 7. Example.
C
C       The problem is to determine the values of X(1), X(2), ..., X(9),
C       which solve the system of tridiagonal equations
C
C       (3-2*X(1))*X(1)           -2*X(2)                   = -1
C               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
C                                   -X(8) + (3-2*X(9))*X(9) = -1
C
C       **********
C
C       PROGRAM TEST(INPUT,OUTPUT,TAPE6=OUTPUT)
C C
C C     Driver for SNSQE example.
C C
C       INTEGER J,N,IOPT,NPRINT,INFO,LWA,NWRITE
C       REAL TOL,FNORM
C       REAL X(9),FVEC(9),WA(180)
C       REAL ENORM,R1MACH
C       EXTERNAL FCN
C       DATA NWRITE /6/
C C
C       IOPT = 2
C       N = 9
C C
C C     The following starting values provide a rough solution.
C C
C       DO 10 J = 1, 9
C          X(J) = -1.E0
C    10    CONTINUE
C
C       LWA = 180
C       NPRINT = 0
C C
C C     Set TOL to the square root of the machine precision.
C C     Unless high precision solutions are required,
C C     this is the recommended setting.
C C
C       TOL = SQRT(R1MACH(4))
C C
C       CALL SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA)
C       FNORM = ENORM(N,FVEC)
C       WRITE (NWRITE,1000) FNORM,INFO,(X(J),J=1,N)
C       STOP
C  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
C      *        5X,' EXIT PARAMETER',16X,I10 //
C      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
C       END
C       SUBROUTINE FCN(N,X,FVEC,IFLAG)
C       INTEGER N,IFLAG
C       REAL X(N),FVEC(N)
C       INTEGER K
C       REAL ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
C       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
C C
C       DO 10 K = 1, N
C          TEMP = (THREE - TWO*X(K))*X(K)
C          TEMP1 = ZERO
C          IF (K .NE. 1) TEMP1 = X(K-1)
C          TEMP2 = ZERO
C          IF (K .NE. N) TEMP2 = X(K+1)
C          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
C    10    CONTINUE
C       RETURN
C       END
C
C       Results obtained with different compilers or machines
C       may be slightly different.
C
C       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
C
C       EXIT PARAMETER                         1
C
C       FINAL APPROXIMATE SOLUTION
C
C       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
C       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
C       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
C***REFERENCES  POWELL, M. J. D.
C                 A HYBRID METHOD FOR NONLINEAR EQUATIONS.
C                 NUMERICAL METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS,
C                 P. RABINOWITZ, EDITOR.  GORDON AND BREACH, 1970.
C***ROUTINES CALLED  SNSQ,XERROR
C***END PROLOGUE  SNSQE
      include "precis.fi"
      INTEGER IOPT,N,NPRINT,INFO,LWA
      DIMENSION X(N),FVEC(N),WA(LWA)
      EXTERNAL FCN,JAC
      INTEGER INDEX,J,LR,MAXFEV,ML,MODE,MU,NFEV,NJEV
      DATA FACTOR,ONE,ZERO /1.0D2,1.0D0,0.0D0/
C***FIRST EXECUTABLE STATEMENT  SNSQE
      INFO = 0
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0
     1    .OR. TOL .LT. ZERO .OR. LWA .LT. (3*N**2 +13*N)/2)
     2   GO TO 20
C
C     CALL SNSQ.
C
      MAXFEV = 100*(N + 1)
      IF (IOPT .EQ. 2) MAXFEV = 2*MAXFEV
      XTOL = TOL
      ML = N - 1
      MU = N - 1
      EPSFCN = ZERO
      MODE = 2
      DO 10 J = 1, N
         WA(J) = ONE
   10    CONTINUE
      LR = (N*(N + 1))/2
      INDEX=6*N+LR
      CALL SNSQ(FCN,JAC,IOPT,N,X,FVEC,WA(INDEX+1),N,XTOL,MAXFEV,ML,MU,
     1           EPSFCN,WA(1),MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
     2           WA(6*N+1),LR,WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),
     3           WA(5*N+1))
      IF (INFO .EQ. 5) INFO = 4
   20 CONTINUE
      IF (INFO .EQ. 0) CALL XERROR( 'SNSQE  -- INVALID INPUT PARAMETER.'
     1,34,2,1)
      RETURN
C
C     LAST CARD OF SUBROUTINE SNSQE.
C
      END
      SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
C***BEGIN PROLOGUE  DOGLEG
C***REFER TO  SNSQ,SNSQE
C
C     **********
C     Subroutine DOGLEG
C
C     Given an M by N matrix A, an N by N nonsingular DIAGONAL
C     matrix D, an M-vector B, and a positive number DELTA, the
C     problem is to determine the convex combination X of the
C     Gauss-Newton and scaled gradient directions that minimizes
C     (A*X - B) in the least squares sense, subject to the
C     restriction that the Euclidean norm of D*X be at most DELTA.
C
C     This subroutine completes the solution of the problem
C     if it is provided with the necessary information from the
C     QR factorization of A. That is, if A = Q*R, where Q has
C     orthogonal columns and R is an upper triangular matrix,
C     then DOGLEG expects the full upper triangle of R and
C     the first N components of (Q TRANSPOSE)*B.
C
C     The subroutine statement is
C
C       SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
C
C     where
C
C       N is a positive integer input variable set to the order of R.
C
C       R is an input array of length LR which must contain the upper
C         triangular matrix R stored by rows.
C
C       LR is a positive integer input variable not less than
C         (N*(N+1))/2.
C
C       DIAG is an input array of length N which must contain the
C         diagonal elements of the matrix D.
C
C       QTB is an input array of length N which must contain the first
C         N elements of the vector (Q TRANSPOSE)*B.
C
C       DELTA is a positive input variable which specifies an upper
C         bound on the Euclidean norm of D*X.
C
C       X is an output array of length N which contains the desired
C         convex combination of the Gauss-Newton direction and the
C         scaled gradient direction.
C
C       WA1 and WA2 are work arrays of length N.
C
C     Subprograms called
C
C       MINPACK-supplied ... R1MACH,ENORM
C
C       FORTRAN-supplied ... ABS,MAX,MIN,SQRT
C
C     MINPACK. Version of July 1978.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  ENORM,R1MACH
C***END PROLOGUE  DOGLEG
      include "precis.fi"
      INTEGER N,LR
      DIMENSION R(LR),DIAG(N),QTB(N),X(N),WA1(N),WA2(N)
      INTEGER I,J,JJ,JP1,K,L
      DATA ONE,ZERO /1.0D0,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DOGLEG
      EPSMCH = D1MACH(4)
C
C     FIRST, CALCULATE THE GAUSS-NEWTON DIRECTION.
C
      JJ = (N*(N + 1))/2 + 1
      DO 50 K = 1, N
         J = N - K + 1
         JP1 = J + 1
         JJ = JJ - K
         L = JJ + 1
         SUM = ZERO
         IF (N .LT. JP1) GO TO 20
         DO 10 I = JP1, N
            SUM = SUM + R(L)*X(I)
            L = L + 1
   10       CONTINUE
   20    CONTINUE
         TEMP = R(JJ)
         IF (TEMP .NE. ZERO) GO TO 40
         L = J
         DO 30 I = 1, J
            TEMP = MAX(TEMP,ABS(R(L)))
            L = L + N - I
   30       CONTINUE
         TEMP = EPSMCH*TEMP
         IF (TEMP .EQ. ZERO) TEMP = EPSMCH
   40    CONTINUE
         X(J) = (QTB(J) - SUM)/TEMP
   50    CONTINUE
C
C     TEST WHETHER THE GAUSS-NEWTON DIRECTION IS ACCEPTABLE.
C
      DO 60 J = 1, N
         WA1(J) = ZERO
         WA2(J) = DIAG(J)*X(J)
   60    CONTINUE
      QNORM = ENORM(N,WA2)
      IF (QNORM .LE. DELTA) GO TO 140
C
C     THE GAUSS-NEWTON DIRECTION IS NOT ACCEPTABLE.
C     NEXT, CALCULATE THE SCALED GRADIENT DIRECTION.
C
      L = 1
      DO 80 J = 1, N
         TEMP = QTB(J)
         DO 70 I = J, N
            WA1(I) = WA1(I) + R(L)*TEMP
            L = L + 1
   70       CONTINUE
         WA1(J) = WA1(J)/DIAG(J)
   80    CONTINUE
C
C     CALCULATE THE NORM OF THE SCALED GRADIENT DIRECTION,
C     NORMALIZE, AND RESCALE THE GRADIENT.
C
      GNORM = ENORM(N,WA1)
      SGNORM = ZERO
      ALPHA = DELTA/QNORM
      IF (GNORM .EQ. ZERO) GO TO 120
      DO 90 J = 1, N
         WA1(J) = (WA1(J)/GNORM)/DIAG(J)
   90    CONTINUE
C
C     CALCULATE THE POINT ALONG THE SCALED GRADIENT
C     AT WHICH THE QUADRATIC IS MINIMIZED.
C
      L = 1
      DO 110 J = 1, N
         SUM = ZERO
         DO 100 I = J, N
            SUM = SUM + R(L)*WA1(I)
            L = L + 1
  100       CONTINUE
         WA2(J) = SUM
  110    CONTINUE
      TEMP = ENORM(N,WA2)
      SGNORM = (GNORM/TEMP)/TEMP
C
C     TEST WHETHER THE SCALED GRADIENT DIRECTION IS ACCEPTABLE.
C
      ALPHA = ZERO
      IF (SGNORM .GE. DELTA) GO TO 120
C
C     THE SCALED GRADIENT DIRECTION IS NOT ACCEPTABLE.
C     FINALLY, CALCULATE THE POINT ALONG THE DOGLEG
C     AT WHICH THE QUADRATIC IS MINIMIZED.
C
      BNORM = ENORM(N,QTB)
      TEMP = (BNORM/GNORM)*(BNORM/QNORM)*(SGNORM/DELTA)
      TEMP = TEMP - (DELTA/QNORM)*(SGNORM/DELTA)**2
     1       + SQRT((TEMP-(DELTA/QNORM))**2
     2              +(ONE-(DELTA/QNORM)**2)*(ONE-(SGNORM/DELTA)**2))
      ALPHA = ((DELTA/QNORM)*(ONE - (SGNORM/DELTA)**2))/TEMP
  120 CONTINUE
C
C     FORM APPROPRIATE CONVEX COMBINATION OF THE GAUSS-NEWTON
C     DIRECTION AND THE SCALED GRADIENT DIRECTION.
C
      TEMP = (ONE - ALPHA)*MIN(SGNORM,DELTA)
      DO 130 J = 1, N
         X(J) = TEMP*WA1(J) + ALPHA*X(J)
  130    CONTINUE
  140 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DOGLEG.
C
      END
      DOUBLE PRECISION FUNCTION ENORM(N,X)
C***BEGIN PROLOGUE  ENORM
C***REFER TO  SNLS1,SNLS1E,SNSQ,SNSQE
C
C     **********
C     Function ENORM
C
C     Given an N-vector X, this function calculates the
C     Euclidean norm of X.
C
C     The Euclidean norm is computed by accumulating the sum of
C     squares in three different sums. The sums of squares for the
C     small and large components are scaled so that no overflows
C     occur. Non-destructive underflows are permitted. Underflows
C     and overflows do not occur in the computation of the unscaled
C     sum of squares for the intermediate components.
C     The definitions of small, intermediate and large components
C     depend on two constants, RDWARF and RGIANT. The main
C     restrictions on these constants are that RDWARF**2 not
C     underflow and RGIANT**2 not overflow. The constants
C     given here are suitable for every known computer.
C
C     The function statement is
C
C       REAL FUNCTION ENORM(N,X)
C
C     where
C
C       N is a positive integer input variable.
C
C       X is an input array of length N.
C
C     Subprograms called
C
C       FORTRAN-supplied ... ABS,SQRT
C
C     MINPACK. Version of October 1979.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  ENORM
      include "precis.fi"
      INTEGER N
      DIMENSION X(N)
      INTEGER I
      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/
C***FIRST EXECUTABLE STATEMENT  ENORM
      S1 = ZERO
      S2 = ZERO
      S3 = ZERO
      X1MAX = ZERO
      X3MAX = ZERO
      FLOATN = N
      AGIANT = RGIANT/FLOATN
      DO 90 I = 1, N
         XABS = ABS(X(I))
         IF (XABS .GT. RDWARF .AND. XABS .LT. AGIANT) GO TO 70
            IF (XABS .LE. RDWARF) GO TO 30
C
C              SUM FOR LARGE COMPONENTS.
C
               IF (XABS .LE. X1MAX) GO TO 10
                  S1 = ONE + S1*(X1MAX/XABS)**2
                  X1MAX = XABS
                  GO TO 20
   10          CONTINUE
                  S1 = S1 + (XABS/X1MAX)**2
   20          CONTINUE
               GO TO 60
   30       CONTINUE
C
C              SUM FOR SMALL COMPONENTS.
C
               IF (XABS .LE. X3MAX) GO TO 40
                  S3 = ONE + S3*(X3MAX/XABS)**2
                  X3MAX = XABS
                  GO TO 50
   40          CONTINUE
                  IF (XABS .NE. ZERO) S3 = S3 + (XABS/X3MAX)**2
   50          CONTINUE
   60       CONTINUE
            GO TO 80
   70    CONTINUE
C
C           SUM FOR INTERMEDIATE COMPONENTS.
C
            S2 = S2 + XABS**2
   80    CONTINUE
   90    CONTINUE
C
C     CALCULATION OF NORM.
C
      IF (S1 .EQ. ZERO) GO TO 100
         ENORM = X1MAX*SQRT(S1+(S2/X1MAX)/X1MAX)
         GO TO 130
  100 CONTINUE
         IF (S2 .EQ. ZERO) GO TO 110
            IF (S2 .GE. X3MAX)
     1         ENORM = SQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
            IF (S2 .LT. X3MAX)
     1         ENORM = SQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
            GO TO 120
  110    CONTINUE
            ENORM = X3MAX*SQRT(S3)
  120    CONTINUE
  130 CONTINUE
      RETURN
C
C     LAST CARD OF FUNCTION ENORM.
C
      END
      SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,
     1   WA1,WA2)
C***BEGIN PROLOGUE  FDJAC1
C***REFER TO  SNSQ,SNSQE
C
C              Subroutine FDJAC1
C
C     This subroutine computes a forward-difference approximation
C     to the N by N Jacobian matrix associated with a specified
C     problem of N functions in N VARIABLES. If the Jacobian has
C     a banded form, then function evaluations are saved by only
C     approximating the nonzero terms.
C
C     The subroutine statement is
C
C       SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,
C                         WA1,WA2)
C
C     where
C
C       FCN is the name of the user-supplied subroutine which
C         calculates the functions. FCN must be declared
C         in an external statement in the user calling
C         program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         REAL X(N),FVEC(N)
C         ----------
C         Calculate the functions at X and
C         return this vector in FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless
C         the user wants to terminate execution of FDJAC1.
C         In this case set IFLAG to a negative integer.
C
C       N Is a positive integer input variable set to the number
C         of functions and variables.
C
C       X is an input array of length N.
C
C       FVEC is an input array of length N which must contain the
C         functions evaluated at X.
C
C       FJAC is an output N by N array which contains the
C         approximation to the Jacobian matrix evaluated at X.
C
C       LDFJAC is a positive integer input variable not less than N
C         which specifies the leading dimension of the array FJAC.
C
C       IFLAG is an integer variable which can be used to terminate
C         the execution of FDJAC1. See description of FCN.
C
C       ML is a nonnegative integer input variable which specifies
C         the number of subdiagonals within the band of the
C         Jacobian matrix. If the Jacobian is not banded, set
C         ML to at least N - 1.
C
C       EPSFCN is an input variable used in determining a suitable
C         step length for the forward-difference approximation. This
C         approximation assumes that the relative errors in the
C         functions are of the order of EPSFCN. If EPSFCN is less
C         than the machine precision, it is assumed that the relative
C         errors in the functions are of the order of the machine
C         precision.
C
C       MU is a nonnegative integer input variable which specifies
C         the number of superdiagonals within the band of the
C         Jacobian matrix. If the Jacobian is not banded, set
C         MU to at least N - 1.
C
C       WA1 and WA2 are work arrays of length N. If ML + MU + 1 is at
C         least N, then the Jacobian is considered dense, and WA2 is
C         not referenced.
C
C     Subprograms called
C
C       MINPACK-supplied ... R1MACH
C
C       FORTRAN-supplied ... ABS,MAX,SQRT
C
C     MINPACK. Version of June 1979.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  R1MACH
C***END PROLOGUE  FDJAC1
      include "precis.fi"
      INTEGER N,LDFJAC,IFLAG,ML,MU
      DIMENSION X(N),FVEC(N),FJAC(LDFJAC,N),WA1(N),WA2(N)
      INTEGER I,J,K,MSUM
      DATA ZERO /0.0D0/
C***FIRST EXECUTABLE STATEMENT  FDJAC1
      EPSMCH = D1MACH(4)
C
      EPS = SQRT(MAX(EPSFCN,EPSMCH))
      MSUM = ML + MU + 1
      IF (MSUM .LT. N) GO TO 40
C
C        COMPUTATION OF DENSE APPROXIMATE JACOBIAN.
C
         DO 20 J = 1, N
            TEMP = X(J)
            H = EPS*ABS(TEMP)
            IF (H .EQ. ZERO) H = EPS
            X(J) = TEMP + H
            CALL FCN(N,X,WA1,IFLAG)
            IF (IFLAG .LT. 0) GO TO 30
            X(J) = TEMP
            DO 10 I = 1, N
               FJAC(I,J) = (WA1(I) - FVEC(I))/H
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
         GO TO 110
   40 CONTINUE
C
C        COMPUTATION OF BANDED APPROXIMATE JACOBIAN.
C
         DO 90 K = 1, MSUM
            DO 60 J = K, N, MSUM
               WA2(J) = X(J)
               H = EPS*ABS(WA2(J))
               IF (H .EQ. ZERO) H = EPS
               X(J) = WA2(J) + H
   60          CONTINUE
            CALL FCN(N,X,WA1,IFLAG)
            IF (IFLAG .LT. 0) GO TO 100
            DO 80 J = K, N, MSUM
               X(J) = WA2(J)
               H = EPS*ABS(WA2(J))
               IF (H .EQ. ZERO) H = EPS
               DO 70 I = 1, N
                  FJAC(I,J) = ZERO
                  IF (I .GE. J - MU .AND. I .LE. J + ML)
     1               FJAC(I,J) = (WA1(I) - FVEC(I))/H
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE FDJAC1.
C
      END
      SUBROUTINE QFORM(M,N,Q,LDQ,WA)
C***BEGIN PROLOGUE  QFORM
C***REFER TO  SNSQ,SNSQE
C
C     **********
C     Subroutine QFORM
C
C     This subroutine proceeds from the computed QR factorization of
C     an M by N matrix A to accumulate the M by M orthogonal matrix
C     Q from its factored form.
C
C     The subroutine statement is
C
C       SUBROUTINE QFORM(M,N,Q,LDQ,WA)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of A and the order of Q.
C
C       N is a positive integer input variable set to the number
C         of columns of A.
C
C       Q is an M by M array. On input the full lower trapezoid in
C         the first min(M,N) columns of Q contains the factored form.
C         On output Q has been accumulated into a square matrix.
C
C       LDQ is a positive integer input variable not less than M
C         which specifies the leading dimension of the array Q.
C
C       WA is a work array of length M.
C
C     Subprograms called
C
C       FORTRAN-supplied ... MIN0
C
C     MINPACK. Version of January 1979.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  QFORM
      include "precis.fi"
      INTEGER M,N,LDQ
      DIMENSION Q(LDQ,M),WA(M)
      INTEGER I,J,JM1,K,L,MINMN,NP1
      DATA ONE,ZERO /1.0D0,0.0D0/
C***FIRST EXECUTABLE STATEMENT  QFORM
      MINMN = MIN0(M,N)
      IF (MINMN .LT. 2) GO TO 30
      DO 20 J = 2, MINMN
         JM1 = J - 1
         DO 10 I = 1, JM1
            Q(I,J) = ZERO
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
C
C     INITIALIZE REMAINING COLUMNS TO THOSE OF THE IDENTITY MATRIX.
C
      NP1 = N + 1
      IF (M .LT. NP1) GO TO 60
      DO 50 J = NP1, M
         DO 40 I = 1, M
            Q(I,J) = ZERO
   40       CONTINUE
         Q(J,J) = ONE
   50    CONTINUE
   60 CONTINUE
C
C     ACCUMULATE Q FROM ITS FACTORED FORM.
C
      DO 120 L = 1, MINMN
         K = MINMN - L + 1
         DO 70 I = K, M
            WA(I) = Q(I,K)
            Q(I,K) = ZERO
   70       CONTINUE
         Q(K,K) = ONE
         IF (WA(K) .EQ. ZERO) GO TO 110
         DO 100 J = K, M
            SUM = ZERO
            DO 80 I = K, M
               SUM = SUM + Q(I,J)*WA(I)
   80          CONTINUE
            TEMP = SUM/WA(K)
            DO 90 I = K, M
               Q(I,J) = Q(I,J) - TEMP*WA(I)
   90          CONTINUE
  100       CONTINUE
  110    CONTINUE
  120    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE QFORM.
C
      END
      SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,SIGMA,ACNORM,WA)
C***BEGIN PROLOGUE  QRFAC
C***REFER TO  SNLS1,SNLS1E,SNSQ,SNSQE
C
C     **********
C     Subroutine QRFAC
C
C     This subroutine uses Householder transformations with column
C     pivoting (optional) to compute a QR factorization of the
C     M by N matrix A. That is, QRFAC determines an orthogonal
C     matrix Q, a permutation matrix P, and an upper trapezoidal
C     matrix R with diagonal elements of nonincreasing magnitude,
C     such that A*P = Q*R. The Householder transformation for
C     column K, K = 1,2,...,MIN(M,N), is of the form
C
C                           T
C           I - (1/U(K))*U*U
C
C     where U has zeros in the first K-1 positions. The form of
C     this transformation and the method of pivoting first
C     appeared in the corresponding LINPACK subroutine.
C
C     The subroutine statement is
C
C       SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,SIGMA,ACNORM,WA)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of A.
C
C       N is a positive integer input variable set to the number
C         of columns of A.
C
C       A is an M by N array. On input A contains the matrix for
C         which the QR factorization is to be computed. On output
C         the strict upper trapezoidal part of A contains the strict
C         upper trapezoidal part of R, and the lower trapezoidal
C         part of A contains a factored form of Q (the non-trivial
C         elements of the U vectors described above).
C
C       LDA is a positive integer input variable not less than M
C         which specifies the leading dimension of the array A.
C
C       PIVOT is a logical input variable. If pivot is set .TRUE.,
C         then column pivoting is enforced. If pivot is set .FALSE.,
C         then no column pivoting is done.
C
C       IPVT is an integer output array of length LIPVT. IPVT
C         defines the permutation matrix P such that A*P = Q*R.
C         Column J of P is column IPVT(J) of the identity matrix.
C         If pivot is .FALSE., IPVT is not referenced.
C
C       LIPVT is a positive integer input variable. If PIVOT is
C             .FALSE., then LIPVT may be as small as 1. If PIVOT is
C             .TRUE., then LIPVT must be at least N.
C
C       SIGMA is an output array of length N which contains the
C         diagonal elements of R.
C
C       ACNORM is an output array of length N which contains the
C         norms of the corresponding columns of the input matrix A.
C         If this information is not needed, then ACNORM can coincide
C         with SIGMA.
C
C       WA is a work array of length N. If pivot is .FALSE., then WA
C         can coincide with SIGMA.
C
C     Subprograms called
C
C       MINPACK-Supplied ... R1MACH,ENORM
C       FORTRAN-Supplied ... MAX,SQRT,MIN0
C
C     MINPACK. Version of December 1978.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  ENORM,R1MACH
C***END PROLOGUE  QRFAC
      include "precis.fi"
      INTEGER M,N,LDA,LIPVT
      INTEGER IPVT(LIPVT)
      LOGICAL PIVOT
      DIMENSION A(LDA,N),SIGMA(N),ACNORM(N),WA(N)
      INTEGER I,J,JP1,K,KMAX,MINMN
      DATA ONE,P05,ZERO /1.0D0,5.0D-2,0.0D0/
C***FIRST EXECUTABLE STATEMENT  QRFAC
      EPSMCH = D1MACH(4)
C
C     COMPUTE THE INITIAL COLUMN NORMS AND INITIALIZE SEVERAL ARRAYS.
C
      DO 10 J = 1, N
         ACNORM(J) = ENORM(M,A(1,J))
         SIGMA(J) = ACNORM(J)
         WA(J) = SIGMA(J)
         IF (PIVOT) IPVT(J) = J
   10    CONTINUE
C
C     REDUCE A TO R WITH HOUSEHOLDER TRANSFORMATIONS.
C
      MINMN = MIN0(M,N)
      DO 110 J = 1, MINMN
         IF (.NOT.PIVOT) GO TO 40
C
C        BRING THE COLUMN OF LARGEST NORM INTO THE PIVOT POSITION.
C
         KMAX = J
         DO 20 K = J, N
            IF (SIGMA(K) .GT. SIGMA(KMAX)) KMAX = K
   20       CONTINUE
         IF (KMAX .EQ. J) GO TO 40
         DO 30 I = 1, M
            TEMP = A(I,J)
            A(I,J) = A(I,KMAX)
            A(I,KMAX) = TEMP
   30       CONTINUE
         SIGMA(KMAX) = SIGMA(J)
         WA(KMAX) = WA(J)
         K = IPVT(J)
         IPVT(J) = IPVT(KMAX)
         IPVT(KMAX) = K
   40    CONTINUE
C
C        COMPUTE THE HOUSEHOLDER TRANSFORMATION TO REDUCE THE
C        J-TH COLUMN OF A TO A MULTIPLE OF THE J-TH UNIT VECTOR.
C
         AJNORM = ENORM(M-J+1,A(J,J))
         IF (AJNORM .EQ. ZERO) GO TO 100
         IF (A(J,J) .LT. ZERO) AJNORM = -AJNORM
         DO 50 I = J, M
            A(I,J) = A(I,J)/AJNORM
   50       CONTINUE
         A(J,J) = A(J,J) + ONE
C
C        APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS
C        AND UPDATE THE NORMS.
C
         JP1 = J + 1
         IF (N .LT. JP1) GO TO 100
         DO 90 K = JP1, N
            SUM = ZERO
            DO 60 I = J, M
               SUM = SUM + A(I,J)*A(I,K)
   60          CONTINUE
            TEMP = SUM/A(J,J)
            DO 70 I = J, M
               A(I,K) = A(I,K) - TEMP*A(I,J)
   70          CONTINUE
            IF (.NOT.PIVOT .OR. SIGMA(K) .EQ. ZERO) GO TO 80
            TEMP = A(J,K)/SIGMA(K)
            SIGMA(K) = SIGMA(K)*SQRT(MAX(ZERO,ONE-TEMP**2))
            IF (P05*(SIGMA(K)/WA(K))**2 .GT. EPSMCH) GO TO 80
            SIGMA(K) = ENORM(M-J,A(JP1,K))
            WA(K) = SIGMA(K)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
         SIGMA(J) = -AJNORM
  110    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE QRFAC.
C
      END
      SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)
C***BEGIN PROLOGUE  R1MPYQ
C***REFER TO  SNSQ,SNSQE
C
C     **********
C     Subroutine R1MPYQ
C
C     Given an M by N matrix A, this subroutine computes A*Q where
C     Q is the product of 2*(N - 1) transformations
C
C           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
C
C     and GV(I), GW(I) are Givens rotations in the (I,N) plane which
C     eliminate elements in the I-th and N-th planes, respectively.
C     Q itself is not given, rather the information to recover the
C     GV, GW rotations is supplied.
C
C     The subroutine statement is
C
C       SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of A.
C
C       N is a positive integer input variable set to the number
C         of columns of A.
C
C       A is an M by N ARRAY. On input A must contain the matrix
C         to be postmultiplied by the orthogonal matrix Q
C         described above. On output A*Q has replaced A.
C
C       LDA is a positive integer input variable not less than M
C         which specifies the leading dimension of the array A.
C
C       V is an input array of length N. V(I) must contain the
C         information necessary to recover the Givens rotation GV(I)
C         described above.
C
C       W is an input array of length N. W(I) must contain the
C         information necessary to recover the Givens rotation GW(I)
C         described above.
C
C     Subroutines called
C
C       FORTRAN-Supplied ... abs,sqrt
C
C     MINPACK. Version of December 1978.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  R1MPYQ
      include "precis.fi"
      INTEGER M,N,LDA
      DIMENSION A(LDA,N),V(N),W(N)
      INTEGER I,J,NMJ,NM1
      DATA ONE /1.0D0/
C***FIRST EXECUTABLE STATEMENT  R1MPYQ
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 50
      DO 20 NMJ = 1, NM1
         J = N - NMJ
         IF (ABS(V(J)) .GT. ONE) COS = ONE/V(J)
         IF (ABS(V(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
         IF (ABS(V(J)) .LE. ONE) SIN = V(J)
         IF (ABS(V(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
         DO 10 I = 1, M
            TEMP = COS*A(I,J) - SIN*A(I,N)
            A(I,N) = SIN*A(I,J) + COS*A(I,N)
            A(I,J) = TEMP
   10       CONTINUE
   20    CONTINUE
C
C     APPLY THE SECOND SET OF GIVENS ROTATIONS TO A.
C
      DO 40 J = 1, NM1
         IF (ABS(W(J)) .GT. ONE) COS = ONE/W(J)
         IF (ABS(W(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
         IF (ABS(W(J)) .LE. ONE) SIN = W(J)
         IF (ABS(W(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
         DO 30 I = 1, M
            TEMP = COS*A(I,J) + SIN*A(I,N)
            A(I,N) = -SIN*A(I,J) + COS*A(I,N)
            A(I,J) = TEMP
   30       CONTINUE
   40    CONTINUE
   50 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE R1MPYQ.
C
      END
      SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)
C***BEGIN PROLOGUE  R1UPDT
C***REFER TO  SNSQ,SNSQE
C
C     **********
C     Subroutine R1UPDT
C
C     Given an M by N lower trapezoidal matrix S, an M-vector U,
C     and an N-vector V, the problem is to determine an
C     orthogonal matrix Q such that
C
C                   T
C           (S + U*V )*Q
C
C     is again lower trapezoidal.
C
C     This subroutine determines Q as the product of 2*(N - 1)
C     transformations
C
C           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
C
C     where GV(I), GW(I) are Givens rotations in the (I,N) plane
C     which eliminate elements in the I-th and N-th planes,
C     respectively. Q Itself is not accumulated, rather the
C     information to recover the GV, GW rotations is returned.
C
C     The subroutine statement is
C
C       SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of S.
C
C       N is a positive integer input variable set to the number
C         of columns of S. N must not exceed M.
C
C       S is an array of length LS. On input S must contain the lower
C         trapezoidal matrix S stored by columns. On output S contains
C         the lower trapezoidal matrix produced as described above.
C
C       LS is a positive integer input variable not less than
C         (N*(2*M-N+1))/2.
C
C       U is an input array of length M which must contain the
C         vector U.
C
C       V is an array of length N. On input V must contain the vector
C         V. On output V(I) contains the information necessary to
C         recover the Givens rotation GV(I) described above.
C
C       W is an output array of length M. W(I) contains information
C         necessary to recover the Givens rotation GW(I) described
C         above.
C
C       SING is a logical output variable. SING is set .TRUE. if any
C         of the diagonal elements of the output S are zero. Otherwise
C         SING is set .FALSE.
C
C     Subprograms called
C
C       MINPACK-supplied ... R1MACH
C       FORTRAN-supplied ... ABS,SQRT
C
C     MINPACK. Version of December 1978.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More,
C     John L. Nazareth
C     **********
C***ROUTINES CALLED  R1MACH
C***END PROLOGUE  R1UPDT
      include "precis.fi"
      INTEGER M,N,LS
      LOGICAL SING
      DIMENSION S(LS),U(M),V(N),W(M)
      INTEGER I,J,JJ,L,NMJ,NM1
      DATA ONE,P5,P25,ZERO /1.0D0,5.0D-1,2.5D-1,0.0D0/
C***FIRST EXECUTABLE STATEMENT  R1UPDT
      GIANT = D1MACH(2)
C
C     INITIALIZE THE DIAGONAL ELEMENT POINTER.
C
      JJ = (N*(2*M - N + 1))/2 - (M - N)
C
C     MOVE THE NONTRIVIAL PART OF THE LAST COLUMN OF S INTO W.
C
      L = JJ
      DO 10 I = N, M
         W(I) = S(L)
         L = L + 1
   10    CONTINUE
C
C     ROTATE THE VECTOR V INTO A MULTIPLE OF THE N-TH UNIT VECTOR
C     IN SUCH A WAY THAT A SPIKE IS INTRODUCED INTO W.
C
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 NMJ = 1, NM1
         J = N - NMJ
         JJ = JJ - (M - J + 1)
         W(J) = ZERO
         IF (V(J) .EQ. ZERO) GO TO 50
C
C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
C        J-TH ELEMENT OF V.
C
         IF (ABS(V(N)) .GE. ABS(V(J))) GO TO 20
            COTAN = V(N)/V(J)
            SIN = P5/SQRT(P25+P25*COTAN**2)
            COS = SIN*COTAN
            TAU = ONE
            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
            GO TO 30
   20    CONTINUE
            TAN = V(J)/V(N)
            COS = P5/SQRT(P25+P25*TAN**2)
            SIN = COS*TAN
            TAU = SIN
   30    CONTINUE
C
C        APPLY THE TRANSFORMATION TO V AND STORE THE INFORMATION
C        NECESSARY TO RECOVER THE GIVENS ROTATION.
C
         V(N) = SIN*V(J) + COS*V(N)
         V(J) = TAU
C
C        APPLY THE TRANSFORMATION TO S AND EXTEND THE SPIKE IN W.
C
         L = JJ
         DO 40 I = J, M
            TEMP = COS*S(L) - SIN*W(I)
            W(I) = SIN*S(L) + COS*W(I)
            S(L) = TEMP
            L = L + 1
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     ADD THE SPIKE FROM THE RANK 1 UPDATE TO W.
C
      DO 80 I = 1, M
         W(I) = W(I) + V(N)*U(I)
   80    CONTINUE
C
C     ELIMINATE THE SPIKE.
C
      SING = .FALSE.
      IF (NM1 .LT. 1) GO TO 140
      DO 130 J = 1, NM1
         IF (W(J) .EQ. ZERO) GO TO 120
C
C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
C        J-TH ELEMENT OF THE SPIKE.
C
         IF (ABS(S(JJ)) .GE. ABS(W(J))) GO TO 90
            COTAN = S(JJ)/W(J)
            SIN = P5/SQRT(P25+P25*COTAN**2)
            COS = SIN*COTAN
            TAU = ONE
            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
            GO TO 100
   90    CONTINUE
            TAN = W(J)/S(JJ)
            COS = P5/SQRT(P25+P25*TAN**2)
            SIN = COS*TAN
            TAU = SIN
  100    CONTINUE
C
C        APPLY THE TRANSFORMATION TO S AND REDUCE THE SPIKE IN W.
C
         L = JJ
         DO 110 I = J, M
            TEMP = COS*S(L) + SIN*W(I)
            W(I) = -SIN*S(L) + COS*W(I)
            S(L) = TEMP
            L = L + 1
  110       CONTINUE
C
C        STORE THE INFORMATION NECESSARY TO RECOVER THE
C        GIVENS ROTATION.
C
         W(J) = TAU
  120    CONTINUE
C
C        TEST FOR ZERO DIAGONAL ELEMENTS IN THE OUTPUT S.
C
         IF (S(JJ) .EQ. ZERO) SING = .TRUE.
         JJ = JJ + (M - J + 1)
  130    CONTINUE
  140 CONTINUE
C
C     MOVE W BACK INTO THE LAST COLUMN OF THE OUTPUT S.
C
      L = JJ
      DO 150 I = N, M
         S(L) = W(I)
         L = L + 1
  150    CONTINUE
      IF (S(JJ) .EQ. ZERO) SING = .TRUE.
      RETURN
C
C     LAST CARD OF SUBROUTINE R1UPDT.
C
      END
      SUBROUTINE SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,ML,
     1   MU,EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,R,LR,QTF,WA1,
     2   WA2,WA3,WA4)
C***BEGIN PROLOGUE  SNSQ
C***DATE WRITTEN   800301   (YYMMDD)
C***REVISION DATE  840405   (YYMMDD)
C***CATEGORY NO.  F2A
C***KEYWORDS  NONLINEAR SQUARE SYSTEM,POWELL HYBRID METHOD,ZERO
C***AUTHOR  HIEBERT, K. L., (SNLA)
C***PURPOSE  SNSQ finds to find a zero of a system of N nonlinear
C            functions in N variables by a modification of the Powell
C            hybrid method.  This code is the combination of the MINPACK
C            codes (Argonne) HYBRD and HYBRDJ.
C***DESCRIPTION
C
C 1. Purpose.
C
C       The purpose of SNSQ is to find a zero of a system of N non-
C       linear functions in N variables by a modification of the Powell
C       hybrid method.  The user must provide a subroutine which calcu-
C       lates the functions.  The user has the option of either to
C       provide a subroutine which calculates the Jacobian or to let the
C       code calculate it by a forward-difference approximation.
C       This code is the combination of the MINPACK codes (Argonne)
C       HYBRD and HYBRDJ.
C
C
C 2. Subroutine and Type Statements.
C
C       SUBROUTINE SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,
C      *                 ML,MU,EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,
C      *                 NJEV,R,LR,QTF,WA1,WA2,WA3,WA4)
C       INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,NJEV,LR
C       REAL XTOL,EPSFCN,FACTOR
C       REAL X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),
C      *     WA1(N),WA2(N),WA3(N),WA4(N)
C       EXTERNAL FCN,JAC
C
C
C 3. Parameters.
C
C       Parameters designated as input parameters must be specified on
C       entry to SNSQ and are not changed on exit, while parameters
C       designated as output parameters need not be specified on entry
C       and are set to appropriate values on exit from SNSQ.
C
C       FCN is the name of the user-supplied subroutine which calculates
C         the functions.  FCN must be declared in an EXTERNAL statement
C         in the user calling program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         REAL X(N),FVEC(N)
C         ----------
C         Calculate the functions at X and
C         return this vector in FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless the
C         user wants to terminate execution of SNSQ.  In this case, set
C         IFLAG to a negative integer.
C
C       JAC is the name of the user-supplied subroutine which calculates
C         the Jacobian.  If IOPT=1, then JAC must be declared in an
C         EXTERNAL statement in the user calling program, and should be
C         written as follows.
C
C         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
C         INTEGER N,LDFJAC,IFLAG
C         REAL X(N),FVEC(N),FJAC(LDFJAC,N)
C         ----------
C         Calculate the Jacobian at X and return this
C         matrix in FJAC.  FVEC contains the function
C         values at X and should not be altered.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by JAC unless the
C         user wants to terminate execution of SNSQ.  In this case, set
C         IFLAG to a negative integer.
C
C         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
C
C       IOPT is an input variable which specifies how the Jacobian will
C         be calculated.  If IOPT=1, then the user must supply the
C         Jacobian through the subroutine JAC.  If IOPT=2, then the
C         code will approximate the Jacobian by forward-differencing.
C
C       N is a positive integer input variable set to the number of
C         functions and variables.
C
C       X is an array of length N.  On input, X must contain an initial
C         estimate of the solution vector.  On output, X contains the
C         final estimate of the solution vector.
C
C       FVEC is an output array of length N which contains the functions
C         evaluated at the output X.
C
C       FJAC is an output N by N array which contains the orthogonal
C         matrix Q produced by the QR factorization of the final approx-
C         imate Jacobian.
C
C       LDFJAC is a positive integer input variable not less than N
C         which specifies the leading dimension of the array FJAC.
C
C       XTOL is a non-negative input variable.  Termination occurs when
C         the relative error between two consecutive iterates is at most
C         XTOL.  Therefore, XTOL measures the relative error desired in
C         the approximate solution.  Section 4 contains more details
C         about XTOL.
C
C       MAXFEV is a positive integer input variable.  Termination occurs
C         when the number of calls to FCN is at least MAXFEV by the end
C         of an iteration.
C
C       ML is a non-negative integer input variable which specifies the
C         number of subdiagonals within the band of the Jacobian matrix.
C         If the Jacobian is not banded or IOPT=1, set ML to at
C         least N - 1.
C
C       MU is a non-negative integer input variable which specifies the
C         number of superdiagonals within the band of the Jacobian
C         matrix.  If the Jacobian is not banded or IOPT=1, set MU to at
C         least N - 1.
C
C       EPSFCN is an input variable used in determining a suitable step
C         for the forward-difference approximation.  This approximation
C         assumes that the relative errors in the functions are of the
C         order of EPSFCN.  If EPSFCN is less than the machine preci-
C         sion, it is assumed that the relative errors in the functions
C         are of the order of the machine precision.  If IOPT=1, then
C         EPSFCN can be ignored (treat it as a dummy argument).
C
C       DIAG is an array of length N.  If MODE = 1 (see below), DIAG is
C         internally set.  If MODE = 2, DIAG must contain positive
C         entries that serve as implicit (multiplicative) scale factors
C         for the variables.
C
C       MODE is an integer input variable.  If MODE = 1, the variables
C         will be scaled internally.  If MODE = 2, the scaling is speci-
C         fied by the input DIAG.  Other values of MODE are equivalent
C         to MODE = 1.
C
C       FACTOR is a positive input variable used in determining the ini-
C         tial step bound.  This bound is set to the product of FACTOR
C         and the Euclidean norm of DIAG*X if nonzero, or else to FACTOR
C         itself.  In most cases FACTOR should lie in the interval
C         (.1,100.).  100. is a generally recommended value.
C
C       NPRINT is an integer input variable that enables controlled
C         printing of iterates if it is positive.  In this case, FCN is
C         called with IFLAG = 0 at the beginning of the first iteration
C         and every NPRINT iteration thereafter and immediately prior
C         to return, with X and FVEC available for printing. Appropriate
C         print statements must be added to FCN(see example).  If NPRINT
C         is not positive, no special calls of FCN with IFLAG = 0 are
C         made.
C
C       INFO is an integer output variable.  If the user has terminated
C         execution, INFO is set to the (negative) value of IFLAG.  See
C         description of FCN and JAC. Otherwise, INFO is set as follows.
C
C         INFO = 0  improper input parameters.
C
C         INFO = 1  relative error between two consecutive iterates is
C                   at most XTOL.
C
C         INFO = 2  number of calls to FCN has reached or exceeded
C                   MAXFEV.
C
C         INFO = 3  XTOL is too small.  No further improvement in the
C                   approximate solution X is possible.
C
C         INFO = 4  iteration is not making good progress, as measured
C                   by the improvement from the last five Jacobian eval-
C                   uations.
C
C         INFO = 5  iteration is not making good progress, as measured
C                   by the improvement from the last ten iterations.
C
C         Sections 4 and 5 contain more details about INFO.
C
C       NFEV is an integer output variable set to the number of calls to
C         FCN.
C
C       NJEV is an integer output variable set to the number of calls to
C         JAC. (If IOPT=2, then NJEV is set to zero.)
C
C       R is an output array of length LR which contains the upper
C         triangular matrix produced by the QR factorization of the
C         final approximate Jacobian, stored rowwise.
C
C       LR is a positive integer input variable not less than
C         (N*(N+1))/2.
C
C       QTF is an output array of length N which contains the vector
C         (Q TRANSPOSE)*FVEC.
C
C       WA1, WA2, WA3, and WA4 are work arrays of length N.
C
C
C 4. Successful Completion.
C
C       The accuracy of SNSQ is controlled by the convergence parameter
C       XTOL.  This parameter is used in a test which makes a comparison
C       between the approximation X and a solution XSOL.  SNSQ termi-
C       nates when the test is satisfied.  If the convergence parameter
C       is less than the machine precision (as defined by the function
C       R1MACH(4)), then SNSQ only attempts to satisfy the test
C       defined by the machine precision.  Further progress is not
C       usually possible.
C
C       The test assumes that the functions are reasonably well behaved,
C       and, if the Jacobian is supplied by the user, that the functions
C       and the Jacobian are coded consistently.  If these conditions
C       are not satisfied, then SNSQ may incorrectly indicate conver-
C       gence.  The coding of the Jacobian can be checked by the
C       subroutine CHKDER. If the Jacobian is coded correctly or IOPT=2,
C       then the validity of the answer can be checked, for example, by
C       rerunning SNSQ with a tighter tolerance.
C
C       Convergence Test.  If ENORM(Z) denotes the Euclidean norm of a
C         vector Z and D is the diagonal matrix whose entries are
C         defined by the array DIAG, then this test attempts to guaran-
C         tee that
C
C               ENORM(D*(X-XSOL)) .LE. XTOL*ENORM(D*XSOL).
C
C         If this condition is satisfied with XTOL = 10**(-K), then the
C         larger components of D*X have K significant decimal digits and
C         INFO is set to 1.  There is a danger that the smaller compo-
C         nents of D*X may have large relative errors, but the fast rate
C         of convergence of SNSQ usually avoids this possibility.
C         Unless high precision solutions are required, the recommended
C         value for XTOL is the square root of the machine precision.
C
C
C 5. Unsuccessful Completion.
C
C       Unsuccessful termination of SNSQ can be due to improper input
C       parameters, arithmetic interrupts, an excessive number of func-
C       tion evaluations, or lack of good progress.
C
C       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1,
C         or IOPT .GT. 2, or N .LE. 0, or LDFJAC .LT. N, or
C         XTOL .LT. 0.E0, or MAXFEV .LE. 0, or ML .LT. 0, or MU .LT. 0,
C         or FACTOR .LE. 0.E0, or LR .LT. (N*(N+1))/2.
C
C       Arithmetic Interrupts.  If these interrupts occur in the FCN
C         subroutine during an early stage of the computation, they may
C         be caused by an unacceptable choice of X by SNSQ.  In this
C         case, it may be possible to remedy the situation by rerunning
C         SNSQ with a smaller value of FACTOR.
C
C       Excessive Number of Function Evaluations.  A reasonable value
C         for MAXFEV is 100*(N+1) for IOPT=1 and 200*(N+1) for IOPT=2.
C         If the number of calls to FCN reaches MAXFEV, then this
C         indicates that the routine is converging very slowly as
C         measured by the progress of FVEC, and INFO is set to 2.  This
C         situation should be unusual because, as indicated below, lack
C         of good progress is usually diagnosed earlier by SNSQ,
C         causing termination with INFO = 4 or INFO = 5.
C
C       Lack of Good Progress.  SNSQ searches for a zero of the system
C         by minimizing the sum of the squares of the functions.  In so
C         doing, it can become trapped in a region where the minimum
C         does not correspond to a zero of the system and, in this situ-
C         ation, the iteration eventually fails to make good progress.
C         In particular, this will happen if the system does not have a
C         zero.  If the system has a zero, rerunning SNSQ from a dif-
C         ferent starting point may be helpful.
C
C
C 6. Characteristics of the Algorithm.
C
C       SNSQ is a modification of the Powell hybrid method.  Two of its
C       main characteristics involve the choice of the correction as a
C       convex combination of the Newton and scaled gradient directions,
C       and the updating of the Jacobian by the rank-1 method of Broy-
C       den.  The choice of the correction guarantees (under reasonable
C       conditions) global convergence for starting points far from the
C       solution and a fast rate of convergence.  The Jacobian is
C       calculated at the starting point by either the user-supplied
C       subroutine or a forward-difference approximation, but it is not
C       recalculated until the rank-1 method fails to produce satis-
C       factory progress.
C
C       Timing.  The time required by SNSQ to solve a given problem
C         depends on N, the behavior of the functions, the accuracy
C         requested, and the starting point.  The number of arithmetic
C         operations needed by SNSQ is about 11.5*(N**2) to process
C         each evaluation of the functions (call to FCN) and 1.3*(N**3)
C         to process each evaluation of the Jacobian (call to JAC,
C         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
C         the timing of SNSQ will be strongly influenced by the time
C         spent in FCN and JAC.
C
C       Storage.  SNSQ requires (3*N**2 + 17*N)/2 single precision
C         storage locations, in addition to the storage required by the
C         program.  There are no internally declared storage arrays.
C
C
C 7. Example.
C
C       The problem is to determine the values of X(1), X(2), ..., X(9),
C       which solve the system of tridiagonal equations
C
C       (3-2*X(1))*X(1)           -2*X(2)                   = -1
C               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
C                                   -X(8) + (3-2*X(9))*X(9) = -1
C C     **********
C
C       PROGRAM TEST(INPUT,OUTPUT,TAPE6=OUTPUT)
C C
C C     Driver for SNSQ example.
C C
C       INTEGER J,IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,
C      *        NWRITE
C       REAL XTOL,EPSFCN,FACTOR,FNORM
C       REAL X(9),FVEC(9),DIAG(9),FJAC(9,9),R(45),QTF(9),
C      *     WA1(9),WA2(9),WA3(9),WA4(9)
C       REAL ENORM,R1MACH
C       EXTERNAL FCN
C       DATA NWRITE /6/
C C
C       IOPT = 2
C       N = 9
C C
C C     The following starting values provide a rough solution.
C C
C       DO 10 J = 1, 9
C          X(J) = -1.E0
C    10    CONTINUE
C C
C       LDFJAC = 9
C       LR = 45
C C
C C     Set XTOL to the square root of the machine precision.
C C     Unless high precision solutions are required,
C C     this is the recommended setting.
C C
C       XTOL = SQRT(R1MACH(4))
C C
C       MAXFEV = 2000
C       ML = 1
C       MU = 1
C       EPSFCN = 0.E0
C       MODE = 2
C       DO 20 J = 1, 9
C          DIAG(J) = 1.E0
C    20    CONTINUE
C       FACTOR = 1.E2
C       NPRINT = 0
C C
C       CALL SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,ML,MU,
C      *           EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
C      *           R,LR,QTF,WA1,WA2,WA3,WA4)
C       FNORM = ENORM(N,FVEC)
C       WRITE (NWRITE,1000) FNORM,NFEV,INFO,(X(J),J=1,N)
C       STOP
C  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
C      *        5X,' NUMBER OF FUNCTION EVALUATIONS',I10 //
C      *        5X,' EXIT PARAMETER',16X,I10 //
C      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
C       END
C       SUBROUTINE FCN(N,X,FVEC,IFLAG)
C       INTEGER N,IFLAG
C       REAL X(N),FVEC(N)
C       INTEGER K
C       REAL ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
C       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
C C
C       IF (IFLAG .NE. 0) GO TO 5
C C
C C     Insert print statements here when NPRINT is positive.
C C
C       RETURN
C     5 CONTINUE
C       DO 10 K = 1, N
C          TEMP = (THREE - TWO*X(K))*X(K)
C          TEMP1 = ZERO
C          IF (K .NE. 1) TEMP1 = X(K-1)
C          TEMP2 = ZERO
C          IF (K .NE. N) TEMP2 = X(K+1)
C          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
C    10    CONTINUE
C       RETURN
C       END
C
C       Results obtained with different compilers or machines
C       may be slightly different.
C
C       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
C
C       NUMBER OF FUNCTION EVALUATIONS        14
C
C       EXIT PARAMETER                         1
C
C       FINAL APPROXIMATE SOLUTION
C
C       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
C       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
C       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
C***REFERENCES  POWELL, M. J. D.
C                 A HYBRID METHOD FOR NONLINEAR EQUATIONS.
C                 NUMERICAL METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS,
C                 P. RABINOWITZ, EDITOR.  GORDON AND BREACH, 1970.
C***ROUTINES CALLED  DOGLEG,ENORM,FDJAC1,QFORM,QRFAC,R1MACH,R1MPYQ,
C                    R1UPDT,XERROR
C***END PROLOGUE  SNSQ
      include "precis.fi"
      INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,NJEV
      DIMENSION X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),WA1(N),
     1     WA2(N),WA3(N),WA4(N)
      EXTERNAL FCN
      INTEGER I,IFLAG,ITER,J,JM1,L,NCFAIL,NCSUC,NSLOW1,NSLOW2
      INTEGER IWA(1)
      LOGICAL JEVAL,SING
      DATA ONE,P1,P5,P001,P0001,ZERO
     .              /1.0D0,1.0D-1,5.0D-1,1.0D-3,1.0D-4,0.0D0/
C
C***FIRST EXECUTABLE STATEMENT  SNSQ
      EPSMCH = D1MACH(4)
C
      INFO = 0
      IFLAG = 0
      NFEV = 0
      NJEV = 0
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR.
     1    N .LE. 0 .OR. XTOL .LT. ZERO .OR. MAXFEV .LE. 0
     2    .OR. ML .LT. 0 .OR. MU .LT. 0 .OR. FACTOR .LE. ZERO
     3    .OR. LDFJAC .LT. N .OR. LR .LT. (N*(N + 1))/2) GO TO 300
      IF (MODE .NE. 2) GO TO 20
      DO 10 J = 1, N
         IF (DIAG(J) .LE. ZERO) GO TO 300
   10    CONTINUE
   20 CONTINUE
C
C     EVALUATE THE FUNCTION AT THE STARTING POINT
C     AND CALCULATE ITS NORM.
C
      IFLAG = 1
      CALL FCN(N,X,FVEC,IFLAG)
      NFEV = 1
      IF (IFLAG .LT. 0) GO TO 300
      FNORM = ENORM(N,FVEC)
C
C     INITIALIZE ITERATION COUNTER AND MONITORS.
C
      ITER = 1
      NCSUC = 0
      NCFAIL = 0
      NSLOW1 = 0
      NSLOW2 = 0
C
C     BEGINNING OF THE OUTER LOOP.
C
   30 CONTINUE
         JEVAL = .TRUE.
C
C        CALCULATE THE JACOBIAN MATRIX.
C
         IF (IOPT .EQ. 2) GO TO 31
C
C        USER SUPPLIES JACOBIAN
C
            CALL JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
            NJEV = NJEV+1
            GO TO 32
C
C        CODE APPROXIMATES THE JACOBIAN
C
   31       IFLAG = 2
            CALL FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,WA1,
     1               WA2)
            NFEV = NFEV + MIN0(ML+MU+1,N)
C
   32    IF (IFLAG .LT. 0) GO TO 300
C
C        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.
C
         CALL QRFAC(N,N,FJAC,LDFJAC,.FALSE.,IWA,1,WA1,WA2,WA3)
C
C        ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING
C        TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.
C
         IF (ITER .NE. 1) GO TO 70
         IF (MODE .EQ. 2) GO TO 50
         DO 40 J = 1, N
            DIAG(J) = WA2(J)
            IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE
   40       CONTINUE
   50    CONTINUE
C
C        ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED X
C        AND INITIALIZE THE STEP BOUND DELTA.
C
         DO 60 J = 1, N
            WA3(J) = DIAG(J)*X(J)
   60       CONTINUE
         XNORM = ENORM(N,WA3)
         DELTA = FACTOR*XNORM
         IF (DELTA .EQ. ZERO) DELTA = FACTOR
   70    CONTINUE
C
C        FORM (Q TRANSPOSE)*FVEC AND STORE IN QTF.
C
         DO 80 I = 1, N
            QTF(I) = FVEC(I)
   80       CONTINUE
         DO 120 J = 1, N
            IF (FJAC(J,J) .EQ. ZERO) GO TO 110
            SUM = ZERO
            DO 90 I = J, N
               SUM = SUM + FJAC(I,J)*QTF(I)
   90          CONTINUE
            TEMP = -SUM/FJAC(J,J)
            DO 100 I = J, N
               QTF(I) = QTF(I) + FJAC(I,J)*TEMP
  100          CONTINUE
  110       CONTINUE
  120       CONTINUE
C
C        COPY THE TRIANGULAR FACTOR OF THE QR FACTORIZATION INTO R.
C
         SING = .FALSE.
         DO 150 J = 1, N
            L = J
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 140
            DO 130 I = 1, JM1
               R(L) = FJAC(I,J)
               L = L + N - I
  130          CONTINUE
  140       CONTINUE
            R(L) = WA1(J)
            IF (WA1(J) .EQ. ZERO) SING = .TRUE.
  150       CONTINUE
C
C        ACCUMULATE THE ORTHOGONAL FACTOR IN FJAC.
C
         CALL QFORM(N,N,FJAC,LDFJAC,WA1)
C
C        RESCALE IF NECESSARY.
C
         IF (MODE .EQ. 2) GO TO 170
         DO 160 J = 1, N
            DIAG(J) = MAX(DIAG(J),WA2(J))
  160       CONTINUE
  170    CONTINUE
C
C        BEGINNING OF THE INNER LOOP.
C
  180    CONTINUE
C
C           IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.
C
            IF (NPRINT .LE. 0) GO TO 190
            IFLAG = 0
            IF (MOD(ITER-1,NPRINT) .EQ. 0) CALL FCN(N,X,FVEC,IFLAG)
            IF (IFLAG .LT. 0) GO TO 300
  190       CONTINUE
C
C           DETERMINE THE DIRECTION P.
C
            CALL DOGLEG(N,R,LR,DIAG,QTF,DELTA,WA1,WA2,WA3)
C
C           STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.
C
            DO 200 J = 1, N
               WA1(J) = -WA1(J)
               WA2(J) = X(J) + WA1(J)
               WA3(J) = DIAG(J)*WA1(J)
  200          CONTINUE
            PNORM = ENORM(N,WA3)
C
C           ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.
C
            IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)
C
C           EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.
C
            IFLAG = 1
            CALL FCN(N,WA2,WA4,IFLAG)
            NFEV = NFEV + 1
            IF (IFLAG .LT. 0) GO TO 300
            FNORM1 = ENORM(N,WA4)
C
C           COMPUTE THE SCALED ACTUAL REDUCTION.
C
            ACTRED = -ONE
            IF (FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
C
C           COMPUTE THE SCALED PREDICTED REDUCTION.
C
            L = 1
            DO 220 I = 1, N
               SUM = ZERO
               DO 210 J = I, N
                  SUM = SUM + R(L)*WA1(J)
                  L = L + 1
  210             CONTINUE
               WA3(I) = QTF(I) + SUM
  220          CONTINUE
            TEMP = ENORM(N,WA3)
            PRERED = ZERO
            IF (TEMP .LT. FNORM) PRERED = ONE - (TEMP/FNORM)**2
C
C           COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED
C           REDUCTION.
C
            RATIO = ZERO
            IF (PRERED .GT. ZERO) RATIO = ACTRED/PRERED
C
C           UPDATE THE STEP BOUND.
C
            IF (RATIO .GE. P1) GO TO 230
               NCSUC = 0
               NCFAIL = NCFAIL + 1
               DELTA = P5*DELTA
               GO TO 240
  230       CONTINUE
               NCFAIL = 0
               NCSUC = NCSUC + 1
               IF (RATIO .GE. P5 .OR. NCSUC .GT. 1)
     1            DELTA = MAX(DELTA,PNORM/P5)
               IF (ABS(RATIO-ONE) .LE. P1) DELTA = PNORM/P5
  240       CONTINUE
C
C           TEST FOR SUCCESSFUL ITERATION.
C
            IF (RATIO .LT. P0001) GO TO 260
C
C           SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.
C
            DO 250 J = 1, N
               X(J) = WA2(J)
               WA2(J) = DIAG(J)*X(J)
               FVEC(J) = WA4(J)
  250          CONTINUE
            XNORM = ENORM(N,WA2)
            FNORM = FNORM1
            ITER = ITER + 1
  260       CONTINUE
C
C           DETERMINE THE PROGRESS OF THE ITERATION.
C
            NSLOW1 = NSLOW1 + 1
            IF (ACTRED .GE. P001) NSLOW1 = 0
            IF (JEVAL) NSLOW2 = NSLOW2 + 1
            IF (ACTRED .GE. P1) NSLOW2 = 0
C
C           TEST FOR CONVERGENCE.
C
            IF (DELTA .LE. XTOL*XNORM+XTOL .OR. FNORM .EQ. ZERO)INFO = 1
            IF (INFO .NE. 0) GO TO 300
C
C           TESTS FOR TERMINATION AND STRINGENT TOLERANCES.
C
            IF (NFEV .GE. MAXFEV) INFO = 2
            IF (P1*MAX(P1*DELTA,PNORM) .LE. EPSMCH*XNORM) INFO = 3
            IF (NSLOW2 .EQ. 5) INFO = 4
            IF (NSLOW1 .EQ. 10) INFO = 5
            IF (INFO .NE. 0) GO TO 300
C
C           CRITERION FOR RECALCULATING JACOBIAN
C
            IF (NCFAIL .EQ. 2) GO TO 290
C
C           CALCULATE THE RANK ONE MODIFICATION TO THE JACOBIAN
C           AND UPDATE QTF IF NECESSARY.
C
            DO 280 J = 1, N
               SUM = ZERO
               DO 270 I = 1, N
                  SUM = SUM + FJAC(I,J)*WA4(I)
  270             CONTINUE
               WA2(J) = (SUM - WA3(J))/PNORM
               WA1(J) = DIAG(J)*((DIAG(J)*WA1(J))/PNORM)
               IF (RATIO .GE. P0001) QTF(J) = SUM
  280          CONTINUE
C
C           COMPUTE THE QR FACTORIZATION OF THE UPDATED JACOBIAN.
C
            CALL R1UPDT(N,N,R,LR,WA1,WA2,WA3,SING)
            CALL R1MPYQ(N,N,FJAC,LDFJAC,WA2,WA3)
            CALL R1MPYQ(1,N,QTF,1,WA2,WA3)
C
C           END OF THE INNER LOOP.
C
            JEVAL = .FALSE.
            GO TO 180
  290    CONTINUE
C
C        END OF THE OUTER LOOP.
C
         GO TO 30
  300 CONTINUE
C
C     TERMINATION, EITHER NORMAL OR USER IMPOSED.
C
      IF (IFLAG .LT. 0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT .GT. 0) CALL FCN(N,X,FVEC,IFLAG)
      IF (INFO .LT. 0) CALL XERROR( 'SNSQ   -- EXECUTION TERMINATED BECA
     1USE USER SET IFLAG NEGATIVE.',63,1,1)
      IF (INFO .EQ. 0) CALL XERROR( 'SNSQ   -- INVALID INPUT PARAMETER.'
     1 ,34,2,1)
      IF (INFO .EQ. 2) CALL XERROR( 'SNSQ   -- TOO MANY FUNCTION EVALUAT
     1IONS.',40,9,1)
      IF (INFO .EQ. 3) CALL XERROR( 'SNSQ   -- XTOL TOO SMALL. NO FURTHE
     1R IMPROVEMENT POSSIBLE.',58,3,1)
      IF (INFO .GT. 4) CALL XERROR( 'SNSQ   -- ITERATION NOT MAKING GOOD
     1 PROGRESS.',45,1,1)
      RETURN
C
C     LAST CARD OF SUBROUTINE SNSQ.
C
      END

      INTEGER FUNCTION IDAMAX(N,DX,INCX)

C***BEGIN PROLOGUE  IDAMAX
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A2
C***KEYWORDS  BLAS,DOUBLE PRECISION,LINEAR ALGEBRA,MAXIMUM COMPONENT, 
C             VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  FIND LARGEST COMPONENT OF D.P. VECTOR
C***DESCRIPTION
C
C                B L A S  SUBPROGRAM
C    DESCRIPTION OF PARAMETERS
C
C     --INPUT--
C        N  NUMBER OF ELEMENTS IN INPUT VECTOR(S) 
C       DX  DOUBLE PRECISION VECTOR WITH N ELEMENTS
C     INCX  STORAGE SPACING BETWEEN ELEMENTS OF DX
C
C     --OUTPUT--
C   IDAMAX  SMALLEST INDEX (ZERO IF N .LE. 0)
C
C     FIND SMALLEST INDEX OF MAXIMUM MAGNITUDE OF DOUBLE PRECISION DX.
C     IDAMAX =  FIRST I, I = 1 TO N, TO MINIMIZE  ABS(DX(1-INCX+I*INCX)
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  IDAMAX
C
      DOUBLE PRECISION DX(*),DMAX,XMAG
C***FIRST EXECUTABLE STATEMENT  IDAMAX
      IDAMAX = 0
      IF(N.LE.0) RETURN
      IDAMAX = 1
      IF(N.LE.1)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      DMAX = DABS(DX(1))
      NS = N*INCX
      II = 1
          DO 10 I = 1,NS,INCX 
          XMAG = DABS(DX(I))
          IF(XMAG.LE.DMAX) GO TO 5
          IDAMAX = II
          DMAX = XMAG
    5     II = II + 1
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
   20 DMAX = DABS(DX(1))
      DO 30 I = 2,N 
          XMAG = DABS(DX(I))
          IF(XMAG.LE.DMAX) GO TO 30
          IDAMAX = I
          DMAX = XMAG
   30 CONTINUE
      RETURN
      END 

      DOUBLE PRECISION FUNCTION DASUM(N,DX,INCX)
C***BEGIN PROLOGUE  DASUM
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A3A
C***KEYWORDS  ADD,BLAS,DOUBLE PRECISION,LINEAR ALGEBRA,MAGNITUDE,SUM,
C             VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  Sum of magnitudes of d.p. vector components
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       DX  double precision vector with N elements
C     INCX  storage spacing between elements of DX
C
C     --Output--
C    DASUM  double precision result (zero if N .LE. 0)
C
C     Returns sum of magnitudes of double precision DX.
C     DASUM = sum from 0 to N-1 of DABS(DX(1+I*INCX))
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DASUM
C
      DOUBLE PRECISION DX(*)
C***FIRST EXECUTABLE STATEMENT  DASUM
      DASUM = 0.D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      NS = N*INCX
          DO 10 I=1,NS,INCX
          DASUM = DASUM + DABS(DX(I))
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.
C
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         DASUM = DASUM + DABS(DX(I))
   30 CONTINUE
      IF( N .LT. 6 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
         DASUM = DASUM + DABS(DX(I)) + DABS(DX(I+1)) + DABS(DX(I+2))
     1   + DABS(DX(I+3)) + DABS(DX(I+4)) + DABS(DX(I+5))
   50 CONTINUE
      RETURN
      END

      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
C***BEGIN PROLOGUE  DAXPY
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A7
C***KEYWORDS  BLAS,DOUBLE PRECISION,LINEAR ALGEBRA,TRIAD,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  D.P COMPUTATION Y = A*X + Y
C***DESCRIPTION
C
C                B L A S  SUBPROGRAM
C    DESCRIPTION OF PARAMETERS
C
C     --INPUT--
C        N  NUMBER OF ELEMENTS IN INPUT VECTOR(S) 
C       DA  DOUBLE PRECISION SCALAR MULTIPLIER
C       DX  DOUBLE PRECISION VECTOR WITH N ELEMENTS
C     INCX  STORAGE SPACING BETWEEN ELEMENTS OF DX
C       DY  DOUBLE PRECISION VECTOR WITH N ELEMENTS
C     INCY  STORAGE SPACING BETWEEN ELEMENTS OF DY
C
C     --OUTPUT--
C       DY  DOUBLE PRECISION RESULT (UNCHANGED IF N .LE. 0) 
C
C     OVERWRITE DOUBLE PRECISION DY WITH DOUBLE PRECISION DA*DX + DY. 
C     FOR I = 0 TO N-1, REPLACE  DY(LY+I*INCY) WITH DA*DX(LX+I*INCX) +
C       DY(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N
C       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DAXPY
C
      DOUBLE PRECISION DX(*),DY(*),DA
C***FIRST EXECUTABLE STATEMENT  DAXPY
      IF(N.LE.0.OR.DA.EQ.0.D0) RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
C
C        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1 
      DO 10 I = 1,N 
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4. 
C
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40 
      DO 30 I = 1,M 
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
C
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
C
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          DY(I) = DA*DX(I) + DY(I)
   70     CONTINUE
      RETURN
      END 

      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)

C***BEGIN PROLOGUE  DDOT
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A4
C***KEYWORDS  BLAS,DOUBLE PRECISION,INNER PRODUCT,LINEAR ALGEBRA,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  D.P. INNER PRODUCT OF D.P. VECTORS
C***DESCRIPTION
C
C                B L A S  SUBPROGRAM
C    DESCRIPTION OF PARAMETERS
C
C     --INPUT--
C        N  NUMBER OF ELEMENTS IN INPUT VECTOR(S) 
C       DX  DOUBLE PRECISION VECTOR WITH N ELEMENTS
C     INCX  STORAGE SPACING BETWEEN ELEMENTS OF DX
C       DY  DOUBLE PRECISION VECTOR WITH N ELEMENTS
C     INCY  STORAGE SPACING BETWEEN ELEMENTS OF DY
C
C     --OUTPUT--
C     DDOT  DOUBLE PRECISION DOT PRODUCT (ZERO IF N .LE. 0) 
C
C     RETURNS THE DOT PRODUCT OF DOUBLE PRECISION DX AND DY.
C     DDOT = SUM FOR I = 0 TO N-1 OF  DX(LX+I*INCX) * DY(LY+I*INCY)
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DDOT
C
      DOUBLE PRECISION DX(*),DY(*)
C***FIRST EXECUTABLE STATEMENT  DDOT
      DDOT = 0.D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
C
C         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1 
      DO 10 I = 1,N 
         DDOT = DDOT + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5. 
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40 
      DO 30 I = 1,M 
         DDOT = DDOT + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
         DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) +
     1   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
      RETURN
C
C         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
C
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          DDOT = DDOT + DX(I)*DY(I)
   70     CONTINUE
      RETURN
      END 

      DOUBLE PRECISION FUNCTION DNRM2(N,DX,INCX)
C***BEGIN PROLOGUE  DNRM2
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A3B
C***KEYWORDS  BLAS,DOUBLE PRECISION,EUCLIDEAN,L2,LENGTH,LINEAR ALGEBRA,
C             NORM,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  EUCLIDEAN LENGTH (L2 NORM) OF D.P. VECTOR
C***DESCRIPTION
C
C                B L A S  SUBPROGRAM
C    DESCRIPTION OF PARAMETERS
C
C     --INPUT--
C        N  NUMBER OF ELEMENTS IN INPUT VECTOR(S) 
C       DX  DOUBLE PRECISION VECTOR WITH N ELEMENTS
C     INCX  STORAGE SPACING BETWEEN ELEMENTS OF DX
C
C     --OUTPUT--
C    DNRM2  DOUBLE PRECISION RESULT (ZERO IF N .LE. 0)
C
C     EUCLIDEAN NORM OF THE N-VECTOR STORED IN DX() WITH STORAGE
C     INCREMENT INCX .
C     IF    N .LE. 0 RETURN WITH RESULT = 0.
C     IF N .GE. 1 THEN INCX MUST BE .GE. 1
C
C           C.L. LAWSON, 1978 JAN 08
C
C     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
C     HOPEFULLY APPLICABLE TO ALL MACHINES.
C         CUTLO = MAXIMUM OF  DSQRT(U/EPS)  OVER ALL KNOWN MACHINES.
C         CUTHI = MINIMUM OF  DSQRT(V)      OVER ALL KNOWN MACHINES.
C     WHERE
C         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.
C         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
C         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
C
C     BRIEF OUTLINE OF ALGORITHM..
C
C     PHASE 1    SCANS ZERO COMPONENTS. 
C     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO
C     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO
C     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M
C     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.
C
C     VALUES FOR CUTLO AND CUTHI..
C     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER
C     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..
C     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE
C                   UNIVAC AND DEC AT 2**(-103)
C                   THUS CUTLO = 2**(-51) = 4.44089E-16
C     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.
C                   THUS CUTHI = 2**(63.5) = 1.30438E19
C     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC. 
C                   THUS CUTLO = 2**(-33.5) = 8.23181D-11
C     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19
C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DNRM2
      INTEGER          NEXT
      DOUBLE PRECISION   DX(*), CUTLO, CUTHI, HITEST, SUM, XMAX,ZERO,ONE
      DATA   ZERO, ONE /0.0D0, 1.0D0/
C
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C***FIRST EXECUTABLE STATEMENT  DNRM2
      IF(N .GT. 0) GO TO 10
         DNRM2  = ZERO
         GO TO 300
C
   10 ASSIGN 30 TO NEXT
      SUM = ZERO
      NN = N * INCX 
C                                                 BEGIN MAIN LOOP
      I = 1
   20    GO TO NEXT,(30, 50, 70, 110)
   30 IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
      ASSIGN 50 TO NEXT
      XMAX = ZERO
C
C                        PHASE 1.  SUM IS ZERO
C
   50 IF( DX(I) .EQ. ZERO) GO TO 200
      IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
C
C                                PREPARE FOR PHASE 2.
      ASSIGN 70 TO NEXT
      GO TO 105
C
C                                PREPARE FOR PHASE 4.
C
  100 I = J
      ASSIGN 110 TO NEXT
      SUM = (SUM / DX(I)) / DX(I)
  105 XMAX = DABS(DX(I))
      GO TO 115
C
C                   PHASE 2.  SUM IS SMALL.
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF( DABS(DX(I)) .GT. CUTLO ) GO TO 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF( DABS(DX(I)) .LE. XMAX ) GO TO 115
         SUM = ONE + SUM * (XMAX / DX(I))**2
         XMAX = DABS(DX(I))
         GO TO 200
C
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
C
C
C                  PREPARE FOR PHASE 3. 
C
   75 SUM = (SUM * XMAX) * XMAX
C
C
C     FOR REAL OR D.P. SET HITEST = CUTHI/N
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
C
   85 HITEST = CUTHI/DBLE( N )
C
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J =I,NN,INCX
      IF(DABS(DX(J)) .GE. HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2 
      DNRM2 = DSQRT( SUM )
      GO TO 300
C
  200 CONTINUE
      I = I + INCX
      IF ( I .LE. NN ) GO TO 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      DNRM2 = XMAX * DSQRT(SUM)
  300 CONTINUE
      RETURN
      END 

      SUBROUTINE DSCAL(N,DA,DX,INCX)
C***BEGIN PROLOGUE  DSCAL
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A6
C***KEYWORDS  BLAS,LINEAR ALGEBRA,SCALE,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  D.P. VECTOR SCALE X = A*X
C***DESCRIPTION
C
C                B L A S  SUBPROGRAM
C    DESCRIPTION OF PARAMETERS
C
C     --INPUT--
C        N  NUMBER OF ELEMENTS IN INPUT VECTOR(S) 
C       DA  DOUBLE PRECISION SCALE FACTOR
C       DX  DOUBLE PRECISION VECTOR WITH N ELEMENTS
C     INCX  STORAGE SPACING BETWEEN ELEMENTS OF DX
C
C     --OUTPUT--
C       DX  DOUBLE PRECISION RESULT (UNCHANGED IF N.LE.0)
C
C     REPLACE DOUBLE PRECISION DX BY DOUBLE PRECISION DA*DX.
C     FOR I = 0 TO N-1, REPLACE DX(1+I*INCX) WITH  DA * DX(1+I*INCX)
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DSCAL
C
      DOUBLE PRECISION DA,DX(*)
C***FIRST EXECUTABLE STATEMENT  DSCAL
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      NS = N*INCX
          DO 10 I = 1,NS,INCX 
          DX(I) = DA*DX(I)
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5. 
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40 
      DO 30 I = 1,M 
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END 
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y := alpha*A'*x + y.
*
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEMV .
*
      END
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And if  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + beta*C
*
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B' + beta*C
*
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B' + beta*C
*
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEMM .
*
      END

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

      SUBROUTINE GJAC

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     GJAC
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description:  DUMMY ROUTINE TO KEEP THE LINKER HAPPY
C
C     Arguments: 
C
C     Revision History:
C        Created:  12/2/1992 at 11:28 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

C     DUMMY ROUTINE TO KEEP THE LINKER HAPPY

      STOP 'internal error in dassl - gjac not instantiated'
      END

      SUBROUTINE JAC

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     JAC
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description:  DUMMY ROUTINE TO KEEP THE LINKER HAPPY
C
C     Arguments: 
C
C     Revision History:
C        Created:  12/2/1992 at 11:28 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------


C     DUMMY ROUTINE TO KEEP THE LINKER HAPPY

      STOP 'internal error in dassl - jac not instantiated'
      END

      FUNCTION JACD()

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     JACD
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description:  Used by DASSL to determine how many columns of the
C                   Jacobian are going to be computed the "hard way".  
C                   This method avoids adding CFAST common blocks to DASSL.
C
C     Arguments: 
C
C     Revision History:
C        Created:  2/4/1993 at 17:13 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cparams.fi"
      include "wdervs.fi"
      JACD = JACDIM
      RETURN
      END

      SUBROUTINE SETDERV(J)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SETDERV
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description: This routine sets the value of JACCOL for use by DASSL. 
C                                                                   
C     Arguments: J       Value to be copied into JACCOL
C
C     Revision History:
C        Created:  2/14/1993 by GPF
C        Modified: 2/2/1995 by gpf  Removed IDERV set option
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "wdervs.fi"
      include "cenviro.fi"
      include "opt.fi"
C
      IF(J.GT.-10)JACCOL = J
      RETURN
      END
      SUBROUTINE INCJAC
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INJAC
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description:  Used to increment the Jacobian counter from within
C                   DASSL.  This method avoids adding CFAST common blocks 
C                   to DASSL.
C
C
C     Revision History:
C        Created:  2/14/1993 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"
C
      NUMJAC = NUMJAC + 1
      RETURN
      END

      integer function rev_numerics
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_numerics = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_numerics