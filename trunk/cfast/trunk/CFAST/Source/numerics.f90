
! --------------------------- ddassl -------------------------------------------

    subroutine ddassl(res,neq,t,y,yprime,tout,info,rtol,atol,idid,rwork,lrw,iwork,liw,rpar,ipar,jac)
    !
    !***begin prologue  ddassl
    !***date written   830315   (yymmdd)
    !***revision date  891228   (yymmdd)
    !***category no.  d2a2
    !***keywords  differential/algebraic,backward differentiation formulas
    !             implicit differential systems
    !***author  petzold,linda r.,computing and mathematics research division
    !             lawrence livermore national laboratory
    !             l - 316, p.o. box 808,
    !             livermore, ca.    94550
    !***purpose  this code solves a system of differential/algebraic
    !            equations of the form g(t,y,yprime) = 0.
    !***description
    !
    ! *usage:
    !
    !      implicit real(8)(a-h,o-z)
    !      external res, jac
    !      integer neq, info(n), idid, lrw, liw, iwork(liw), ipar
    !      real(8) t, y(neq), yprime(neq), tout, rtol, atol,
    !     *   rwork(lrw), rpar
    !
    !      call ddassl (res, neq, t, y, yprime, tout, info, rtol, atol,
    !     *   idid, rwork, lrw, iwork, liw, rpar, ipar, jac)
    !
    !
    !
    ! *arguments:
    !
    !  res:ext  this is a subroutine which you provide to define the
    !           differential/algebraic system.
    !
    !  neq:in  this is the number of equations to be solved.
    !
    !  t:inout  this is the current value of the independent variable.
    !
    !  y(*):inout  this array contains the solution components at t.
    !
    !  yprime(*):inout  this array contains the derivatives of the solution
    !                   components at t.
    !
    !  tout:in  this is a point at which a solution is desired.
    !
    !  info(n):in  the basic task of the code is to solve the system from t
    !              to tout and return an answer at tout.  info is an integer
    !              array which is used to communicate exactly how you want
    !              this task to be carried out.  n must be greater than or
    !              equal to 15.
    !
    !  rtol,atol:inout  these quantities represent absolute and relative
    !                   error tolerances which you provide to indicate how
    !                   accurately you wish the solution to be computed.
    !                   you may choose them to be both scalars or else
    !                   both vectors.
    !
    !  idid:out  this scalar quantity is an indicator reporting what the
    !            code did.  you must monitor this integer variable to decide
    !            what action to take next.
    !
    !  rwork:work  a real work array of length lrw which provides the
    !               code with needed storage space.
    !
    !  lrw:in  the length of rwork.
    !
    !  iwork:work  an integer work array of length liw which probides the
    !               code with needed storage space.
    !
    !  liw:in  the length of iwork.
    !
    !  rpar,ipar:in  these are real and integer parameter arrays which
    !                you can use for communication between your calling
    !                program and the res subroutine (and the jac subroutine)
    !
    !  jac:ext  this is the name of a subroutine which you may choose to
    !           provide for defining a matrix of partial derivatives
    !           described below.
    !
    !
    ! *description
    !  quantities which may be altered by the code are
    !     t,y(*),yprime(*),info(1),rtol,atol,
    !     idid,rwork(*) and iwork(*)
    !
    !  subroutine ddassl uses the backward differentiation formulas of
    !  orders one through five to solve a system of the above form for y and
    !  yprime.  values for y and yprime at the initial time must be given as
    !  input.  these values must be consistent, (that is, if t,y,yprime are
    !  the given initial values, they must satisfy g(t,y,yprime) = 0.).  the
    !  subroutine solves the system from t to tout.  it is easy to continue
    !  the solution to get results at additional tout.  this is the interval
    !  mode of operation.  intermediate results can also be obtained easily
    !  by using the intermediate-output capability.
    !
    !  ----------input-what to do on the first call to ddassl---------------
    !
    !
    !  the first call of the code is defined to be the start of each new
    !  problem. read through the descriptions of all the following items,
    !  provide sufficient storage space for designated arrays, set
    !  appropriate variables for the initialization of the problem, and
    !  give information about how you want the problem to be solved.
    !
    !
    !  res -- provide a subroutine of the form
    !             subroutine res(t,y,yprime,delta,ires,rpar,ipar)
    !         to define the system of differential/algebraic
    !         equations which is to be solved. for the given values
    !         of t,y and yprime, the subroutine should
    !         return the residual of the defferential/algebraic
    !         system
    !             delta = g(t,y,yprime)
    !         (delta(*) is a vector of length neq which is
    !         output for res.)
    !
    !         subroutine res must not alter t,y or yprime.
    !         you must declare the name res in an external
    !         statement in your program that calls ddassl.
    !         you must dimension y,yprime and delta in res.
    !
    !         ires is an integer flag which is always equal to
    !         zero on input. subroutine res should alter ires
    !         only if it encounters an illegal value of y or
    !         a stop condition. set ires = -1 if an input value
    !         is illegal, and ddassl will try to solve the problem
    !         without getting ires = -1. if ires = -2, ddassl
    !         will return control to the calling program
    !         with idid = -11.
    !
    !         rpar and ipar are real and integer parameter arrays which
    !         you can use for communication between your calling program
    !         and subroutine res. they are not altered by ddassl. if you
    !         do not need rpar or ipar, ignore these parameters by treat-
    !         ing them as dummy arguments. if you do choose to use them,
    !         dimension them in your calling program and in res as arrays
    !         of appropriate length.
    !
    !  neq -- set it to the number of differential equations.
    !         (neq >= 1)
    !
    !  t -- set it to the initial point of the integration.
    !       t must be defined as a variable.
    !
    !  y(*) -- set this vector to the initial values of the neq solution
    !          components at the initial point. you must dimension y of
    !          length at least neq in your calling program.
    !
    !  yprime(*) -- set this vector to the initial values of
    !               the neq first derivatives of the solution
    !               components at the initial point. you
    !               must dimension yprime at least neq
    !               in your calling program. if you do not
    !               know initial values of some of the solution
    !               components, see the explanation of info(11).
    !
    !  tout - set it to the first point at which a solution
    !         is desired. you can not take tout = t.
    !         integration either forward in t (tout > t) or
    !         backward in t (tout < t) is permitted.
    !
    !         the code advances the solution from t to tout using
    !         step sizes which are automatically selected so as to
    !         achieve the desired accuracy. if you wish, the code will
    !         return with the solution and its derivative at
    !         intermediate steps (intermediate-output mode) so that
    !         you can monitor them, but you still must provide tout in
    !         accord with the basic aim of the code.
    !
    !         the first step taken by the code is a critical one
    !         because it must reflect how fast the solution changes near
    !         the initial point. the code automatically selects an
    !         initial step size which is practically always suitable for
    !         the problem. by using the fact that the code will not step
    !         past tout in the first step, you could, if necessary,
    !         restrict the length of the initial step size.
    !
    !         for some problems it may not be permissable to integrate
    !         past a point tstop because a discontinuity occurs there
    !         or the solution or its derivative is not defined beyond
    !         tstop. when you have declared a tstop point (see info(4)
    !         and rwork(1)), you have told the code not to integrate
    !         past tstop. in this case any tout beyond tstop is invalid
    !         input.
    !
    !  info(*) - use the info array to give the code more details about
    !            how you want your problem solved. this array should be
    !            dimensioned of length 15, though ddassl uses
    !            only the first eleven entries. you must respond to all of
    !            the following items which are arranged as questions. the
    !            simplest use of the code corresponds to answering all
    !            questions as yes, i.e. setting all entries of info to 0.
    !
    !       info(1) - this parameter enables the code to initialize
    !              itself. you must set it to indicate the start of every
    !              new problem.
    !
    !          **** is this the first call for this problem ...
    !                yes - set info(1) = 0
    !                 no - not applicable here.
    !                      see below for continuation calls.  ****
    !
    !       info(2) - how much accuracy you want of your solution
    !              is specified by the error tolerances rtol and atol.
    !              the simplest use is to take them both to be scalars.
    !              to obtain more flexibility, they can both be vectors.
    !              the code must be told your choice.
    !
    !          **** are both error tolerances rtol, atol scalars ...
    !                yes - set info(2) = 0
    !                      and input scalars for both rtol and atol
    !                 no - set info(2) = 1
    !                      and input arrays for both rtol and atol ****
    !
    !       info(3) - the code integrates from t in the direction
    !              of tout by steps. if you wish, it will return the
    !              computed solution and derivative at the next
    !              intermediate step (the intermediate-output mode) or
    !              tout, whichever comes first. this is a good way to
    !              proceed if you want to see the behavior of the solution.
    !              if you must have solutions at a great many specific
    !              tout points, this code will compute them efficiently.
    !
    !          **** do you want the solution only at
    !                tout (and not at the next intermediate step) ...
    !                 yes - set info(3) = 0
    !                  no - set info(3) = 1 ****
    !
    !       info(4) - to handle solutions at a great many specific
    !              values tout efficiently, this code may integrate past
    !              tout and interpolate to obtain the result at tout.
    !              sometimes it is not possible to integrate beyond some
    !              point tstop because the equation changes there or it is
    !              not defined past tstop. then you must tell the code
    !              not to go past.
    !
    !           **** can the integration be carried out without any
    !                restrictions on the independent variable t ...
    !                 yes - set info(4)=0
    !                  no - set info(4)=1
    !                       and define the stopping point tstop by
    !                       setting rwork(1)=tstop ****
    !
    !       info(5) - to solve differential/algebraic problems it is
    !              necessary to use a matrix of partial derivatives of the
    !              system of differential equations. if you do not
    !              provide a subroutine to evaluate it analytically (see
    !              description of the item jac in the call list), it will
    !              be approximated by numerical differencing in this code.
    !              although it is less trouble for you to have the code
    !              compute partial derivatives by numerical differencing,
    !              the solution will be more reliable if you provide the
    !              derivatives via jac. sometimes numerical differencing
    !              is cheaper than evaluating derivatives in jac and
    !              sometimes it is not - this depends on your problem.
    !
    !           **** do you want the code to evaluate the partial
    !                derivatives automatically by numerical differences ...
    !                   yes - set info(5)=0
    !                    no - set info(5)=1
    !                  and provide subroutine jac for evaluating the
    !                  matrix of partial derivatives ****
    !
    !       info(6) - ddassl will perform much better if the matrix of
    !              partial derivatives, dg/dy + cj*dg/dyprime,
    !              (here cj is a scalar determined by ddassl)
    !              is banded and the code is told this. in this
    !              case, the storage needed will be greatly reduced,
    !              numerical differencing will be performed much cheaper,
    !              and a number of important algorithms will execute much
    !              faster. the differential equation is said to have
    !              half-bandwidths ml (lower) and mu (upper) if equation i
    !              involves only unknowns y(j) with
    !                             i-ml <= j <= i+mu
    !              for all i=1,2,...,neq. thus, ml and mu are the widths
    !              of the lower and upper parts of the band, respectively,
    !              with the main diagonal being excluded. if you do not
    !              indicate that the equation has a banded matrix of partial
    !              derivatives, the code works with a full matrix of neq**2
    !              elements (stored in the conventional way). computations
    !              with banded matrices cost less time and storage than with
    !              full matrices if 2*ml+mu < neq. if you tell the
    !              code that the matrix of partial derivatives has a banded
    !              structure and you want to provide subroutine jac to
    !              compute the partial derivatives, then you must be careful
    !              to store the elements of the matrix in the special form
    !              indicated in the description of jac.
    !
    !          **** do you want to solve the problem using a full
    !               (dense) matrix (and not a special banded
    !               structure) ...
    !                yes - set info(6)=0
    !                 no - set info(6)=1
    !                       and provide the lower (ml) and upper (mu)
    !                       bandwidths by setting
    !                       iwork(1)=ml
    !                       iwork(2)=mu ****
    !
    !
    !        info(7) -- you can specify a maximum (absolute value of)
    !              stepsize, so that the code
    !              will avoid passing over very
    !              large regions.
    !
    !          ****  do you want the code to decide
    !                on its own maximum stepsize?
    !                yes - set info(7)=0
    !                 no - set info(7)=1
    !                      and define hmax by setting
    !                      rwork(2)=hmax ****
    !
    !        info(8) -- differential/algebraic problems
    !              may occaisionally suffer from
    !              severe scaling difficulties on the
    !              first step. if you know a great deal
    !              about the scaling of your problem, you can
    !              help to alleviate this problem by
    !              specifying an initial stepsize ho.
    !
    !          ****  do you want the code to define
    !                its own initial stepsize?
    !                yes - set info(8)=0
    !                 no - set info(8)=1
    !                      and define ho by setting
    !                      rwork(3)=ho ****
    !
    !        info(9) -- if storage is a severe problem,
    !              you can save some locations by
    !              restricting the maximum order maxord.
    !              the default value is 5. for each
    !              order decrease below 5, the code
    !              requires neq fewer locations, however
    !              it is likely to be slower. in any
    !              case, you must have 1 <= maxord <= 5
    !          ****  do you want the maximum order to
    !                default to 5?
    !                yes - set info(9)=0
    !                 no - set info(9)=1
    !                      and define maxord by setting
    !                      iwork(3)=maxord ****
    !
    !        info(10) --if you know that the solutions to your equations
    !               will always be nonnegative, it may help to set this
    !               parameter. however, it is probably best to
    !               try the code without using this option first,
    !               and only to use this option if that doesn't
    !               work very well.
    !           ****  do you want the code to solve the problem without
    !                 invoking any special nonnegativity constraints?
    !                  yes - set info(10)=0
    !                   no - set info(10)=1
    !
    !        info(11) --ddassl normally requires the initial t,
    !               y, and yprime to be consistent. that is,
    !               you must have g(t,y,yprime) = 0 at the initial
    !               time. if you do not know the initial
    !               derivative precisely, you can let ddassl try
    !               to compute it.
    !          ****   are the initialhe initial t, y, yprime consistent?
    !                 yes - set info(11) = 0
    !                  no - set info(11) = 1,
    !                       and set yprime to an initial approximation
    !                       to yprime.  (if you have no idea what
    !                       yprime should be, set it to zero. note
    !                       that the initial y should be such
    !                       that there must exist a yprime so that
    !                       g(t,y,yprime) = 0.)
    !
    !   rtol, atol -- you must assign relative (rtol) and absolute (atol
    !               error tolerances to tell the code how accurately you
    !               want the solution to be computed. they must be defined
    !               as variables because the code may change them. you
    !               have two choices --
    !                     both rtol and atol are scalars. (info(2)=0)
    !                     both rtol and atol are vectors. (info(2)=1)
    !               in either case all components must be non-negative.
    !
    !               the tolerances are used by the code in a local error
    !               test at each step which requires roughly that
    !                     abs(local error) <= rtol*abs(y)+atol
    !               for each vector component.
    !               (more specifically, a root-mean-square norm is used to
    !               measure the size of vectors, and the error test uses the
    !               magnitude of the solution at the beginning of the step.)
    !
    !               the true (global) error is the difference between the
    !               true solution of the initial value problem and the
    !               computed approximation. practically all present day
    !               codes, including this one, control the local error at
    !               each step and do not even attempt to control the global
    !               error directly.
    !               usually, but not always, the true accuracy of the
    !               computed y is comparable to the error tolerances. this
    !               code will usually, but not always, deliver a more
    !               accurate solution if you reduce the tolerances and
    !               integrate again. by comparing two such solutions you
    !               can get a fairly reliable idea of the true error in the
    !               solution at the bigger tolerances.
    !
    !               setting atol=0. results in a pure relative error test on
    !               that component. setting rtol=0. results in a pure
    !               absolute error test on that component. a mixed test
    !               with non-zero rtol and atol corresponds roughly to a
    !               relative error test when the solution component is much
    !               bigger than atol and to an absolute error test when the
    !               solution component is smaller than the threshhold atol.
    !
    !               the code will not attempt to compute a solution at an
    !               accuracy unreasonable for the machine being used. it
    !               will advise you if you ask for too much accuracy and
    !               inform you as to the maximum accuracy it believes
    !               possible.
    !
    !  rwork(*) --  dimension this real work array of length lrw in your
    !               calling program.
    !
    !  lrw -- set it to the declared length of the rwork array.
    !               you must have
    !                    lrw >= 40+(maxord+4)*neq+neq**2
    !               for the full (dense) jacobian case (when info(6)=0), or
    !                    lrw >= 40+(maxord+4)*neq+(2*ml+mu+1)*neq
    !               for the banded user-defined jacobian case
    !               (when info(5)=1 and info(6)=1), or
    !                     lrw >= 40+(maxord+4)*neq+(2*ml+mu+1)*neq
    !                           +2*(neq/(ml+mu+1)+1)
    !               for the banded finite-difference-generated jacobian case
    !               (when info(5)=0 and info(6)=1)
    !
    !  iwork(*) --  dimension this integer work array of length liw in
    !               your calling program.
    !
    !  liw -- set it to the declared length of the iwork array.
    !               you must have liw >= 20+neq
    !
    !  rpar, ipar -- these are parameter arrays, of real and integer
    !               type, respectively. you can use them for communication
    !               between your program that calls ddassl and the
    !               res subroutine (and the jac subroutine). they are not
    !               altered by ddassl. if you do not need rpar or ipar,
    !               ignore these parameters by treating them as dummy
    !               arguments. if you do choose to use them, dimension
    !               them in your calling program and in res (and in jac)
    !               as arrays of appropriate length.
    !
    !  jac -- if you have set info(5)=0, you can ignore this parameter
    !               by treating it as a dummy argument. otherwise, you must
    !               provide a subroutine of the form
    !               jac(t,y,yprime,pd,cj,rpar,ipar)
    !               to define the matrix of partial derivatives
    !               pd=dg/dy+cj*dg/dyprime
    !               cj is a scalar which is input to jac.
    !               for the given values of t,y,yprime, the
    !               subroutine must evaluate the non-zero partial
    !               derivatives for each equation and each solution
    !               component, and store these values in the
    !               matrix pd. the elements of pd are set to zero
    !               before each call to jac so only non-zero elements
    !               need to be defined.
    !
    !               subroutine jac must not alter t,y,(*),yprime(*), or cj.
    !               you must declare the name jac in an
    !               external statement in your program that calls
    !               ddassl. you must dimension y, yprime and pd
    !               in jac.
    !
    !               the way you must store the elements into the pd matrix
    !               depends on the structure of the matrix which you
    !               indicated by info(6).
    !               *** info(6)=0 -- full (dense) matrix ***
    !                   give pd a first dimension of neq.
    !                   when you evaluate the (non-zero) partial derivative
    !                   of equation i with respect to variable j, you must
    !                   store it in pd according to
    !                   pd(i,j) = * dg(i)/dy(j)+cj*dg(i)/dyprime(j)*
    !               *** info(6)=1 -- banded jacobian with ml lower and mu
    !                   upper diagonal bands (refer to info(6) description
    !                   of ml and mu) ***
    !                   give pd a first dimension of 2*ml+mu+1.
    !                   when you evaluate the (non-zero) partial derivative
    !                   of equation i with respect to variable j, you must
    !                   store it in pd according to
    !                   irow = i - j + ml + mu + 1
    !                   pd(irow,j) = *dg(i)/dy(j)+cj*dg(i)/dyprime(j)*
    !               rpar and ipar are real and integer parameter arrays
    !               which you can use for communication between your calling
    !               program and your jacobian subroutine jac. they are not
    !               altered by ddassl. if you do not need rpar or ipar,
    !               ignore these parameters by treating them as dummy
    !               arguments. if you do choose to use them, dimension
    !               them in your calling program and in jac as arrays of
    !               appropriate length.
    !
    !
    !
    !  optionally replaceable norm routine:
    !  ddassl uses a weighted norm ddanrm to measure the size
    !  of vectors such as the estimated error in each step.
    !  a function subprogram
    !    real(8) function ddanrm(neq,v,wt,ipar)
    !    dimension v(neq),wt(neq)
    !  is used to define this norm. here, v is the vector
    !  whose norm is to be computed, and wt is a vector of
    !  weights.  a ddanrm routine has been included with ddassl
    !  which computes the weighted root-mean-square norm
    !  given by
    !    ddanrm=sqrt((1/neq)*sum(v(i)/wt(i))**2)
    !  this norm is suitable for most problems. in some
    !  special cases, it may be more convenient and/or
    !  efficient to define your own norm by writing a function
    !  subprogram to be called instead of ddanrm. this should
    !  ,however, be attempted only after careful thought and
    !  consideration.
    !
    !
    !------output-after any return from ddassl----
    !
    !  the principal aim of the code is to return a computed solution at
    !  tout, although it is also possible to obtain intermediate results
    !  along the way. to find out whether the code achieved its goal
    !  or if the integration process was interrupted before the task was
    !  completed, you must check the idid parameter.
    !
    !
    !   t -- the solution was successfully advanced to the
    !               output value of t.
    !
    !     y(*) -- contains the computed solution approximation at t.
    !
    !     yprime(*) -- contains the computed derivative
    !               approximation at t.
    !
    !     idid -- reports what the code did.
    !
    !                     *** task completed ***
    !                reported by positive values of idid
    !
    !     idid = 1 -- a step was successfully taken in the
    !       intermediate-output mode. the code has not yet reached tout.
    !
    !     idid = 2 -- the integration to tout was successfully
    !       completed (t=tout) by stepping exactly to tout.
    !
    !     idid = 3 -- the integration to tout was successfully completed (t=tout) by 
    !       stepping past tout. y(*) is obtained by interpolation. yprime(*) is obtained by interpolation.
    !
    !                    *** task interrupted ***
    !                reported by negative values of idid
    !
    !     idid = -1, the code has taken about 500 steps. if you want to continue, set info(1) = 1 and
    !       call the code again. an additional 500 steps will be allowed.
    !
    !     idid = -2, the error tolerances are too stringent. the error tolerances rtol, atol have been
    !       increased to values the code estimates appropriate for continuing. you may want to 
    !	change them yourself. if you are sure you want to continue with relaxed error 
    !	tolerances, set info(1)=1 and call the code again.
    !
    !     idid = -3  the local error test cannot be satisfied because you specified a zero component in atol
    !       and the corresponding computed solution component is zero. thus, a pure relative error
    !       test is impossible for this component. a solution component is zero and you set the
    !       corresponding component of atol to zero. if you are sure you want to continue, you must 
    !	first alter the error criterion to use positive values for those components of atol 
    !	corresponding to zero solution components, then set info(1)=1 and call the code again.
    !
    !     idid = -6, repeated error test failures occurred on the last attempted step in ddassl. a singularity 
    !     	in the solution may be present. if you are absolutely certain you want to continue, you 
    !	should restart the integration. (provide initial values of y and yprime which are consistent)
    !
    !     idid = -7, repeated convergence test failures occurred on the last attempted step in ddassl. an 
    !     	inaccurate or ill-conditioned jacobian may be the problem. if you are absolutely certain 
    !	you want to continue, you should restart the integration.
    !
    !     idid = -8, the matrix of partial derivatives is singular. some of your equations may be redundant.
    !       could be removed, and then ddassl could solve the problem. it is also possible
    !       that a solution to your problem either does not exist or is not unique.
    !
    !     idid = -9, ddassl had multiple convergence test (corrector) failures, preceeded by multiple error
    !       test failures, on the last attempted step. it is possible that your problem is ill-posed, 
    !	and cannot be solved using this code. or, there may be a discontinuity or a singularity in 
    !	the solution. if you are absolutely certain you want to continue, you should restart the integration.
    !
    !     idid =-10, ddassl had multiple convergence test failures because ires was equal to minus one.
    !       if you are absolutely certain you want to continue, you should restart the integration.
    !
    !     idid =-11, ires=-2 was encountered, and control is being returned to the calling program.
    !
    !     idid =-12, ddassl failed to compute the initial yprime. this could happen because the initial 
    !       approximation to yprime was not very good, or if a yprime consistent with the initial y does not 
    !       exist. the problem could also be caused by an inaccurate or singular iteration matrix.
    !
    !
    !     idid = -13,..,-32 -- not applicable for this code
    !
    !                    *** task terminated ***
    !                reported by the value of idid=-33
    !
    !     idid = -33 -- the code has encountered trouble from which
    !       it cannot recover. a message is printed explaining the trouble and control is returned
    !       to the calling program. for example, this occurs when invalid input is detected.
    !
    !     rtol, atol -- these quantities remain unchanged except when idid = -2. in this case, 
    !       the error tolerances have been increased by the code to values which are estimated to
    !       be appropriate for continuing the integration. however, the reported solution at t was obtained using 
    !       the input values of rtol and atol.
    !
    !   rwork, iwork -- contain information which is usually of no
    !               interest to the user but necessary for subsequent calls.
    !               however, you may find use for
    !
    !               rwork(3)--which contains the step size h to be
    !                       attempted on the next step.
    !
    !               rwork(4)--which contains the current value of the
    !                       independent variable, i.e., the farthest point
    !                       integration has reached. this will be different
    !                       from t only when interpolation has been
    !                       performed (idid=3).
    !
    !               rwork(7)--which contains the stepsize used
    !                       on the last successful step.
    !
    !               iwork(7)--which contains the order of the method to
    !                       be attempted on the next step.
    !
    !               iwork(8)--which contains the order of the method used
    !                       on the last step.
    !
    !               iwork(11)--which contains the number of steps taken so
    !                        far.
    !
    !               iwork(12)--which contains the number of calls to res
    !                        so far.
    !
    !               iwork(13)--which contains the number of evaluations of
    !                        the matrix of partial derivatives needed so
    !                        far.
    !
    !               iwork(14)--which contains the total number
    !                        of error test failures so far.
    !
    !               iwork(15)--which contains the total number
    !                        of convergence test failures so far.
    !                        (includes singular iteration matrix
    !                        failures.)
    !
    !
    !
    !   input -- what to do to continue the integration
    !            (calls after the first)                **
    !
    !     this code is organized so that subsequent calls to continue the
    !     integration involve little (if any) additional effort on your
    !     part. you must monitor the idid parameter in order to determine
    !     what to do next.
    !
    !     recalling that the principal task of the code is to integrate
    !     from t to tout (the interval mode), usually all you will need
    !     to do is specify a new tout upon reaching the current tout.
    !
    !     do not alter any quantity not specifically permitted below,
    !     in particular do not alter neq,t,y(*),yprime(*),rwork(*),iwork(*)
    !     or the differential equation in subroutine res. any such
    !     alteration constitutes a new problem and must be treated as such,
    !     i.e., you must start afresh.
    !
    !     you cannot change from vector to scalar error control or vice
    !     versa (info(2)), but you can change the size of the entries of
    !     rtol, atol. increasing a tolerance makes the equation easier
    !     to integrate. decreasing a tolerance will make the equation
    !     harder to integrate and should generally be avoided.
    !
    !     you can switch from the intermediate-output mode to the
    !     interval mode (info(3)) or vice versa at any time.
    !
    !     if it has been necessary to prevent the integration from going
    !     past a point tstop (info(4), rwork(1)), keep in mind that the
    !     code will not integrate to any tout beyond the currently
    !     specified tstop. once tstop has been reached you must change
    !     the value of tstop or set info(4)=0. you may change info(4)
    !     or tstop at any time but you must supply the value of tstop in
    !     rwork(1) whenever you set info(4)=1.
    !
    !     do not change info(5), info(6), iwork(1), or iwork(2)
    !     unless you are going to restart the code.
    !

    !  ---------------------------------------------------------------------
    !
    !***references  a description of dassl: a differential/algebraic
    !                 system solver, l. r. petzold, sand82-8637,
    !                 sandia national laboratories, september 1982.
    !***routines called  ddastp,ddaini,ddanrm,ddawts,ddatrp,xerrwv,d1mach
    !***end prologue  ddassl
    !
    !**end
    !
    !implicit real(8)(a-h,o-z)
    implicit none
    logical done
    external res,jac
    real(8) :: y(*),yprime(*),rwork(*),rtol(*),atol(*),rpar(*)
    integer :: info(15), iwork(*),ipar(*)
    character :: msg*80, mesg*128
    
    real(8) :: uround, tn, rtoli, atoli, hmin, hmax, t, tout, d1mach, tdist, ho, ypnorm, ddanrm, dsign, rh, abs, tstop, h, tnext, r
    integer :: i, neq, mxord, lenpd, lenrw, jacdim, jacd, mband, msave, leniw, lrw, liw, idid, nzflg, le, lwt, lphi, lpd, lwm, ntemp, itemp
    !
    !     set pointers into iwork
    integer, parameter :: lml=1, lmu=2, lmxord=3, lmtype=4, lnst=11, lnre=12, lnje=13, letf=14, lctf=15, lnpd=16, lipvt=21, ljcalc=5, lphase=6, lk=7, lkold=8, lns=9, lnstl=10, liwm=1
    !
    !     set relative offset into rwork
    integer, parameter :: npd=1
    !
    !     set pointers into rwork
    integer, parameter :: ltstop=1, lhmax=2, lh=3, ltn=4, lcj=5, lcjold=6, lhold=7, ls=8, lround=9, lalpha=11, lbeta=17, lgamma=23, lpsi=29, lsigma=35, ldelta=41
    !
    !*** added by gpf 11/21/91 in case the "no remember"
    !
    save
    !
    !***first executable statement  ddassl
    if(info(1)/=0)go to 100
    !
    !-----------------------------------------------------------------------
    !     this block is executed for the initial call only.
    !     it contains checking of inputs and initializations.
    !-----------------------------------------------------------------------
    !
    !     first check info array to make sure all elements of info
    !     are either zero or one.
    do i=2,11
        !
        !     added by par 01/08/93 to allow and added option for solving the jacobian
        !
        if (i==5) then
            if (info(5)/=0.and.info(5)/=1.and.info(5)/=2) go to 701
        else
            if(info(i)/=0.and.info(i)/=1) go to 701
        endif
    end do       
    !
    if(neq<=0)go to 702
    !
    !     check and compute maximum order
    mxord=5
    if(info(9)==0)go to 20
    mxord=iwork(lmxord)
    if(mxord<1.or.mxord>5)go to 703
20  iwork(lmxord)=mxord
    !
    !     compute mtype,lenpd,lenrw.check ml and mu.
    if(info(6)/=0)go to 40
    lenpd=neq**2
    lenrw=40+(iwork(lmxord)+4)*neq+lenpd
    if(info(5)/=0)go to 30
    iwork(lmtype)=2
    go to 60
    !
    !     modified by par 01/08/93 to allow for custom solution of jacobian
    !
30  if (info(5)==1) then
        iwork(lmtype)=1
    else
        iwork(lmtype)=6
        jacdim = jacd()
        lenpd = jacdim**2
        lenrw=40+(iwork(lmxord)+4)*neq+lenpd
    endif
    go to 60
40  if(iwork(lml)<0.or.iwork(lml)>=neq)go to 717
    if(iwork(lmu)<0.or.iwork(lmu)>=neq)go to 718
    lenpd=(2*iwork(lml)+iwork(lmu)+1)*neq
    if(info(5)/=0)go to 50
    iwork(lmtype)=5
    mband=iwork(lml)+iwork(lmu)+1
    msave=(neq/mband)+1
    lenrw=40+(iwork(lmxord)+4)*neq+lenpd+2*msave
    go to 60
50  iwork(lmtype)=4
    lenrw=40+(iwork(lmxord)+4)*neq+lenpd
    !
    !     check lengths of rwork and iwork
60  leniw=20+neq
    iwork(lnpd)=lenpd
    if(lrw<lenrw)go to 704
    if(liw<leniw)go to 705
    !
    !     check to see that tout is different from t
    if(tout == t)go to 719
    !
    !     check hmax
    if(info(7)==0)go to 70
    hmax=rwork(lhmax)
    if(hmax<=0.0d0)go to 710
70  continue
    !
    !     initialize counters
    iwork(lnst)=0
    iwork(lnre)=0
    iwork(lnje)=0
    !
    iwork(lnstl)=0
    idid=1
    go to 200
    !
    !-----------------------------------------------------------------------
    !     this block is for continuation calls
    !     only. here we check info(1),and if the
    !     last step was interrupted we check whether
    !     appropriate action was taken.
    !-----------------------------------------------------------------------
    !
100 continue
    if(info(1)==1)go to 110
    if(info(1)/=-1)go to 701
    !     if we are here, the last step was interrupted
    !     by an error condition from ddastp,and
    !     appropriate action was not taken. this
    !     is a fatal error.
    msg = 'dassl--  the last step terminated with a negative'
    call xerrwv(msg,49,201,0,0,0,0,0,0.0d0,0.0d0)
    msg = 'dassl--  value (=i1) of idid and no appropriate'
    call xerrwv(msg,47,202,0,1,idid,0,0,0.0d0,0.0d0)
    msg = 'dassl--  action was taken. run terminated'
    call xerrwv(msg,41,203,1,0,0,0,0,0.0d0,0.0d0)
    return
110 continue
    iwork(lnstl)=iwork(lnst)
    !
    !-----------------------------------------------------------------------
    !     this block is executed on all calls.
    !     the error tolerance parameters are
    !     checked, and the work array pointers
    !     are set.
    !-----------------------------------------------------------------------
    !
200 continue
    !     check rtol,atol
    nzflg=0
    rtoli=rtol(1)
    atoli=atol(1)
    do 210 i=1,neq
        if(info(2)==1)rtoli=rtol(i)
        if(info(2)==1)atoli=atol(i)
        if(rtoli>0.0d0.or.atoli>0.0d0)nzflg=1
        if(rtoli<0.0d0)go to 706
        if(atoli<0.0d0)go to 707
210 continue
    if(nzflg==0)go to 708
    !
    !     set up rwork storage.iwork storage is fixed
    !     in data statement.
    le=ldelta+neq
    lwt=le+neq
    lphi=lwt+neq
    lpd=lphi+(iwork(lmxord)+1)*neq
    lwm=lpd
    ntemp=npd+iwork(lnpd)
    if(info(1)==1)go to 400
    !
    !-----------------------------------------------------------------------
    !     this block is executed on the initial call
    !     only. set the initial step size, and
    !     the error weight vector, and phi.
    !     compute initial yprime, if necessary.
    !-----------------------------------------------------------------------
    !
300 continue
    tn=t
    idid=1
    !
    !     set error weight vector wt
    call ddawts(neq,info(2),rtol,atol,y,rwork(lwt))
    do 305 i = 1,neq
        if(rwork(lwt+i-1)<=0.0d0) go to 713
305 continue
    !
    !     compute unit roundoff and hmin
    uround = d1mach(4)
    rwork(lround) = uround
    hmin = 4.0d0*uround*dmax1(abs(t),abs(tout))
    !
    !     check initial interval to see that it is long enough
    tdist = abs(tout - t)
    if(tdist < hmin) go to 714
    !
    !     check ho, if this was input
    if (info(8) == 0) go to 310
    ho = rwork(lh)
    if ((tout - t)*ho < 0.0d0) go to 711
    if (ho == 0.0d0) go to 712
    go to 320
310 continue
    !
    !     compute initial stepsize, to be used by either
    !     ddastp or ddaini, depending on info(11)
    ho = 0.001d0*tdist
    ypnorm = ddanrm(neq,yprime,rwork(lwt),ipar)
    if (ypnorm > 0.5d0/ho) ho = 0.5d0/ypnorm
    ho = dsign(ho,tout-t)
    !     adjust ho if necessary to meet hmax bound
320 if (info(7) == 0) go to 330
    rh = abs(ho)/rwork(lhmax)
    if (rh > 1.0d0) ho = ho/rh
    !     compute tstop, if applicable
330 if (info(4) == 0) go to 340
    tstop = rwork(ltstop)
    if ((tstop - t)*ho < 0.0d0) go to 715
    if ((t + ho - tstop)*ho > 0.0d0) ho = tstop - t
    if ((tstop - tout)*ho < 0.0d0) go to 709
    !
    !     compute initial derivative, updating tn and y, if applicable
340 if (info(11) == 0) go to 350
    call ddaini(tn,y,yprime,neq,res,jac,ho,rwork(lwt),idid,rpar,ipar,rwork(lphi),rwork(ldelta),rwork(le),rwork(lwm),iwork(liwm),hmin,rwork(lround),info(10),ntemp)
    if (idid < 0) go to 390
    !
    !     load h with ho.  store h in rwork(lh)
350 h = ho
    rwork(lh) = h
    !
    !     load y and h*yprime into phi(*,1) and phi(*,2)
360 itemp = lphi + neq
    do i = 1,neq
        rwork(lphi + i - 1) = y(i)
        rwork(itemp + i - 1) = h*yprime(i)
    end do
    !
390 go to 500
    !
    !-------------------------------------------------------
    !     this block is for continuation calls only. its
    !     purpose is to check stop conditions before
    !     taking a step.
    !     adjust h if necessary to meet hmax bound
    !-------------------------------------------------------
    !
400 continue
    uround=rwork(lround)
    done = .false.
    tn=rwork(ltn)
    h=rwork(lh)
    if(info(7) == 0) go to 410
    rh = abs(h)/rwork(lhmax)
    if(rh > 1.0d0) h = h/rh
410 continue
    if(t == tout) go to 719
    if((t - tout)*h > 0.0d0) go to 711
    if(info(4) == 1) go to 430
    if(info(3) == 1) go to 420
    if((tn-tout)*h<0.0d0)go to 490
    call ddatrp(tn,tout,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    t=tout
    idid = 3
    done = .true.
    go to 490
420 if((tn-t)*h <= 0.0d0) go to 490
    if((tn - tout)*h > 0.0d0) go to 425
    call ddatrp(tn,tn,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    t = tn
    idid = 1
    done = .true.
    go to 490
425 continue
    call ddatrp(tn,tout,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    t = tout
    idid = 3
    done = .true.
    go to 490
430 if(info(3) == 1) go to 440
    tstop=rwork(ltstop)
    if((tn-tstop)*h>0.0d0) go to 715
    if((tstop-tout)*h<0.0d0)go to 709
    if((tn-tout)*h<0.0d0)go to 450
    call ddatrp(tn,tout,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    t=tout
    idid = 3
    done = .true.
    go to 490
440 tstop = rwork(ltstop)
    if((tn-tstop)*h > 0.0d0) go to 715
    if((tstop-tout)*h < 0.0d0) go to 709
    if((tn-t)*h <= 0.0d0) go to 450
    if((tn - tout)*h > 0.0d0) go to 445
    call ddatrp(tn,tn,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    t = tn
    idid = 1
    done = .true.
    go to 490
445 continue
    call ddatrp(tn,tout,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    t = tout
    idid = 3
    done = .true.
    go to 490
450 continue
    !     check whether we are with in roundoff of tstop
    if(abs(tn-tstop)>100.0d0*uround*(abs(tn)+abs(h)))go to 460
    idid=2
    t=tstop
    done = .true.
    go to 490
460 tnext=tn+h*(1.0d0+4.0d0*uround)
    if((tnext-tstop)*h<=0.0d0)go to 490
    h=(tstop-tn)*(1.0d0-4.0d0*uround)
    rwork(lh)=h
    !
490 if (done) go to 590
    !
    !-------------------------------------------------------
    !     the next block contains the call to the
    !     one-step integrator ddastp.
    !     this is a looping point for the integration steps.
    !     check for too many steps.
    !     update wt.
    !     check for too much accuracy requested.
    !     compute minimum stepsize.
    !-------------------------------------------------------
    !
500 continue
    !     check for failure to compute initial yprime
    if (idid == -12) go to 527
    !
    !     check for too many steps
    if((iwork(lnst)-iwork(lnstl))<500)go to 510
    idid=-1
    go to 527
    !
    !     update wt
510 call ddawts(neq,info(2),rtol,atol,rwork(lphi),rwork(lwt))
    do 520 i=1,neq
        if(rwork(i+lwt-1)>0.0d0)go to 520
        idid=-3
        go to 527
520 continue
    !
    !     test for too much accuracy requested.
    r=ddanrm(neq,rwork(lphi),rwork(lwt),ipar)*100.0d0*uround
    if(r<=1.0d0)go to 525
    !     multiply rtol and atol by r and return
    if(info(2)==1)go to 523
    rtol(1)=r*rtol(1)
    atol(1)=r*atol(1)
    idid=-2
    go to 527
523 do i=1,neq
        rtol(i)=r*rtol(i)
        atol(i)=r*atol(i)
    end do
    idid=-2
    go to 527
525 continue
    !
    !     compute minimum stepsize
    hmin=4.0d0*uround*dmax1(abs(tn),abs(tout))
    !
    call ddastp(tn,y,yprime,neq,res,jac,h,rwork(lwt),info(1),idid,rpar,ipar,rwork(lphi),rwork(ldelta),rwork(le),rwork(lwm),iwork(liwm), &
    rwork(lalpha),rwork(lbeta),rwork(lgamma),rwork(lpsi),rwork(lsigma),rwork(lcj),rwork(lcjold),rwork(lhold),rwork(ls),hmin,rwork(lround), &
    iwork(lphase),iwork(ljcalc),iwork(lk),iwork(lkold),iwork(lns),info(10),ntemp)
527 if(idid<0)go to 600
    !
    !--------------------------------------------------------
    !     this block handles the case of a successful return
    !     from ddastp (idid=1).  test for stop conditions.
    !--------------------------------------------------------
    !
    if(info(4)/=0)go to 540
    if(info(3)/=0)go to 530
    if((tn-tout)*h<0.0d0)go to 500
    call ddatrp(tn,tout,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    idid=3
    t=tout
    go to 580
530 if((tn-tout)*h>=0.0d0)go to 535
    t=tn
    idid=1
    go to 580
535 call ddatrp(tn,tout,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    idid=3
    t=tout
    go to 580
540 if(info(3)/=0)go to 550
    if((tn-tout)*h<0.0d0)go to 542
    call ddatrp(tn,tout,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    t=tout
    idid=3
    go to 580
542 if(abs(tn-tstop)<=100.0d0*uround*(abs(tn)+abs(h)))go to 545
    tnext=tn+h*(1.0d0+4.0d0*uround)
    if((tnext-tstop)*h<=0.0d0)go to 500
    h=(tstop-tn)*(1.0d0-4.0d0*uround)
    go to 500
545 idid=2
    t=tstop
    go to 580
550 if((tn-tout)*h>=0.0d0)go to 555
    if(abs(tn-tstop)<=100.0d0*uround*(abs(tn)+abs(h)))go to 552
    t=tn
    idid=1
    go to 580
552 idid=2
    t=tstop
    go to 580
555 call ddatrp(tn,tout,y,yprime,neq,iwork(lkold),rwork(lphi),rwork(lpsi))
    t=tout
    idid=3
580 continue

    !-----------------------------------------------------------------
    !     all successful returns from ddassl are made from this block.
    !-----------------------------------------------------------------

590 continue
    rwork(ltn)=tn
    rwork(lh)=h
    return

    !------------------------------------------------------------------------------
    !     this block handles all unsuccessful returns other than for illegal input.
    !------------------------------------------------------------------------------

600 continue
    itemp= iabs(idid)
    go to (610,620,630,690,690,640,650,660,670,675,680,685), itemp

    !     the maximum number of steps was taken before reaching tout

610 mesg = 'at current t (=r1) 500 steps taken on this call before reaching tout'
    call xerrmod(mesg,610,1,tn,0.0d0)
    go to 690

    !     too much accuracy for machine precision
620 mesg = 'at t (=r1) too much accuracy requested for precision of machine. rtol and atol'// ' were increased to appropriate values'
    call xerrmod(mesg,620,1,tn,0.0d0)
    go to 690

630 mesg = 'at t (=r1) some element of wt has become <= 0.0'
    call xerrmod(mesg,630,1,tn,0.0d0)
    go to 690

    !     error test failed repeatedly or with h=hmin
640 mesg = 'at t (=r1) and stepsize h (=r2) the error test failed repeatedly or with abs(h)=hmin'
    call xerrmod(mesg,640,2,tn,h)
    go to 690

    !     corrector convergence failed repeatedly or with h=hmin
650 mesg = 'at t (=r1) and stepsize h (=r2) the corrector failed to converge repeatedly'// ' or with abs(h)=hmin'
    call xerrmod(mesg,650,2,tn,h)
    go to 690

    !     the iteration matrix is singular
660 mesg = 'at t (=r1) and stepsize h (=r2) the iteration matrix is singular'
    call xerrmod(mesg,660,2,tn,h)
    go to 690

    !     corrector failure preceeded by error test failures.
670 mesg = 'at t (=r1) and stepsize h (=r2) the corrector could not converge.  also, the'// ' error test failed repeatedly.'
    call xerrmod(mesg,670,2,tn,h)
    go to 690

    !     corrector failure because ires = -1
675 mesg = 'at t (=r1) and stepsize h (=r2) the corrector could not converge because'// ' ires was equal to minus one'
    call xerrmod(mesg,675,2,tn,h)
    go to 690

    !     failure because ires = -2
680 mesg = 'at t (=r1) and stepsize h (=r2) ires was equal to minus two'
    call xerrmod(mesg,680,2,tn,h)
    go to 690

    !     failed to compute initial yprime
685 mesg = 'at t (=r1) and stepsize h (=r2) the initial yprime could not be computed'
    call xerrmod(mesg,685,2,tn,h)
    go to 690

    !     collective return from errors
690 continue
    info(1)=-1
    t=tn
    rwork(ltn)=tn
    rwork(lh)=h
    return

    !-------------------------------------------------------------------------
    !     this block handles all error returns due to illegal input, as 
    !     detected before calling ddastp. first the error message routine is
    !     called. if this happens twice in succession, execution is terminated
    !-------------------------------------------------------------------------

701 msg = 'dassl--  some element of info vector is not zero or one'
    call xerrwv(msg,55,1,0,0,0,0,0,0.0d0,0.0d0)
    go to 750
702 msg = 'dassl--  neq (=i1) <= 0'
    call xerrwv(msg,25,2,0,1,neq,0,0,0.0d0,0.0d0)
    go to 750
703 msg = 'dassl--  maxord (=i1) not in range'
    call xerrwv(msg,34,3,0,1,mxord,0,0,0.0d0,0.0d0)
    go to 750
704 msg='dassl--  rwork length needed, lenrw (=i1), exceeds lrw (=i2)'
    call xerrwv(msg,60,4,0,2,lenrw,lrw,0,0.0d0,0.0d0)
    go to 750
705 msg='dassl--  iwork length needed, leniw (=i1), exceeds liw (=i2)'
    call xerrwv(msg,60,5,0,2,leniw,liw,0,0.0d0,0.0d0)
    go to 750
706 msg = 'dassl--  some element of rtol is < 0'
    call xerrwv(msg,39,6,0,0,0,0,0,0.0d0,0.0d0)
    go to 750
707 msg = 'dassl--  some element of atol is < 0'
    call xerrwv(msg,39,7,0,0,0,0,0,0.0d0,0.0d0)
    go to 750
708 msg = 'dassl--  all elements of rtol and atol are zero'
    call xerrwv(msg,47,8,0,0,0,0,0,0.0d0,0.0d0)
    go to 750
709 msg='dassl--  info(4) = 1 and tstop (=r1) behind tout (=r2)'
    call xerrwv(msg,54,9,0,0,0,0,2,tstop,tout)
    go to 750
710 msg = 'dassl--  hmax (=r1) < 0.0'
    call xerrwv(msg,28,10,0,0,0,0,1,hmax,0.0d0)
    go to 750
711 msg = 'dassl--  tout (=r1) behind t (=r2)'
    call xerrwv(msg,34,11,0,0,0,0,2,tout,t)
    go to 750
712 msg = 'dassl--  info(8)=1 and h0=0.0'
    call xerrwv(msg,29,12,0,0,0,0,0,0.0d0,0.0d0)
    go to 750
713 msg = 'dassl--  some element of wt is <= 0.0'
    call xerrwv(msg,39,13,0,0,0,0,0,0.0d0,0.0d0)
    go to 750
714 msg='dassl-- tout (=r1) too close to t (=r2) to start integration'
    call xerrwv(msg,60,14,0,0,0,0,2,tout,t)
    go to 750
715 msg = 'dassl--  info(4)=1 and tstop (=r1) behind t (=r2)'
    call xerrwv(msg,49,15,0,0,0,0,2,tstop,t)
    go to 750
717 msg = 'dassl--  ml (=i1) illegal. either < 0 or > neq'
    call xerrwv(msg,52,17,0,1,iwork(lml),0,0,0.0d0,0.0d0)
    go to 750
718 msg = 'dassl--  mu (=i1) illegal. either < 0 or > neq'
    call xerrwv(msg,52,18,0,1,iwork(lmu),0,0,0.0d0,0.0d0)
    go to 750
719 msg = 'dassl--  tout (=r1) is equal to t (=r2)'
    call xerrwv(msg,39,19,0,0,0,0,2,tout,t)
    go to 750
750 if(info(1)==-1) go to 760
    info(1)=-1
    idid=-33
    return
760 msg = 'dassl--  repeated occurrences of illegal input'
    call xerrwv(msg,46,801,0,0,0,0,0,0.0d0,0.0d0)
770 msg = 'dassl--  run terminated. apparent infinite loop'
    call xerrwv(msg,47,802,1,0,0,0,0,0.0d0,0.0d0)
    return
    !-----------end of subroutine ddassl------------------------------------
    end subroutine ddassl

! --------------------------- ddawts -------------------------------------------

    subroutine ddawts(neq,iwt,rtol,atol,y,wt)

    !***begin prologue  ddawts
    !***refer to  ddassl
    !***routines called  (none)
    !***date written   830315   (yymmdd)
    !***revision date  891228   (yymmdd)
    !***end prologue  ddawts
    !-----------------------------------------------------------------------
    !     this subroutine sets the error weight vector
    !     wt according to wt(i)=rtol(i)*abs(y(i))+atol(i),
    !     i=1,-,n.
    !     rtol and atol are scalars if iwt = 0,
    !     and vectors if iwt = 1.
    !-----------------------------------------------------------------------
    !
    implicit none
    real(8) :: rtol(*),atol(*),y(*),wt(*),atoli, rtoli
    integer :: i, neq, iwt
    !
    !*** added by gpf 11/21/91 in case the "no remember"
    !
    save
    !
    rtoli=rtol(1)
    atoli=atol(1)
    do 20 i=1,neq
        if (iwt ==0) go to 10
        rtoli=rtol(i)
        atoli=atol(i)
10      wt(i)=rtoli*abs(y(i))+atoli
20  continue
    return
    !-----------end of subroutine ddawts------------------------------------
    end
    
    real(8) function ddanrm(neq,v,wt,ipar)
    !
    !***begin prologue  ddanrm
    !***refer to  ddassl
    !***routines called  (none)
    !***date written   830315   (yymmdd)
    !***revision date  891228   (yymmdd)
    !***end prologue  ddanrm
    !-----------------------------------------------------------------------
    !     this function routine computes the weighted
    !     root-mean-square norm of the vector of length
    !     neq contained in the array v,with weights
    !     contained in the array wt of length neq.
    !        ddanrm=sqrt((1/neq)*sum(v(i)/wt(i))**2)
    !-----------------------------------------------------------------------
    !
    implicit none
    
    integer :: ipar(*), i, neq
    real(8) :: v(neq), wt(neq), vmax, sum
    ddanrm = 0.0d0
    vmax = 0.0d0
    ipar(3) = 1
    do i = 1,neq
        if(abs(v(i)/wt(i)) > vmax) then
            vmax = abs(v(i)/wt(i))
            ipar(3) = i
        endif
    end do
    if(vmax <= 0.0d0) go to 30
    sum = 0.0d0
    do  i = 1,neq
        sum = sum + ((v(i)/wt(i))/vmax)**2
    end do
    ddanrm = vmax*sqrt(sum/float(neq))
30  continue
    return
    !------end of function ddanrm------
    end function ddanrm

! --------------------------- ddaini -------------------------------------------

    subroutine ddaini(x,y,yprime,neq,res,jac,h,wt,idid,rpar,ipar,phi,delta,e,wm,iwm,hmin,uround,nonneg,ntemp)
    !
    !***begin prologue  ddaini
    !***refer to  ddassl
    !***routines called  ddanrm,ddajac,ddaslv
    !***date written   830315   (yymmdd)
    !***revision date  891228   (yymmdd)
    !***end prologue ddaini
    !
    !-----------------------------------------------------------------
    !     ddaini takes one step of size h or smaller
    !     with the backward euler method, to
    !     find yprime.  x and y are updated to be consistent with the
    !     new step.  a modified damped newton iteration is used to
    !     solve the corrector iteration.
    !
    !     the initial guess for yprime is used in the
    !     prediction, and in forming the iteration
    !     matrix, but is not involved in the
    !     error test. this may have trouble
    !     converging if the initial guess is no
    !     good, or if g(x,y,yprime) depends
    !     nonlinearly on yprime.
    !
    !     the parameters represent:
    !     x --         independent variable
    !     y --         solution vector at x
    !     yprime --    derivative of solution vector
    !     neq --       number of equations
    !     h --         stepsize. imder may use a stepsize
    !                  smaller than h.
    !     wt --        vector of weights for error
    !                  criterion
    !     idid --      completion code with the following meanings
    !                  idid= 1 -- yprime was found successfully
    !                  idid=-12 -- ddaini failed to find yprime
    !     rpar,ipar -- real and integer parameter arrays
    !                  that are not altered by ddaini
    !     phi --       work space for ddaini
    !     delta,e --   work space for ddaini
    !     wm,iwm --    real and integer arrays storing
    !                  matrix information
    !
    !-----------------------------------------------------------------
    !
    !
    implicit none
    
    logical :: convgd
    integer :: iwm(*), ipar(*), maxit, mjac, idid, nef, ncf, nsf, i, jcalc, m, ires, ier, ntemp, nonneg, neq
    real(8) :: y(*),yprime(*),wt(*), phi(neq,*),delta(*),e(*),wm(*),rpar(*), damp, xold, x, ynorm, ddanrm, cj, h, uround, s, delnrm, oldnrm, rate, err, hmin, r, dmin1
    external res,jac
    !
    integer, parameter :: lnre=12
    integer, parameter :: lnje=13
    !
    !*** added by gpf 11/21/91 in case the "no remember"
    !
    save
    !
    !
    data maxit/10/,mjac/5/
    data damp/0.75d0/
    !
    !
    !---------------------------------------------------
    !     block 1.
    !     initializations.
    !---------------------------------------------------
    !
    idid=1
    nef=0
    ncf=0
    nsf=0
    xold=x
    ynorm=ddanrm(neq,y,wt,ipar)
    !
    !     save y and yprime in phi
    do i=1,neq
        phi(i,1)=y(i)
        phi(i,2)=yprime(i)
    end do
    !
    !
    !----------------------------------------------------
    !     block 2.
    !     do one backward euler step.
    !----------------------------------------------------
    !
    !     set up for start of corrector iteration
200 cj=1.0d0/h
    x=x+h
    !
    !     predict solution and derivative
    do i=1,neq
        y(i)=y(i)+h*yprime(i)
    end do
    !
    jcalc=-1
    m=0
    convgd=.true.
    !
    !
    !     corrector loop.
300 iwm(lnre)=iwm(lnre)+1
    ires=0
    !
    !*** we're about to compute a jacobian so set the iderv
    !    flag, telling res to compute jacobian elements
    !
    call setderv(0)
    !
    call res(x,y,yprime,delta,ires,rpar,ipar)
    call setderv(0)

    if (ires<0) go to 430
    !
    !
    !     evaluate the iteration matrix
    if (jcalc/=-1) go to 310
    iwm(lnje)=iwm(lnje)+1
    jcalc=0
    call ddajac(neq,x,y,yprime,delta,cj,h,ier,wt,e,wm,iwm,res,ires,uround,jac,rpar,ipar,ntemp)
    !
    s=1000000.d0
    if (ires<0) go to 430
    if (ier/=0) go to 430
    nsf=0
    !
    !
    !
    !     multiply residual by damping factor
310 continue
    do i=1,neq
        delta(i)=delta(i)*damp
    end do
    !
    !     compute a new iterate (back substitution)
    !     store the correction in delta
    !
    call ddaslv(neq,delta,wm,iwm)
    !
    !     update y and yprime
    do i=1,neq
        y(i)=y(i)-delta(i)
        yprime(i)=yprime(i)-cj*delta(i)
    end do
    !
    !     test for convergence of the iteration.
    !
    delnrm=ddanrm(neq,delta,wt,ipar)
    if (delnrm<=100.d0*uround*ynorm) go to 400
    !
    if (m>0) go to 340
    oldnrm=delnrm
    go to 350
    !
340 rate=(delnrm/oldnrm)**(1.0d0/float(m))
    if (rate>0.90d0) go to 430
    s=rate/(1.0d0-rate)
    !
350 if (s*delnrm <= 0.33d0) go to 400
    !
    !
    !     the corrector has not yet converged. update
    !     m and and test whether the maximum
    !     number of iterations have been tried.
    !     every mjac iterations, get a new
    !     iteration matrix.
    !
    m=m+1
    if (m>=maxit) go to 430
    !
    if ((m/mjac)*mjac==m) jcalc=-1
    go to 300
    !
    !
    !     the iteration has converged.
    !     check nonnegativity constraints
400 if (nonneg==0) go to 450
    do i=1,neq
        delta(i)=dmin1(y(i),0.0d0)
    end do
    !
    delnrm=ddanrm(neq,delta,wt,ipar)
    if (delnrm>0.33d0) go to 430
    !
    do i=1,neq
        y(i)=y(i)-delta(i)
        yprime(i)=yprime(i)-cj*delta(i)
    end do
    go to 450
    !
    !
    !     exits from corrector loop.
430 convgd=.false.
450 if (.not.convgd) go to 600
    !
    !
    !
    !-----------------------------------------------------
    !     block 3.
    !     the corrector iteration converged.
    !     do error test.
    !-----------------------------------------------------
    !
    do i=1,neq
        e(i)=y(i)-phi(i,1)
    end do
    err=ddanrm(neq,e,wt,ipar)
    !
    if (err<=1.0d0) return
    !
    !
    !
    !--------------------------------------------------------
    !     block 4.
    !     the backward euler step failed. restore x, y
    !     and yprime to their original values.
    !     reduce stepsize and try again, if
    !     possible.
    !---------------------------------------------------------
    !
600 continue
    x = xold
    do i=1,neq
        y(i)=phi(i,1)
        yprime(i)=phi(i,2)
    end do
    !
    if (convgd) go to 640
    if (ier==0) go to 620
    nsf=nsf+1
    h=h*0.25d0
    if (nsf<3.and.abs(h)>=hmin) go to 690
    idid=-12
    return
620 if (ires>-2) go to 630
    idid=-12
    return
630 ncf=ncf+1
    h=h*0.25d0
    if (ncf<10.and.abs(h)>=hmin) go to 690
    idid=-12
    return
    !
640 nef=nef+1
    r=0.90d0/(2.0d0*err+0.0001d0)
    r=dmax1(0.1d0,dmin1(0.5d0,r))
    h=h*r
    if (abs(h)>=hmin.and.nef<10) go to 690
    idid=-12
    return
690 go to 200
    !
    !-------------end of subroutine ddaini----------------------
    end subroutine ddaini

! --------------------------- ddatrp -------------------------------------------

    subroutine ddatrp(x,xout,yout,ypout,neq,kold,phi,psi)
    !
    !***begin prologue  ddatrp
    !***refer to  ddassl
    !***routines called  (none)
    !***date written   830315   (yymmdd)
    !***revision date  891228   (yymmdd)
    !***end prologue  ddatrp
    !
    !-----------------------------------------------------------------------
    !     the methods in subroutine ddastp use polynomials
    !     to approximate the solution. ddatrp approximates the
    !     solution and its derivative at time xout by evaluating
    !     one of these polynomials,and its derivative,there.
    !     information defining this polynomial is passed from
    !     ddastp, so ddatrp cannot be used alone.
    !
    !     the parameters are:
    !     x     the current time in the integration.
    !     xout  the time at which the solution is desired
    !     yout  the interpolated approximation to y at xout
    !           (this is output)
    !     ypout the interpolated approximation to yprime at xout
    !           (this is output)
    !     neq   number of equations
    !     kold  order used on last successful step
    !     phi   array of scaled divided differences of y
    !     psi   array of past stepsize history
    !-----------------------------------------------------------------------
    !
    implicit none
    
    integer :: i, j, neq, koldp1, kold
    real(8) ::  yout(*), ypout(*), phi(neq,*), psi(*), temp1, xout, x, c, d, gamma
    !
    !*** added by gpf 11/21/91 in case the "no remember"
    !
    save
    !
    koldp1=kold+1
    temp1=xout-x
    do i=1,neq
        yout(i)=phi(i,1)
        ypout(i)=0.0d0
    end do
    c=1.0d0
    d=0.0d0
    gamma=temp1/psi(1)
    do j=2,koldp1
        d=d*gamma+c/psi(j-1)
        c=c*gamma
        gamma=(temp1+psi(j-1))/psi(j)
        do i=1,neq
            yout(i)=yout(i)+c*phi(i,j)
            ypout(i)=ypout(i)+d*phi(i,j)
        end do
    end do
    return
    !
    !------end of subroutine ddatrp------
    end subroutine ddatrp

! --------------------------- ddastp -------------------------------------------

    subroutine ddastp(x,y,yprime,neq,res,jac,h,wt,jstart,idid,rpar,ipar,phi,delta,e,wm,iwm,alpha,beta,gamma,psi,sigma,cj,cjold,hold,s,hmin,uround,iphase,jcalc,k,kold,ns,nonneg,ntemp)
    !
    !***begin prologue  ddastp
    !***refer to  ddassl
    !***routines called  ddanrm,ddajac,ddaslv,ddatrp
    !***date written   830315   (yymmdd)
    !***revision date  891228   (yymmdd)
    !***end prologue  ddastp
    !
    !
    !-----------------------------------------------------------------------
    !     ddastp solves a system of differential/
    !     algebraic equations of the form
    !     g(x,y,yprime) = 0,  for one step (normally
    !     from x to x+h).
    !
    !     the methods used are modified divided
    !     difference,fixed leading coefficient
    !     forms of backward differentiation
    !     formulas. the code adjusts the stepsize
    !     and order to control the local error per
    !     step.
    !
    !
    !     the parameters represent
    !     x  --        independent variable
    !     y  --        solution vector at x
    !     yprime --    derivative of solution vector
    !                  after successful step
    !     neq --       number of equations to be integrated
    !     res --       external user-supplied subroutine
    !                  to evaluate the residual.  the call is
    !                  call res(x,y,yprime,delta,ires,rpar,ipar)
    !                  x,y,yprime are input.  delta is output.
    !                  on input, ires=0.  res should alter ires only
    !                  if it encounters an illegal value of y or a
    !                  stop condition.  set ires=-1 if an input value
    !                  of y is illegal, and ddastp will try to solve
    !                  the problem without getting ires = -1.  if
    !                  ires=-2, ddastp returns control to the calling
    !                  program with idid = -11.
    !     jac --       external user-supplied routine to evaluate
    !                  the iteration matrix (this is optional)
    !                  the call is of the form
    !                  call jac(x,y,yprime,pd,cj,rpar,ipar)
    !                  pd is the matrix of partial derivatives,
    !                  pd=dg/dy+cj*dg/dyprime
    !     h --         appropriate step size for next step.
    !                  normally determined by the code
    !     wt --        vector of weights for error criterion.
    !     jstart --    integer variable set 0 for
    !                  first step, 1 otherwise.
    !     idid --      completion code with the following meanings:
    !                  idid= 1 -- the step was completed successfully
    !                  idid=-6 -- the error test failed repeatedly
    !                  idid=-7 -- the corrector could not converge
    !                  idid=-8 -- the iteration matrix is singular
    !                  idid=-9 -- the corrector could not converge.
    !                             there were repeated error test
    !                             failures on this step.
    !                  idid=-10-- the corrector could not converge
    !                             because ires was equal to minus one
    !                  idid=-11-- ires equal to -2 was encountered,
    !                             and control is being returned to
    !                             the calling program
    !     rpar,ipar -- real and integer parameter arrays that
    !                  are used for communication between the
    !                  calling program and external user routines
    !                  they are not altered by ddastp
    !     phi --       array of divided differences used by
    !                  ddastp. the length is neq*(k+1),where
    !                  k is the maximum order
    !     delta,e --   work vectors for ddastp of length neq
    !     wm,iwm --    real and integer arrays storing
    !                  matrix information such as the matrix
    !                  of partial derivatives,permutation
    !                  vector,and various other information.
    !
    !     the other parameters are information
    !     which is needed internally by ddastp to
    !     continue from step to step.
    !
    !-----------------------------------------------------------------------
    !
    !
    !
    implicit none
    logical convgd
    integer :: iwm(*), ipar(*), maxit, idid, ncf, nsf, nef, jstart,  kold, knew, jcalc, iphase, ns, kp1, kp2, km1, nsp1, i, j, k, m, ntemp, ires, ier, nonneg, kdiff, j1, neq
    real(8) :: y(*), yprime(*), wt(*), phi(neq, *), delta(*), e(*), wm(*), psi(*), alpha(*), beta(*), gamma(*), sigma(*), rpar(*), xrate, xold, x, hold, h, cjold, cj, s, delnrm, &
        temp1, temp2, alphas, alpha0, cjlast, ck, pnorm, ddanrm, uround, oldnrm, rate, enorm, erk, terk, est, terkm1, erkm1, erkm2, terkm2, err, erkp1, terkp1, &
        hmin, hnew, r
    external res,jac
    !
    integer, parameter :: lmxord=3, lnst=11, lnre=12, lnje=13, letf=14, lctf=15
    !
    !
    !*** added by gpf 11/21/91 in case the "no remember"
    !
    save
    !
    data maxit/4/
    data xrate/0.25d0/
    !
    !
    !
    !
    !
    !-----------------------------------------------------------------------
    !     block 1.
    !     initialize. on the first call,set
    !     the order to 1 and initialize
    !     other variables.
    !-----------------------------------------------------------------------
    !
    !     initializations for all calls
    idid=1
    xold=x
    ncf=0
    nsf=0
    nef=0
    if(jstart /= 0) go to 120
    !
    !     if this is the first step,perform
    !     other initializations
    iwm(letf) = 0
    iwm(lctf) = 0
    k=1
    kold=0
    hold=0.0d0
    jstart=1
    psi(1)=h
    cjold = 1.0d0/h
    cj = cjold
    s = 100.d0
    jcalc = -1
    delnrm=1.0d0
    iphase = 0
    ns=0
120 continue
    !
    !
    !
    !
    !
    !-----------------------------------------------------------------------
    !     block 2
    !     compute coefficients of formulas for
    !     this step.
    !-----------------------------------------------------------------------
200 continue
    kp1=k+1
    kp2=k+2
    km1=k-1
    xold=x
    if(h/=hold.or.k /= kold) ns = 0
    ns=min0(ns+1,kold+2)
    nsp1=ns+1
    if(kp1 < ns)go to 230
    !
    beta(1)=1.0d0
    alpha(1)=1.0d0
    temp1=h
    gamma(1)=0.0d0
    sigma(1)=1.0d0
    do i=2,kp1
        temp2=psi(i-1)
        psi(i-1)=temp1
        beta(i)=beta(i-1)*psi(i-1)/temp2
        temp1=temp2+h
        alpha(i)=h/temp1
        sigma(i)=float(i-1)*sigma(i-1)*alpha(i)
        gamma(i)=gamma(i-1)+alpha(i-1)/h
    end do
    psi(kp1)=temp1
230 continue
    !
    !     compute alphas, alpha0
    alphas = 0.0d0
    alpha0 = 0.0d0
    do i = 1,k
        alphas = alphas - 1.0d0/float(i)
        alpha0 = alpha0 - alpha(i)
    end do
    !
    !     compute leading coefficient cj
    cjlast = cj
    cj = -alphas/h
    !
    !     compute variable stepsize error coefficient ck
    ck = abs(alpha(kp1) + alphas - alpha0)
    ck = dmax1(ck,alpha(kp1))
    !
    !     decide whether new jacobian is needed
    temp1 = (1.0d0 - xrate)/(1.0d0 + xrate)
    temp2 = 1.0d0/temp1
    if (cj/cjold < temp1 .or. cj/cjold > temp2) jcalc = -1
    if (cj /= cjlast) s = 100.d0
    !
    !     change phi to phi star
    if(kp1 < nsp1) go to 280
    do j=nsp1,kp1
        do i=1,neq
            phi(i,j)=beta(j)*phi(i,j)
        end do
    end do
280 continue
    !
    !     update time
    x=x+h
    !
    !
    !
    !
    !
    !-----------------------------------------------------------------------
    !     block 3
    !     predict the solution and derivative,
    !     and solve the corrector equation
    !-----------------------------------------------------------------------
    !
    !     first,predict the solution and derivative
300 continue
    do i=1,neq
        y(i)=phi(i,1)
        yprime(i)=0.0d0
    end do
    do j=2,kp1
        do i=1,neq
            y(i)=y(i)+phi(i,j)
         yprime(i)=yprime(i)+gamma(j)*phi(i,j)
        end do
    end do
    pnorm = ddanrm(neq,y,wt,ipar)
    !
    !
    !
    !     solve the corrector equation using a
    !     modified newton scheme.
    convgd= .true.
    m=0
    iwm(lnre)=iwm(lnre)+1
    ires = 0
    !
    !*** we're about to compute a jacobian so set the iderv
    !    flag, telling res to compute jacobian elements
    !
    call setderv(0)
    !
    call res(x,y,yprime,delta,ires,rpar,ipar)
    call setderv(0)
    if (ires < 0) go to 380
    !
    !
    !     if indicated,reevaluate the
    !     iteration matrix pd = dg/dy + cj*dg/dyprime
    !     (where g(x,y,yprime)=0). set
    !     jcalc to 0 as an indicator that
    !     this has been done.
    if(jcalc /= -1)go to 340
    iwm(lnje)=iwm(lnje)+1
    jcalc=0
    call ddajac(neq,x,y,yprime,delta,cj,h,ier,wt,e,wm,iwm,res,ires,uround,jac,rpar,ipar,ntemp)
    cjold=cj
    s = 100.d0
    if (ires < 0) go to 380
    if(ier /= 0)go to 380
    nsf=0
    !
    !
    !     initialize the error accumulation vector e.
340 continue
    do i=1,neq
        e(i)=0.0d0
    end do
    !
    !
    !     corrector loop.
350 continue
    !
    !     multiply residual by temp1 to accelerate convergence
    temp1 = 2.0d0/(1.0d0 + cj/cjold)
    do i = 1,neq
        delta(i) = delta(i) * temp1
    end do
    !
    !     compute a new iterate (back-substitution).
    !     store the correction in delta.
    call ddaslv(neq,delta,wm,iwm)
    !
    !     update y,e,and yprime
    do i=1,neq
        y(i)=y(i)-delta(i)
        e(i)=e(i)-delta(i)
        yprime(i)=yprime(i)-cj*delta(i)
    end do
    !
    !     test for convergence of the iteration
    delnrm=ddanrm(neq,delta,wt,ipar)
    if (delnrm <= 100.d0*uround*pnorm) go to 375
    if (m > 0) go to 365
    oldnrm = delnrm
    go to 367
365 rate = (delnrm/oldnrm)**(1.0d0/float(m))
    if (rate > 0.90d0) go to 370
    s = rate/(1.0d0 - rate)
367 if (s*delnrm <= 0.33d0) go to 375
    !
    !     the corrector has not yet converged.
    !     update m and test whether the
    !     maximum number of iterations have
    !     been tried.
    m=m+1
    if(m>=maxit)go to 370
    !
    !     evaluate the residual
    !     and go back to do another iteration
    iwm(lnre)=iwm(lnre)+1
    ires = 0
    call res(x,y,yprime,delta,ires,rpar,ipar)
    if (ires < 0) go to 380
    go to 350
    !
    !
    !     the corrector failed to converge in maxit
    !     iterations. if the iteration matrix
    !     is not current,re-do the step with
    !     a new iteration matrix.
370 continue
    if(jcalc==0)go to 380
    jcalc=-1
    go to 300
    !
    !
    !     the iteration has converged.  if nonnegativity of solution is
    !     required, set the solution nonnegative, if the perturbation
    !     to do it is small enough.  if the change is too large, then
    !     consider the corrector iteration to have failed.
375 if(nonneg == 0) go to 390
    do i = 1,neq
        delta(i) = dmin1(y(i),0.0d0)
    end do
    delnrm = ddanrm(neq,delta,wt,ipar)
    if(delnrm > 0.33d0) go to 380
    do i = 1,neq
        e(i) = e(i) - delta(i)
    end do
    go to 390
    !
    !
    !     exits from block 3
    !     no convergence with current iteration
    !     matrix,or singular iteration matrix
380 convgd= .false.
390 jcalc = 1
    if(.not.convgd)go to 600
    !
    !
    !
    !
    !
    !-----------------------------------------------------------------------
    !     block 4
    !     estimate the errors at orders k,k-1,k-2
    !     as if constant stepsize was used. estimate
    !     the local error at order k and test
    !     whether the current step is successful.
    !-----------------------------------------------------------------------
    !
    !     estimate errors at orders k,k-1,k-2
    enorm = ddanrm(neq,e,wt,ipar)
    erk = sigma(k+1)*enorm
    terk = float(k+1)*erk
    est = erk
    knew=k
    if(k == 1)go to 430
    do i = 1,neq
        delta(i) = phi(i,kp1) + e(i)
    end do
    erkm1=sigma(k)*ddanrm(neq,delta,wt,ipar)
    terkm1 = float(k)*erkm1
    if(k > 2)go to 410
    if(terkm1 <= 0.5d0*terk)go to 420
    go to 430
410 continue
    do i = 1,neq
        delta(i) = phi(i,k) + delta(i)
    end do
    erkm2=sigma(k-1)*ddanrm(neq,delta,wt,ipar)
    terkm2 = float(k-1)*erkm2
    if(dmax1(terkm1,terkm2)>terk)go to 430
    !     lower the order
420 continue
    knew=k-1
    est = erkm1
    !
    !
    !     calculate the local error for the current step
    !     to see if the step was successful
430 continue
    err = ck * enorm
    if(err > 1.0d0)go to 600
    !
    !
    !
    !
    !
    !-----------------------------------------------------------------------
    !     block 5
    !     the step is successful. determine
    !     the best order and stepsize for
    !     the next step. update the differences
    !     for the next step.
    !-----------------------------------------------------------------------
    idid=1
    iwm(lnst)=iwm(lnst)+1
    kdiff=k-kold
    kold=k
    hold=h
    !
    !
    !     estimate the error at order k+1 unless:
    !        already decided to lower order, or
    !        already using maximum order, or
    !        stepsize not constant, or
    !        order raised in previous step
    if(knew==km1.or.k==iwm(lmxord))iphase=1
    if(iphase == 0)go to 545
    if(knew==km1)go to 540
    if(k==iwm(lmxord)) go to 550
    if(kp1>=ns.or.kdiff==1)go to 550
    do i=1,neq
        delta(i)=e(i)-phi(i,kp2)
    end do
    erkp1 = (1.0d0/float(k+2))*ddanrm(neq,delta,wt,ipar)
    terkp1 = float(k+2)*erkp1
    if(k>1)go to 520
    if(terkp1>=0.5d0*terk)go to 550
    go to 530
520 if(terkm1<=dmin1(terk,terkp1))go to 540
    if(terkp1>=terk.or.k==iwm(lmxord))go to 550
    !
    !     raise order
530 k=kp1
    est = erkp1
    go to 550
    !
    !     lower order
540 k=km1
    est = erkm1
    go to 550
    !
    !     if iphase = 0, increase order by one and multiply stepsize by
    !     factor two
545 k = kp1
    hnew = h*2.0d0
    h = hnew
    go to 575
    !
    !
    !     determine the appropriate stepsize for
    !     the next step.
550 hnew=h
    temp2=k+1
    r=(2.0d0*est+0.0001d0)**(-1.0d0/temp2)
    if(r < 2.0d0) go to 555
    hnew = 2.0d0*h
    go to 560
555 if(r > 1.0d0) go to 560
    r = dmax1(0.5d0,dmin1(0.9d0,r))
    hnew = h*r
560 h=hnew
    !
    !
    !     update differences for next step
575 continue
    if(kold==iwm(lmxord))go to 585
    do i=1,neq
        phi(i,kp2)=e(i)
    end do
585 continue
    do i=1,neq
        phi(i,kp1)=phi(i,kp1)+e(i)
    end do
    do j1=2,kp1
        j=kp1-j1+1
        do i=1,neq
            phi(i,j)=phi(i,j)+phi(i,j+1)
        end do
    end do
    return
    !
    !
    !
    !
    !
    !-----------------------------------------------------------------------
    !     block 6
    !     the step is unsuccessful. restore x,psi,phi
    !     determine appropriate stepsize for
    !     continuing the integration, or exit with
    !     an error flag if there have been many
    !     failures.
    !-----------------------------------------------------------------------
600 iphase = 1
    !
    !     restore x,phi,psi
    x=xold
    if(kp1<nsp1)go to 630
    do j=nsp1,kp1
        temp1=1.0d0/beta(j)
        do i=1,neq
            phi(i,j)=temp1*phi(i,j)
        end do
    end do
630 continue
    do i=2,kp1
        psi(i-1)=psi(i)-h
    end do
    !
    !
    !     test whether failure is due to corrector iteration
    !     or error test
    if(convgd)go to 660
    iwm(lctf)=iwm(lctf)+1
    !
    !
    !     the newton iteration failed to converge with
    !     a current iteration matrix.  determine the cause
    !     of the failure and take appropriate action.
    if(ier==0)go to 650
    !
    !     the iteration matrix is singular. reduce
    !     the stepsize by a factor of 4. if
    !     this happens three times in a row on
    !     the same step, return with an error flag
    nsf=nsf+1
    r = 0.25d0
    h=h*r
    if (nsf < 3 .and. abs(h) >= hmin) go to 690
    idid=-8
    go to 675
    !
    !
    !     the newton iteration failed to converge for a reason
    !     other than a singular iteration matrix.  if ires = -2, then
    !     return.  otherwise, reduce the stepsize and try again, unless
    !     too many failures have occured.
650 continue
    if (ires > -2) go to 655
    idid = -11
    go to 675
655 ncf = ncf + 1
    r = 0.25d0
    h = h*r
    if (ncf < 10 .and. abs(h) >= hmin) go to 690
    idid = -7
    if (ires < 0) idid = -10
    if (nef >= 3) idid = -9
    go to 675
    !
    !
    !     the newton scheme converged,and the cause
    !     of the failure was the error estimate
    !     exceeding the tolerance.
660 nef=nef+1
    iwm(letf)=iwm(letf)+1
    if (nef > 1) go to 665
    !
    !     on first error test failure, keep current order or lower
    !     order by one.  compute new stepsize based on differences
    !     of the solution.
    k = knew
    temp2 = k + 1
    r = 0.90d0*(2.0d0*est+0.0001d0)**(-1.0d0/temp2)
    r = dmax1(0.25d0,dmin1(0.9d0,r))
    h = h*r
    if (abs(h) >= hmin) go to 690
    idid = -6
    go to 675
    !
    !     on second error test failure, use the current order or
    !     decrease order by one.  reduce the stepsize by a factor of
    !     one quarter.
665 if (nef > 2) go to 670
    k = knew
    h = 0.25d0*h
    if (abs(h) >= hmin) go to 690
    idid = -6
    go to 675
    !
    !     on third and subsequent error test failures, set the order to
    !     one and reduce the stepsize by a factor of one quarter
670 k = 1
    h = 0.25d0*h
    if (abs(h) >= hmin) go to 690
    idid = -6
    go to 675
    !
    !
    !
    !
    !     for all crashes, restore y to its last value,
    !     interpolate to find yprime at last x, and return
675 continue
    call ddatrp(x,x,y,yprime,neq,k,phi,psi)
    return
    !
    !
    !     go back and try this step again
690 go to 200
    !
    !------end of subroutine ddastp------
    end subroutine ddastp

! --------------------------- ddajac -------------------------------------------

    subroutine ddajac(neq,x,y,yprime,delta,cj,h,ier,wt,e,wm,iwm,res,ires,uround,jac,rpar,ipar,ntemp)
    !
    !***begin prologue  ddajac
    !***refer to  ddassl
    !***routines called  dgefa,dgbfa
    !***date written   830315   (yymmdd)
    !***revision date  891228   (yymmdd)
    !***end prologue  ddajac
    !-----------------------------------------------------------------------
    !     this routine computes the iteration matrix
    !     pd=dg/dy+cj*dg/dyprime (where g(x,y,yprime)=0).
    !     here pd is computed by the user-supplied
    !     routine jac if iwm(mtype) is 1 or 4, and
    !     it is computed by numerical finite differencing
    !     if iwm(mtype)is 2 or 5
    !     the parameters have the following meanings.
    !     y        = array containing predicted values
    !     yprime   = array containing predicted derivatives
    !     delta    = residual evaluated at (x,y,yprime)
    !                (used only if iwm(mtype)=2 or 5)
    !     cj       = scalar parameter defining iteration matrix
    !     h        = current stepsize in integration
    !     ier      = variable which is /= 0
    !                if iteration matrix is singular,
    !                and 0 otherwise.
    !     wt       = vector of weights for computing norms
    !     e        = work space (temporary) of length neq
    !     wm       = real work space for matrices. on
    !                output it contains the lu decomposition
    !                of the iteration matrix.
    !     iwm      = integer work space containing
    !                matrix information
    !     res      = name of the external user-supplied routine
    !                to evaluate the residual function g(x,y,yprime)
    !     ires     = flag which is equal to zero if no illegal values
    !                in res, and less than zero otherwise.  (if ires
    !                is less than zero, the matrix was not completed)
    !                in this case (if ires < 0), then ier = 0.
    !     uround   = the unit roundoff error of the machine being used.
    !     jac      = name of the external user-supplied routine
    !                to evaluate the iteration matrix (this routine
    !                is only used if iwm(mtype) is 1 or 4)
    !-----------------------------------------------------------------------
    !
    implicit none
    external res,jac
    real(8) :: y(*), yprime(*), delta(*), wt(*), e(*), wm(*), rpar(*), x, cj, squr, uround, del, h, ysave, ypsave, delinv
    integer :: iwm(*), ipar(*),  ier, npdm1, mtype, lenpd, neq, i, ires, nrow, l, meband, mband, mba, meb1, msave, isave, ntemp, ipsave, j, n, k, i1, i2, ii
    !
    integer, parameter :: npd=1, lml=1, lmu=2, lmtype=4, lipvt=21
    !
    !*** added by gpf 11/21/91 in case the "no remember"
    !
    save
    !
    !
    ier = 0
    npdm1=npd-1
    mtype=iwm(lmtype)
    !
    !     modified by par 01/08/93 to allow custom solution for cfast
    !
    go to (100,200,300,400,500,600),mtype
    !
    !
    !     dense user-supplied matrix
100 lenpd=neq*neq
    do i=1,lenpd
        wm(npdm1+i)=0.0d0
    end do
    call jac(x,y,yprime,wm(npd),cj,rpar,ipar)
    go to 230
    !
    !
    !     dense finite-difference-generated matrix
200 ires=0
    nrow=npdm1
    squr = sqrt(uround)
    do i=1,neq
        del=squr*dmax1(abs(y(i)),abs(h*yprime(i)),abs(wt(i)))
        del=dsign(del,h*yprime(i))
        del=(y(i)+del)-y(i)
        ysave=y(i)
        ypsave=yprime(i)
        y(i)=y(i)+del
        yprime(i)=yprime(i)+cj*del
        call setderv(i)
        call res(x,y,yprime,e,ires,rpar,ipar)
        if (ires < 0) return
        delinv=1.0d0/del
        do l=1,neq
            wm(nrow+l)=(e(l)-delta(l))*delinv
        end do
        nrow=nrow+neq
        y(i)=ysave
        yprime(i)=ypsave
    end do
    call setderv(-1)
    !
    !*** code added by gpf and par to increment jacobian count
    !    and print out jacobian
    !
    call incjac
    call outjac(x,wm(npd),neq)
    !
    !
    !
    !     do dense-matrix lu decomposition on pd
230 call dgefa(wm(npd),neq,neq,iwm(lipvt),ier)
    return
    !
    !
    !     dummy section for iwm(mtype)=3
300 return
    !
    !
    !     banded user-supplied matrix
400 lenpd=(2*iwm(lml)+iwm(lmu)+1)*neq
    do i=1,lenpd
        wm(npdm1+i)=0.0d0
    end do
    call jac(x,y,yprime,wm(npd),cj,rpar,ipar)
    meband=2*iwm(lml)+iwm(lmu)+1
    go to 550
    !
    !
    !     banded finite-difference-generated matrix
500 mband=iwm(lml)+iwm(lmu)+1
    mba=min0(mband,neq)
    meband=mband+iwm(lml)
    meb1=meband-1
    msave=(neq/mband)+1
    isave=ntemp-1
    ipsave=isave+msave
    ires=0
    squr=sqrt(uround)
    do j=1,mba
        do n=j,neq,mband
            k= (n-j)/mband + 1
            wm(isave+k)=y(n)
            wm(ipsave+k)=yprime(n)
            del=squr*dmax1(abs(y(n)),abs(h*yprime(n)),abs(wt(n)))
            del=dsign(del,h*yprime(n))
            del=(y(n)+del)-y(n)
            y(n)=y(n)+del
            yprime(n)=yprime(n)+cj*del
        end do
        call res(x,y,yprime,e,ires,rpar,ipar)
        if (ires < 0) return
        do n=j,neq,mband
            k= (n-j)/mband + 1
            y(n)=wm(isave+k)
            yprime(n)=wm(ipsave+k)
            del=squr*dmax1(abs(y(n)),abs(h*yprime(n)),abs(wt(n)))
            del=dsign(del,h*yprime(n))
            del=(y(n)+del)-y(n)
            delinv=1.0d0/del
            i1=max0(1,(n-iwm(lmu)))
            i2=min0(neq,(n+iwm(lml)))
            ii=n*meb1-iwm(lml)+npdm1
            do i=i1,i2
                wm(ii+i)=(e(i)-delta(i))*delinv
            end do
        end do
    end do
    !
    !
    !     do lu decomposition of banded pd
550 call dgbfa(wm(npd),meband,neq,iwm(lml),iwm(lmu),iwm(lipvt),ier)
    return
    !
    !
    !     custom generation of jacobian for cfast
    !     added by par 01/08/93
    !     removed by gpf 2/2/96 (obsolete)
600 continue
    return
    !------end of subroutine ddajac------
    end subroutine ddajac

! --------------------------- ddaslv -------------------------------------------

    subroutine ddaslv(neq,delta,wm,iwm)
    !
    !***begin prologue  ddaslv
    !***refer to  ddassl
    !***routines called dgesl,dgbsl
    !***date written   830315   (yymmdd)
    !***revision date  891228   (yymmdd)
    !***end prologue  ddaslv
    !-----------------------------------------------------------------------
    !     this routine manages the solution of the linear
    !     system arising in the newton iteration.
    !     matrices and real temporary storage and
    !     real information are stored in the array wm.
    !     integer matrix information is stored in
    !     the array iwm.
    !     for a dense matrix, the linpack routine
    !     dgesl is called.
    !     for a banded matrix,the linpack routine
    !     dgbsl is called.
    !-----------------------------------------------------------------------
    !
    implicit none
    real(8) :: delta(*),wm(*)
    integer :: iwm(*), mtype, neq, meband
    !
    integer, parameter :: npd=1, lml=1, lmu=2, lmtype=4, lipvt=21
    !
    !
    !*** added by gpf 11/21/91 in case the "no remember"
    !
    save
    !
    mtype=iwm(lmtype)
    !
    !     modified by par 01/08/93 for custom solution for cfast
    !
    go to(100,100,300,400,400,600),mtype
    !
    !     dense matrix
100 call dgesl(wm(npd),neq,neq,iwm(lipvt),delta,0)
    return
    !
    !     dummy section for mtype=3
300 continue
    return
    !
    !     banded matrix
400 meband=2*iwm(lml)+iwm(lmu)+1
    call dgbsl(wm(npd),meband,neq,iwm(lml),iwm(lmu),iwm(lipvt),delta,0)
    return
    !
    !     added by par 01/08/93 for custom solution of cfast
600 continue
    return
    !------end of subroutine ddaslv------
    end subroutine ddaslv

! --------------------------- snsqe -------------------------------------------

    subroutine snsqe(fcn,jac,iopt,n,x,fvec,tol,nprint,info,wa,lwa)
    !***begin prologue  snsqe
    !***date written   800301   (yymmdd)
    !***revision date  840405   (yymmdd)
    !***category no.  f2a
    !***keywords  easy-to-use,nonlinear square system,powell hybrid method,
    !             zero
    !***author  hiebert, k. l., (snla)
    !***purpose  snsqe is the easy-to-use version of snsq which finds a zero
    !            of a system of n nonlinear functions in n variables by a
    !            modification of powell hybrid method.  this code is the
    !            combination of the minpack codes(argonne) hybrd1 and hybrj1
    !***description
    !
    ! 1. purpose.
    !
    !
    !       the purpose of snsqe is to find a zero of a system of n non-
    !       linear functions in n variables by a modification of the powell
    !       hybrid method.  this is done by using the more general nonlinear
    !       equation solver snsq.  the user must provide a subroutine which
    !       calculates the functions.  the user has the option of either to
    !       provide a subroutine which calculates the jacobian or to let the
    !       code calculate it by a forward-difference approximation.  this
    !       code is the combination of the minpack codes (argonne) hybrd1
    !       and hybrj1.
    !
    !
    ! 2. subroutine and type statements.
    !
    !       subroutine snsqe(fcn,jac,iopt,n,x,fvec,tol,nprint,info,
    !      *                  wa,lwa)
    !       integer iopt,n,nprint,info,lwa
    !       real tol
    !       real x(n),fvec(n),wa(lwa)
    !       external fcn,jac
    !
    !
    ! 3. parameters.
    !
    !       parameters designated as input parameters must be specified on
    !       entry to snsqe and are not changed on exit, while parameters
    !       designated as output parameters need not be specified on entry
    !       and are set to appropriate values on exit from snsqe.
    !
    !       fcn is the name of the user-supplied subroutine which calculates
    !         the functions.  fcn must be declared in an external statement
    !         in the user calling program, and should be written as follows.
    !
    !         subroutine fcn(n,x,fvec,iflag)
    !         integer n,iflag
    !         real x(n),fvec(n)
    !         ----------
    !         calculate the functions at x and
    !         return this vector in fvec.
    !         ----------
    !         return
    !         end
    !
    !         the value of iflag should not be changed by fcn unless the
    !         user wants to terminate execution of snsqe.  in this case, set
    !         iflag to a negative integer.
    !
    !       jac is the name of the user-supplied subroutine which calculates
    !         the jacobian.  if iopt=1, then jac must be declared in an
    !         external statement in the user calling program, and should be
    !         written as follows.
    !
    !         subroutine jac(n,x,fvec,fjac,ldfjac,iflag)
    !         integer n,ldfjac,iflag
    !         real x(n),fvec(n),fjac(ldfjac,n)
    !         ----------
    !         calculate the jacobian at x and return this
    !         matrix in fjac.  fvec contains the function
    !         values at x and should not be altered.
    !         ----------
    !         return
    !         end
    !
    !         the value of iflag should not be changed by jac unless the
    !         user wants to terminate execution of snsqe.  in this case, set
    !         iflag to a negative integer.
    !
    !         if iopt=2, jac can be ignored (treat it as a dummy argument).
    !
    !       iopt is an input variable which specifies how the jacobian will
    !         be calculated.  if iopt=1, then the user must supply the
    !         jacobian through the subroutine jac.  if iopt=2, then the
    !         code will approximate the jacobian by forward-differencing.
    !
    !       n is a positive integer input variable set to the number of
    !         functions and variables.
    !
    !       x is an array of length n.  on input, x must contain an initial
    !         estimate of the solution vector.  on output, x contains the
    !         final estimate of the solution vector.
    !
    !       fvec is an output array of length n which contains the functions
    !         evaluated at the output x.
    !
    !       tol is a non-negative input variable.  termination occurs when
    !         the algorithm estimates that the relative error between x and
    !         the solution is at most tol.  section 4 contains more details
    !         about tol.
    !
    !       nprint is an integer input variable that enables controlled
    !         printing of iterates if it is positive.  in this case, fcn is
    !         called with iflag = 0 at the beginning of the first iteration
    !         and every nprint iteration thereafter and immediately prior
    !         to return, with x and fvec available for printing. appropriate
    !         print statements must be added to fcn (see example). if nprint
    !         is not positive, no special calls of fcn with iflag = 0 are
    !         made.
    !
    !       info is an integer output variable.  if the user has terminated
    !         execution, info is set to the (negative) value of iflag.  see
    !         description of fcn and jac. otherwise, info is set as follows.
    !
    !         info = 0  improper input parameters.
    !
    !         info = 1  algorithm estimates that the relative error between
    !                   x and the solution is at most tol.
    !
    !         info = 2  number of calls to fcn has reached or exceeded
    !                   100*(n+1) for iopt=1 or 200*(n+1) for iopt=2.
    !
    !         info = 3  tol is too small.  no further improvement in the
    !                   approximate solution x is possible.
    !
    !         info = 4  iteration is not making good progress.
    !
    !         sections 4 and 5 contain more details about info.
    !
    !       wa is a work array of length lwa.
    !
    !       lwa is a positive integer input variable not less than
    !         (3*n**2+13*n))/2.
    !
    !
    ! 4. successful completion.
    !
    !       the accuracy of snsqe is controlled by the convergence parame-
    !       ter tol.  this parameter is used in a test which makes a compar-
    !       ison between the approximation x and a solution xsol.  snsqe
    !       terminates when the test is satisfied.  if tol is less than the
    !       machine precision (as defined by the function r1mach(4)), then
    !       snsqe attemps only to satisfy the test defined by the machine
    !       precision.  further progress is not usually possible.  unless
    !       high precision solutions are required, the recommended value
    !       for tol is the square root of the machine precision.
    !
    !       the test assumes that the functions are reasonably well behaved,
    !       and, if the jacobian is supplied by the user, that the functions
    !       and the jacobian  coded consistently.  if these conditions
    !       are not satisfied, snsqe may incorrectly indicate convergence.
    !       the coding of the jacobian can be checked by the subroutine
    !       chkder.  if the jacobian is coded correctly or iopt=2, then
    !       the validity of the answer can be checked, for example, by
    !       rerunning snsqe with a tighter tolerance.
    !
    !       convergence test.  if enorm(z) denotes the euclidean norm of a
    !         vector z, then this test attempts to guarantee that
    !
    !               enorm(x-xsol) <=  tol*enorm(xsol).
    !
    !         if this condition is satisfied with tol = 10**(-k), then the
    !         larger components of x have k significant decimal digits and
    !         info is set to 1.  there is a danger that the smaller compo-
    !         nents of x may have large relative errors, but the fast rate
    !         of convergence of snsqe usually avoids this possibility.
    !
    !
    ! 5. unsuccessful completion.
    !
    !       unsuccessful termination of snsqe can be due to improper input
    !       parameters, arithmetic interrupts, an excessive number of func-
    !       tion evaluations, errors in the functions, or lack of good prog-
    !       ress.
    !
    !       improper input parameters.  info is set to 0 if iopt < 1, or
    !         iopt > 2, or n <= 0, or tol < 0.e0, or
    !         lwa < (3*n**2+13*n)/2.
    !
    !       arithmetic interrupts.  if these interrupts occur in the fcn
    !         subroutine during an early stage of the computation, they may
    !         be caused by an unacceptable choice of x by snsqe.  in this
    !         case, it may be possible to remedy the situation by not evalu-
    !         ating the functions here, but instead setting the components
    !         of fvec to numbers that exceed those in the initial fvec.
    !
    !       excessive number of function evaluations.  if the number of
    !         calls to fcn reaches 100*(n+1) for iopt=1 or 200*(n+1) for
    !         iopt=2, then this indicates that the routine is converging
    !         very slowly as measured by the progress of fvec, and info is
    !         set to 2.  this situation should be unusual because, as
    !         indicated below, lack of good progress is usually diagnosed
    !         earlier by snsqe, causing termination with info = 4.
    !
    !       errors in the functions.  when iopt=2, the choice of step length
    !         in the forward-difference approximation to the jacobian
    !         assumes that the relative errors in the functions are of the
    !         order of the machine precision.  if this is not the case,
    !         snsqe may fail (usually with info = 4).  the user should
    !         then either use snsq and set the step length or use iopt=1
    !         and supply the jacobian.
    !
    !       lack of good progress.  snsqe searches for a zero of the system
    !         by minimizing the sum of the squares of the functions.  in so
    !         doing, it can become trapped in a region where the minimum
    !         does not correspond to a zero of the system and, in this situ-
    !         ation, the iteration eventually fails to make good progress.
    !         in particular, this will happen if the system does not have a
    !         zero.  if the system has a zero, rerunning snsqe from a dif-
    !         ferent starting point may be helpful.
    !
    !
    ! 6. characteristics of the algorithm.
    !
    !       snsqe is a modification of the powell hybrid method.  two of
    !       its main characteristics involve the choice of the correction as
    !       a convex combination of the newton and scaled gradient direc-
    !       tions, and the updating of the jacobian by the rank-1 method of
    !       broyden.  the choice of the correction guarantees (under reason-
    !       able conditions) global convergence for starting points far from
    !       the solution and a fast rate of convergence.  the jacobian is
    !       calculated at the starting point by either the user-supplied
    !       subroutine or a forward-difference approximation, but it is not
    !       recalculated until the rank-1 method fails to produce satis-
    !       factory progress.
    !
    !       timing.  the time required by snsqe to solve a given problem
    !         depends on n, the behavior of the functions, the accuracy
    !         requested, and the starting point.  the number of arithmetic
    !         operations needed by snsqe is about 11.5*(n**2) to process
    !         each evaluation of the functions (call to fcn) and 1.3*(n**3)
    !         to process each evaluation of the jacobian (call to jac,
    !         if iopt = 1).  unless fcn and jac can be evaluated quickly,
    !         the timing of snsqe will be strongly influenced by the time
    !         spent in fcn and jac.
    !
    !       storage.  snsqe requires (3*n**2 + 17*n)/2 single precision
    !         storage locations, in addition to the storage required by the
    !         program.  there are no internally declared storage arrays.
    !
    !
    ! 7. example.
    !
    !       the problem is to determine the values of x(1), x(2), ..., x(9),
    !       which solve the system of tridiagonal equations
    !
    !       (3-2*x(1))*x(1)           -2*x(2)                   = -1
    !               -x(i-1) + (3-2*x(i))*x(i)         -2*x(i+1) = -1, i=2-8
    !                                   -x(8) + (3-2*x(9))*x(9) = -1
    !
    !       **********
    !
    !       program test(input,output,tape6=output)
    ! c
    ! c     driver for snsqe example.
    ! c
    !       integer j,n,iopt,nprint,info,lwa,nwrite
    !       real tol,fnorm
    !       real x(9),fvec(9),wa(180)
    !       real enorm,r1mach
    !       external fcn
    !       data nwrite /6/
    ! c
    !       iopt = 2
    !       n = 9
    ! c
    ! c     the following starting values provide a rough solution.
    ! c
    !       do 10 j = 1, 9
    !          x(j) = -1.e0
    !    10    continue
    !
    !       lwa = 180
    !       nprint = 0
    ! c
    ! c     set tol to the square root of the machine precision.
    ! c     unless high precision solutions are required,
    ! c     this is the recommended setting.
    ! c
    !       tol = sqrt(r1mach(4))
    ! c
    !       call snsqe(fcn,jac,iopt,n,x,fvec,tol,nprint,info,wa,lwa)
    !       fnorm = enorm(n,fvec)
    !       write (nwrite,1000) fnorm,info,(x(j),j=1,n)
    !       stop
    !  1000 format (5x,' final l2 norm of the residuals',e15.7 //
    !      *        5x,' exit parameter',16x,i10 //
    !      *        5x,' final approximate solution' // (5x,3e15.7))
    !       end
    !       subroutine fcn(n,x,fvec,iflag)
    !       integer n,iflag
    !       real x(n),fvec(n)
    !       integer k
    !       real one,temp,temp1,temp2,three,two,zero
    !       data zero,one,two,three /0.e0,1.e0,2.e0,3.e0/
    ! c
    !       do 10 k = 1, n
    !          temp = (three - two*x(k))*x(k)
    !          temp1 = zero
    !          if (k /= 1) temp1 = x(k-1)
    !          temp2 = zero
    !          if (k /= n) temp2 = x(k+1)
    !          fvec(k) = temp - temp1 - two*temp2 + one
    !    10    continue
    !       return
    !       end
    !
    !       results obtained with different compilers or machines
    !       may be slightly different.
    !
    !       final l2 norm of the residuals  0.1192636e-07
    !
    !       exit parameter                         1
    !
    !       final approximate solution
    !
    !       -0.5706545e+00 -0.6816283e+00 -0.7017325e+00
    !       -0.7042129e+00 -0.7013690e+00 -0.6918656e+00
    !       -0.6657920e+00 -0.5960342e+00 -0.4164121e+00
    !***references  powell, m. j. d.
    !                 a hybrid method for nonlinear equations.
    !                 numerical methods for nonlinear algebraic equations,
    !                 p. rabinowitz, editor.  gordon and breach, 1970.
    !***routines called  snsq,xerror
    !***end prologue  snsqe
    
    implicit none
    integer :: iopt,  n,  nprint,  info,  lwa, index, j, lr, maxfev, ml, mode, mu, nfev, njev
    real(8) :: x(n), fvec(n), wa(lwa), factor, one, zero, tol, xtol, epsfcn
    external fcn,jac
    
    data factor,one,zero /1.0d2,1.0d0,0.0d0/
    !***first executable statement  snsqe
    info = 0
    !
    !     check the input parameters for errors.
    !
    if (iopt < 1 .or. iopt > 2 .or. n <= 0 .or. tol < zero .or. lwa < (3*n**2 +13*n)/2) go to 20
    !
    !     call snsq.
    !
    maxfev = 100*(n + 1)
    if (iopt == 2) maxfev = 2*maxfev
    xtol = tol
    ml = n - 1
    mu = n - 1
    epsfcn = zero
    mode = 2
    do j = 1, n
        wa(j) = one
    end do
    lr = (n*(n + 1))/2
    index=6*n+lr
    call snsq(fcn,jac,iopt,n,x,fvec,wa(index+1),n,xtol,maxfev,ml,mu,epsfcn,wa(1),mode,factor,nprint,info,nfev,njev, &
    wa(6*n+1),lr,wa(n+1),wa(2*n+1),wa(3*n+1),wa(4*n+1),wa(5*n+1))
    if (info == 5) info = 4
20  continue
    if (info == 0) call xerror( 'snsqe  -- invalid input parameter.',34,2,1)
    return
    !
    !     last card of subroutine snsqe.
    !
    end subroutine snsqe

! --------------------------- dogleg -------------------------------------------

    subroutine dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2)
    !***begin prologue  dogleg
    !***refer to  snsq,snsqe
    !
    !     **********
    !     subroutine dogleg
    !
    !     given an m by n matrix a, an n by n nonsingular diagonal
    !     matrix d, an m-vector b, and a positive number delta, the
    !     problem is to determine the convex combination x of the
    !     gauss-newton and scaled gradient directions that minimizes
    !     (a*x - b) in the least squares sense, subject to the
    !     restriction that the euclidean norm of d*x be at most delta.
    !
    !     this subroutine completes the solution of the problem
    !     if it is provided with the necessary information from the
    !     qr factorization of a. that is, if a = q*r, where q has
    !     orthogonal columns and r is an upper triangular matrix,
    !     then dogleg expects the full upper triangle of r and
    !     the first n components of (q transpose)*b.
    !
    !     the subroutine statement is
    !
    !       subroutine dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2)
    !
    !     where
    !
    !       n is a positive integer input variable set to the order of r.
    !
    !       r is an input array of length lr which must contain the upper
    !         triangular matrix r stored by rows.
    !
    !       lr is a positive integer input variable not less than
    !         (n*(n+1))/2.
    !
    !       diag is an input array of length n which must contain the
    !         diagonal elements of the matrix d.
    !
    !       qtb is an input array of length n which must contain the first
    !         n elements of the vector (q transpose)*b.
    !
    !       delta is a positive input variable which specifies an upper
    !         bound on the euclidean norm of d*x.
    !
    !       x is an output array of length n which contains the desired
    !         convex combination of the gauss-newton direction and the
    !         scaled gradient direction.
    !
    !       wa1 and wa2 are work arrays of length n.
    !
    !     subprograms called
    !
    !       minpack-supplied ... r1mach,enorm
    !
    !       fortran-supplied ... abs,max,min,sqrt
    !
    !     minpack. version of july 1978.
    !     burton s. garbow, kenneth e. hillstrom, jorge j. more
    !     **********
    !***routines called  enorm,r1mach
    !***end prologue  dogleg
    
    implicit none
    integer :: n, lr, i, j, jj, jp1, k, l
    real(8) ::  r(lr), diag(n), qtb(n), x(n), wa1(n), wa2(n), one, zero, epsmch, d1mach, sum, temp, qnorm, enorm, delta, gnorm, sgnorm, alpha, bnorm

    data one,zero /1.0d0,0.0d0/
    
    !***first executable statement  dogleg
    epsmch = d1mach(4)
    !
    !     first, calculate the gauss-newton direction.
    !
    jj = (n*(n + 1))/2 + 1
    do k = 1, n
        j = n - k + 1
        jp1 = j + 1
        jj = jj - k
        l = jj + 1
        sum = zero
        if (n < jp1) go to 20
        do i = jp1, n
            sum = sum + r(l)*x(i)
            l = l + 1
        end do
20      continue
        temp = r(jj)
        if (temp /= zero) go to 40
        l = j
        do i = 1, j
            temp = max(temp,abs(r(l)))
            l = l + n - i
        end do
        temp = epsmch*temp
        if (temp == zero) temp = epsmch
40      continue
        x(j) = (qtb(j) - sum)/temp
    end do
    !
    !     test whether the gauss-newton direction is acceptable.
    !
    do j = 1, n
        wa1(j) = zero
        wa2(j) = diag(j)*x(j)
    end do
    qnorm = enorm(n,wa2)
    if (qnorm <= delta) go to 140
    !
    !     the gauss-newton direction is not acceptable.
    !     next, calculate the scaled gradient direction.
    !
    l = 1
    do j = 1, n
        temp = qtb(j)
        do i = j, n
            wa1(i) = wa1(i) + r(l)*temp
            l = l + 1
        end do
        wa1(j) = wa1(j)/diag(j)
    end do
    !
    !     calculate the norm of the scaled gradient direction,
    !     normalize, and rescale the gradient.
    !
    gnorm = enorm(n,wa1)
    sgnorm = zero
    alpha = delta/qnorm
    if (gnorm == zero) go to 120
    do j = 1, n
        wa1(j) = (wa1(j)/gnorm)/diag(j)
    end do
    !
    !     calculate the point along the scaled gradient
    !     at which the quadratic is minimized.
    !
    l = 1
    do j = 1, n
        sum = zero
        do i = j, n
            sum = sum + r(l)*wa1(i)
            l = l + 1
        end do
        wa2(j) = sum
    end do
    temp = enorm(n,wa2)
    sgnorm = (gnorm/temp)/temp
    !
    !     test whether the scaled gradient direction is acceptable.
    !
    alpha = zero
    if (sgnorm >= delta) go to 120
    !
    !     the scaled gradient direction is not acceptable.
    !     finally, calculate the point along the dogleg
    !     at which the quadratic is minimized.
    !
    bnorm = enorm(n,qtb)
    temp = (bnorm/gnorm)*(bnorm/qnorm)*(sgnorm/delta)
    temp = temp - (delta/qnorm)*(sgnorm/delta)**2 + sqrt((temp-(delta/qnorm))**2 + (one-(delta/qnorm)**2)*(one-(sgnorm/delta)**2))
    alpha = ((delta/qnorm)*(one - (sgnorm/delta)**2))/temp
120 continue
    !
    !     form appropriate convex combination of the gauss-newton
    !     direction and the scaled gradient direction.
    !
    temp = (one - alpha)*min(sgnorm,delta)
    do j = 1, n
        x(j) = temp*wa1(j) + alpha*x(j)
    end do
140 continue
    return
    !
    !     last card of subroutine dogleg.
    !
    end subroutine dogleg

! --------------------------- enorm -------------------------------------------

    real(8) function enorm(n,x)
    !***begin prologue  enorm
    !***refer to  snls1,snls1e,snsq,snsqe
    !
    !     **********
    !     function enorm
    !
    !     given an n-vector x, this function calculates the
    !     euclidean norm of x.
    !
    !     the euclidean norm is computed by accumulating the sum of
    !     squares in three different sums. the sums of squares for the
    !     small and large components are scaled so that no overflows
    !     occur. non-destructive underflows are permitted. underflows
    !     and overflows do not occur in the computation of the unscaled
    !     sum of squares for the intermediate components.
    !     the definitions of small, intermediate and large components
    !     depend on two constants, rdwarf and rgiant. the main
    !     restrictions on these constants are that rdwarf**2 not
    !     underflow and rgiant**2 not overflow. the constants
    !     given here are suitable for every known computer.
    !
    !     the function statement is
    !
    !       real function enorm(n,x)
    !
    !     where
    !
    !       n is a positive integer input variable.
    !
    !       x is an input array of length n.
    !
    !     subprograms called
    !
    !       fortran-supplied ... abs,sqrt
    !
    !     minpack. version of october 1979.
    !     burton s. garbow, kenneth e. hillstrom, jorge j. more
    !     **********
    !***routines called  (none)
    !***end prologue  enorm
    
    implicit none
    integer :: n, i
    real(8) ::  x(n), one, zero, rdwarf, rgiant, s1, s2, s3, x1max, x3max, floatn, agiant, xabs
    
    data one,zero,rdwarf,rgiant /1.0d0,0.0d0,3.834d-20,1.304d19/
    !***first executable statement  enorm
    s1 = zero
    s2 = zero
    s3 = zero
    x1max = zero
    x3max = zero
    floatn = n
    agiant = rgiant/floatn
    do i = 1, n
        xabs = abs(x(i))
        if (xabs > rdwarf .and. xabs < agiant) go to 70
        if (xabs <= rdwarf) go to 30
        !
        !              sum for large components.
        !
        if (xabs <= x1max) go to 10
        s1 = one + s1*(x1max/xabs)**2
        x1max = xabs
        go to 20
10      continue
        s1 = s1 + (xabs/x1max)**2
20      continue
        go to 60
30      continue
        !
        !              sum for small components.
        !
        if (xabs <= x3max) go to 40
        s3 = one + s3*(x3max/xabs)**2
        x3max = xabs
        go to 50
40      continue
        if (xabs /= zero) s3 = s3 + (xabs/x3max)**2
50      continue
60      continue
        go to 80
70      continue
        !
        !           sum for intermediate components.
        !
        s2 = s2 + xabs**2
80      continue
    end do
    !
    !     calculation of norm.
    !
    if (s1 == zero) go to 100
    enorm = x1max*sqrt(s1+(s2/x1max)/x1max)
    go to 130
100 continue
    if (s2 == zero) go to 110
    if (s2 >= x3max) enorm = sqrt(s2*(one+(x3max/s2)*(x3max*s3)))
    if (s2 < x3max) enorm = sqrt(x3max*((s2/x3max)+(x3max*s3)))
    go to 120
110 continue
    enorm = x3max*sqrt(s3)
120 continue
130 continue
    return
    !
    !     last card of function enorm.
    !
    end function enorm

! --------------------------- fdjac1 -------------------------------------------

    subroutine fdjac1(fcn,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,wa1,wa2)
    !***begin prologue  fdjac1
    !***refer to  snsq,snsqe
    !
    !              subroutine fdjac1
    !
    !     this subroutine computes a forward-difference approximation
    !     to the n by n jacobian matrix associated with a specified
    !     problem of n functions in n variables. if the jacobian has
    !     a banded form, then function evaluations are saved by only
    !     approximating the nonzero terms.
    !
    !     the subroutine statement is
    !
    !       subroutine fdjac1(fcn,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,
    !                         wa1,wa2)
    !
    !     where
    !
    !       fcn is the name of the user-supplied subroutine which
    !         calculates the functions. fcn must be declared
    !         in an external statement in the user calling
    !         program, and should be written as follows.
    !
    !         subroutine fcn(n,x,fvec,iflag)
    !         integer n,iflag
    !         real x(n),fvec(n)
    !         ----------
    !         calculate the functions at x and
    !         return this vector in fvec.
    !         ----------
    !         return
    !         end
    !
    !         the value of iflag should not be changed by fcn unless
    !         the user wants to terminate execution of fdjac1.
    !         in this case set iflag to a negative integer.
    !
    !       n is a positive integer input variable set to the number
    !         of functions and variables.
    !
    !       x is an input array of length n.
    !
    !       fvec is an input array of length n which must contain the
    !         functions evaluated at x.
    !
    !       fjac is an output n by n array which contains the
    !         approximation to the jacobian matrix evaluated at x.
    !
    !       ldfjac is a positive integer input variable not less than n
    !         which specifies the leading dimension of the array fjac.
    !
    !       iflag is an integer variable which can be used to terminate
    !         the execution of fdjac1. see description of fcn.
    !
    !       ml is a nonnegative integer input variable which specifies
    !         the number of subdiagonals within the band of the
    !         jacobian matrix. if the jacobian is not banded, set
    !         ml to at least n - 1.
    !
    !       epsfcn is an input variable used in determining a suitable
    !         step length for the forward-difference approximation. this
    !         approximation assumes that the relative errors in the
    !         functions are of the order of epsfcn. if epsfcn is less
    !         than the machine precision, it is assumed that the relative
    !         errors in the functions are of the order of the machine
    !         precision.
    !
    !       mu is a nonnegative integer input variable which specifies
    !         the number of superdiagonals within the band of the
    !         jacobian matrix. if the jacobian is not banded, set
    !         mu to at least n - 1.
    !
    !       wa1 and wa2 are work arrays of length n. if ml + mu + 1 is at
    !         least n, then the jacobian is considered dense, and wa2 is
    !         not referenced.
    !
    !     subprograms called
    !
    !       minpack-supplied ... r1mach
    !
    !       fortran-supplied ... abs,max,sqrt
    !
    !     minpack. version of june 1979.
    !     burton s. garbow, kenneth e. hillstrom, jorge j. more
    !     **********
    !***routines called  r1mach
    !***end prologue  fdjac1
    
    implicit none
    
    integer :: n, ldfjac, iflag, ml, mu, i, j, k, msum
    real(8) :: x(n), fvec(n), fjac(ldfjac, n), wa1(n), wa2(n), zero, epsmch, d1mach, eps, epsfcn, temp, h

    data zero /0.0d0/
    !***first executable statement  fdjac1
    epsmch = d1mach(4)
    !
    eps = sqrt(max(epsfcn,epsmch))
    msum = ml + mu + 1
    if (msum < n) go to 40
    !
    !        computation of dense approximate jacobian.
    !
    do j = 1, n
        temp = x(j)
        h = eps*abs(temp)
        if (h == zero) h = eps
        x(j) = temp + h
        call fcn(n,x,wa1,iflag)
        if (iflag < 0) go to 30
        x(j) = temp
        do i = 1, n
            fjac(i,j) = (wa1(i) - fvec(i))/h
        end do
    end do
30  continue
    go to 110
40  continue
    !
    !        computation of banded approximate jacobian.
    !
    do k = 1, msum
        do j = k, n, msum
            wa2(j) = x(j)
            h = eps*abs(wa2(j))
            if (h == zero) h = eps
            x(j) = wa2(j) + h
        end do
        call fcn(n,x,wa1,iflag)
        if (iflag < 0) go to 100
        do j = k, n, msum
            x(j) = wa2(j)
            h = eps*abs(wa2(j))
            if (h == zero) h = eps
            do i = 1, n
                fjac(i,j) = zero
                if (i >= j - mu .and. i <= j + ml) fjac(i,j) = (wa1(i) - fvec(i))/h
            end do
        end do
    end do
100 continue
110 continue
    return
    !
    !     last card of subroutine fdjac1.
    !
    end subroutine fdjac1

! --------------------------- qform -------------------------------------------

    subroutine qform(m,n,q,ldq,wa)
    !***begin prologue  qform
    !***refer to  snsq,snsqe
    !
    !     **********
    !     subroutine qform
    !
    !     this subroutine proceeds from the computed qr factorization of
    !     an m by n matrix a to accumulate the m by m orthogonal matrix
    !     q from its factored form.
    !
    !     the subroutine statement is
    !
    !       subroutine qform(m,n,q,ldq,wa)
    !
    !     where
    !
    !       m is a positive integer input variable set to the number
    !         of rows of a and the order of q.
    !
    !       n is a positive integer input variable set to the number
    !         of columns of a.
    !
    !       q is an m by m array. on input the full lower trapezoid in
    !         the first min(m,n) columns of q contains the factored form.
    !         on output q has been accumulated into a square matrix.
    !
    !       ldq is a positive integer input variable not less than m
    !         which specifies the leading dimension of the array q.
    !
    !       wa is a work array of length m.
    !
    !     subprograms called
    !
    !       fortran-supplied ... min0
    !
    !     minpack. version of january 1979.
    !     burton s. garbow, kenneth e. hillstrom, jorge j. more
    !     **********
    !***routines called  (none)
    !***end prologue  qform
    
    implicit none
    
    integer :: m, n, ldq,  i, j, jm1, k, l, minmn, np1
    real(8) :: q(ldq, m), wa(m), one, zero, sum, temp

    data one,zero /1.0d0,0.0d0/
    !***first executable statement  qform
    minmn = min0(m,n)
    if (minmn < 2) go to 30
    do j = 2, minmn
        jm1 = j - 1
        do i = 1, jm1
            q(i,j) = zero
        end do
    end do
30  continue
    !
    !     initialize remaining columns to those of the identity matrix.
    !
    np1 = n + 1
    if (m < np1) go to 60
    do j = np1, m
        do i = 1, m
            q(i,j) = zero
        end do
        q(j,j) = one
    end do
60  continue
    !
    !     accumulate q from its factored form.
    !
    do l = 1, minmn
        k = minmn - l + 1
        do i = k, m
            wa(i) = q(i,k)
            q(i,k) = zero
        end do
        q(k,k) = one
        if (wa(k) == zero) go to 110
        do j = k, m
            sum = zero
            do i = k, m
                sum = sum + q(i,j)*wa(i)
            end do
            temp = sum/wa(k)
            do i = k, m
                q(i,j) = q(i,j) - temp*wa(i)
            end do
        end do
110     continue
    end do
    return
    !
    !     last card of subroutine qform.
    !
    end subroutine qform

! --------------------------- qrfac -------------------------------------------

    subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,sigma,acnorm,wa)
    !***begin prologue  qrfac
    !***refer to  snls1,snls1e,snsq,snsqe
    !
    !     **********
    !     subroutine qrfac
    !
    !     this subroutine uses householder transformations with column
    !     pivoting (optional) to compute a qr factorization of the
    !     m by n matrix a. that is, qrfac determines an orthogonal
    !     matrix q, a permutation matrix p, and an upper trapezoidal
    !     matrix r with diagonal elements of nonincreasing magnitude,
    !     such that a*p = q*r. the householder transformation for
    !     column k, k = 1,2,...,min(m,n), is of the form
    !
    !                           t
    !           i - (1/u(k))*u*u
    !
    !     where u has zeros in the first k-1 positions. the form of
    !     this transformation and the method of pivoting first
    !     appeared in the corresponding linpack subroutine.
    !
    !     the subroutine statement is
    !
    !       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,sigma,acnorm,wa)
    !
    !     where
    !
    !       m is a positive integer input variable set to the number
    !         of rows of a.
    !
    !       n is a positive integer input variable set to the number
    !         of columns of a.
    !
    !       a is an m by n array. on input a contains the matrix for
    !         which the qr factorization is to be computed. on output
    !         the strict upper trapezoidal part of a contains the strict
    !         upper trapezoidal part of r, and the lower trapezoidal
    !         part of a contains a factored form of q (the non-trivial
    !         elements of the u vectors described above).
    !
    !       lda is a positive integer input variable not less than m
    !         which specifies the leading dimension of the array a.
    !
    !       pivot is a logical input variable. if pivot is set .true.,
    !         then column pivoting is enforced. if pivot is set .false.,
    !         then no column pivoting is done.
    !
    !       ipvt is an integer output array of length lipvt. ipvt
    !         defines the permutation matrix p such that a*p = q*r.
    !         column j of p is column ipvt(j) of the identity matrix.
    !         if pivot is .false., ipvt is not referenced.
    !
    !       lipvt is a positive integer input variable. if pivot is
    !             .false., then lipvt may be as small as 1. if pivot is
    !             .true., then lipvt must be at least n.
    !
    !       sigma is an output array of length n which contains the
    !         diagonal elements of r.
    !
    !       acnorm is an output array of length n which contains the
    !         norms of the corresponding columns of the input matrix a.
    !         if this information is not needed, then acnorm can coincide
    !         with sigma.
    !
    !       wa is a work array of length n. if pivot is .false., then wa
    !         can coincide with sigma.
    !
    !     subprograms called
    !
    !       minpack-supplied ... r1mach,enorm
    !       fortran-supplied ... max,sqrt,min0
    !
    !     minpack. version of december 1978.
    !     burton s. garbow, kenneth e. hillstrom, jorge j. more
    !     **********
    !***routines called  enorm,r1mach
    !***end prologue  qrfac
    
    implicit none
    
    integer :: m,n,lda,lipvt,ipvt(lipvt),i,j,jp1,k,kmax,minmn
    logical pivot
    real(8) ::  a(lda,n),sigma(n),acnorm(n),wa(n), one, zero, p05, epsmch, d1mach, enorm, temp, ajnorm, sum

    data one,p05,zero /1.0d0,5.0d-2,0.0d0/
    !***first executable statement  qrfac
    epsmch = d1mach(4)
    !
    !     compute the initial column norms and initialize several arrays.
    !
    do j = 1, n
        acnorm(j) = enorm(m,a(1,j))
        sigma(j) = acnorm(j)
        wa(j) = sigma(j)
        if (pivot) ipvt(j) = j
    end do
    !
    !     reduce a to r with householder transformations.
    !
    minmn = min0(m,n)
    do j = 1, minmn
        if (.not.pivot) go to 40
        !
        !        bring the column of largest norm into the pivot position.
        !
        kmax = j
        do k = j, n
            if (sigma(k) > sigma(kmax)) kmax = k
        end do
        if (kmax == j) go to 40
        do i = 1, m
            temp = a(i,j)
            a(i,j) = a(i,kmax)
            a(i,kmax) = temp
        end do
        sigma(kmax) = sigma(j)
        wa(kmax) = wa(j)
        k = ipvt(j)
        ipvt(j) = ipvt(kmax)
        ipvt(kmax) = k
40      continue
        !
        !        compute the householder transformation to reduce the
        !        j-th column of a to a multiple of the j-th unit vector.
        !
        ajnorm = enorm(m-j+1,a(j,j))
        if (ajnorm == zero) go to 100
        if (a(j,j) < zero) ajnorm = -ajnorm
        do i = j, m
            a(i,j) = a(i,j)/ajnorm
        end do
        a(j,j) = a(j,j) + one
        !
        !        apply the transformation to the remaining columns
        !        and update the norms.
        !
        jp1 = j + 1
        if (n < jp1) go to 100
        do k = jp1, n
            sum = zero
            do i = j, m
                sum = sum + a(i,j)*a(i,k)
            end do
            temp = sum/a(j,j)
            do i = j, m
                a(i,k) = a(i,k) - temp*a(i,j)
            end do
            if (.not.pivot .or. sigma(k) == zero) go to 80
            temp = a(j,k)/sigma(k)
            sigma(k) = sigma(k)*sqrt(max(zero,one-temp**2))
            if (p05*(sigma(k)/wa(k))**2 > epsmch) go to 80
            sigma(k) = enorm(m-j,a(jp1,k))
            wa(k) = sigma(k)
80          continue
        end do
100     continue
        sigma(j) = -ajnorm
    end do
    return
    !
    !     last card of subroutine qrfac.
    !
    end subroutine qrfac

! --------------------------- r1mpyq -------------------------------------------

    subroutine r1mpyq(m,n,a,lda,v,w)
    !***begin prologue  r1mpyq
    !***refer to  snsq,snsqe
    !
    !     **********
    !     subroutine r1mpyq
    !
    !     given an m by n matrix a, this subroutine computes a*q where
    !     q is the product of 2*(n - 1) transformations
    !
    !           gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
    !
    !     and gv(i), gw(i) are givens rotations in the (i,n) plane which
    !     eliminate elements in the i-th and n-th planes, respectively.
    !     q itself is not given, rather the information to recover the
    !     gv, gw rotations is supplied.
    !
    !     the subroutine statement is
    !
    !       subroutine r1mpyq(m,n,a,lda,v,w)
    !
    !     where
    !
    !       m is a positive integer input variable set to the number
    !         of rows of a.
    !
    !       n is a positive integer input variable set to the number
    !         of columns of a.
    !
    !       a is an m by n array. on input a must contain the matrix
    !         to be postmultiplied by the orthogonal matrix q
    !         described above. on output a*q has replaced a.
    !
    !       lda is a positive integer input variable not less than m
    !         which specifies the leading dimension of the array a.
    !
    !       v is an input array of length n. v(i) must contain the
    !         information necessary to recover the givens rotation gv(i)
    !         described above.
    !
    !       w is an input array of length n. w(i) must contain the
    !         information necessary to recover the givens rotation gw(i)
    !         described above.
    !
    !     subroutines called
    !
    !       fortran-supplied ... abs,sqrt
    !
    !     minpack. version of december 1978.
    !     burton s. garbow, kenneth e. hillstrom, jorge j. more
    !     **********
    !***routines called  (none)
    !***end prologue  r1mpyq
    
    implicit none
    integer m,n,lda,i,j,nmj,nm1
    real(8) :: a(lda,n),v(n),w(n),one, temp, cos, sin

    data one /1.0d0/
    !***first executable statement  r1mpyq
    nm1 = n - 1
    if (nm1 < 1) go to 50
    do nmj = 1, nm1
        j = n - nmj
        if (abs(v(j)) > one) cos = one/v(j)
        if (abs(v(j)) > one) sin = sqrt(one-cos**2)
        if (abs(v(j)) <= one) sin = v(j)
        if (abs(v(j)) <= one) cos = sqrt(one-sin**2)
        do i = 1, m
            temp = cos*a(i,j) - sin*a(i,n)
            a(i,n) = sin*a(i,j) + cos*a(i,n)
            a(i,j) = temp
        end do
    end do
    !
    !     apply the second set of givens rotations to a.
    !
    do j = 1, nm1
        if (abs(w(j)) > one) cos = one/w(j)
        if (abs(w(j)) > one) sin = sqrt(one-cos**2)
        if (abs(w(j)) <= one) sin = w(j)
        if (abs(w(j)) <= one) cos = sqrt(one-sin**2)
        do i = 1, m
            temp = cos*a(i,j) + sin*a(i,n)
            a(i,n) = -sin*a(i,j) + cos*a(i,n)
            a(i,j) = temp
        end do
    end do
50  continue
    return
    !
    !     last card of subroutine r1mpyq.
    !
    end subroutine r1mpyq

! --------------------------- r1updt -------------------------------------------

    subroutine r1updt(m,n,s,ls,u,v,w,sing)
    !***begin prologue  r1updt
    !***refer to  snsq,snsqe
    !
    !     **********
    !     subroutine r1updt
    !
    !     given an m by n lower trapezoidal matrix s, an m-vector u,
    !     and an n-vector v, the problem is to determine an
    !     orthogonal matrix q such that
    !
    !                   t
    !           (s + u*v )*q
    !
    !     is again lower trapezoidal.
    !
    !     this subroutine determines q as the product of 2*(n - 1)
    !     transformations
    !
    !           gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
    !
    !     where gv(i), gw(i) are givens rotations in the (i,n) plane
    !     which eliminate elements in the i-th and n-th planes,
    !     respectively. q itself is not accumulated, rather the
    !     information to recover the gv, gw rotations is returned.
    !
    !     the subroutine statement is
    !
    !       subroutine r1updt(m,n,s,ls,u,v,w,sing)
    !
    !     where
    !
    !       m is a positive integer input variable set to the number
    !         of rows of s.
    !
    !       n is a positive integer input variable set to the number
    !         of columns of s. n must not exceed m.
    !
    !       s is an array of length ls. on input s must contain the lower
    !         trapezoidal matrix s stored by columns. on output s contains
    !         the lower trapezoidal matrix produced as described above.
    !
    !       ls is a positive integer input variable not less than
    !         (n*(2*m-n+1))/2.
    !
    !       u is an input array of length m which must contain the
    !         vector u.
    !
    !       v is an array of length n. on input v must contain the vector
    !         v. on output v(i) contains the information necessary to
    !         recover the givens rotation gv(i) described above.
    !
    !       w is an output array of length m. w(i) contains information
    !         necessary to recover the givens rotation gw(i) described
    !         above.
    !
    !       sing is a logical output variable. sing is set .true. if any
    !         of the diagonal elements of the output s are zero. otherwise
    !         sing is set .false.
    !
    !     subprograms called
    !
    !       minpack-supplied ... r1mach
    !       fortran-supplied ... abs,sqrt
    !
    !     minpack. version of december 1978.
    !     burton s. garbow, kenneth e. hillstrom, jorge j. more,
    !     john l. nazareth
    !     **********
    !***routines called  r1mach
    !***end prologue  r1updt
    
    implicit none
    integer :: m,n,ls, i,j,jj,l,nmj,nm1
    logical :: sing
    real(8) :: s(ls),u(m),v(n),w(m), one, p5, p25, zero, giant, d1mach, cotan, sin, cos, tau, tan, temp
    
    data one,p5,p25,zero /1.0d0,5.0d-1,2.5d-1,0.0d0/
    !***first executable statement  r1updt
    giant = d1mach(2)
    !
    !     initialize the diagonal element pointer.
    !
    jj = (n*(2*m - n + 1))/2 - (m - n)
    !
    !     move the nontrivial part of the last column of s into w.
    !
    l = jj
    do i = n, m
        w(i) = s(l)
        l = l + 1
    end do
    !
    !     rotate the vector v into a multiple of the n-th unit vector
    !     in such a way that a spike is introduced into w.
    !
    nm1 = n - 1
    if (nm1 < 1) go to 70
    do nmj = 1, nm1
        j = n - nmj
        jj = jj - (m - j + 1)
        w(j) = zero
        if (v(j) == zero) go to 50
        !
        !        determine a givens rotation which eliminates the
        !        j-th element of v.
        !
        if (abs(v(n)) >= abs(v(j))) go to 20
        cotan = v(n)/v(j)
        sin = p5/sqrt(p25+p25*cotan**2)
        cos = sin*cotan
        tau = one
        if (abs(cos)*giant > one) tau = one/cos
        go to 30
20      continue
        tan = v(j)/v(n)
        cos = p5/sqrt(p25+p25*tan**2)
        sin = cos*tan
        tau = sin
30      continue
        !
        !        apply the transformation to v and store the information
        !        necessary to recover the givens rotation.
        !
        v(n) = sin*v(j) + cos*v(n)
        v(j) = tau
        !
        !        apply the transformation to s and extend the spike in w.
        !
        l = jj
        do i = j, m
            temp = cos*s(l) - sin*w(i)
            w(i) = sin*s(l) + cos*w(i)
            s(l) = temp
            l = l + 1
        end do
50      continue
    end do
70  continue
    !
    !     add the spike from the rank 1 update to w.
    !
    do i = 1, m
        w(i) = w(i) + v(n)*u(i)
    end do
    !
    !     eliminate the spike.
    !
    sing = .false.
    if (nm1 < 1) go to 140
    do j = 1, nm1
        if (w(j) == zero) go to 120
        !
        !        determine a givens rotation which eliminates the
        !        j-th element of the spike.
        !
        if (abs(s(jj)) >= abs(w(j))) go to 90
        cotan = s(jj)/w(j)
        sin = p5/sqrt(p25+p25*cotan**2)
        cos = sin*cotan
        tau = one
        if (abs(cos)*giant > one) tau = one/cos
        go to 100
90      continue
        tan = w(j)/s(jj)
        cos = p5/sqrt(p25+p25*tan**2)
        sin = cos*tan
        tau = sin
100     continue
        !
        !        apply the transformation to s and reduce the spike in w.
        !
        l = jj
        do i = j, m
            temp = cos*s(l) + sin*w(i)
            w(i) = -sin*s(l) + cos*w(i)
            s(l) = temp
            l = l + 1
        end do
        !
        !        store the information necessary to recover the
        !        givens rotation.
        !
        w(j) = tau
120     continue
        !
        !        test for zero diagonal elements in the output s.
        !
        if (s(jj) == zero) sing = .true.
        jj = jj + (m - j + 1)
    end do
140 continue
    !
    !     move w back into the last column of the output s.
    !
    l = jj
    do i = n, m
        s(l) = w(i)
        l = l + 1
    end do
    if (s(jj) == zero) sing = .true.
    return
    !
    !     last card of subroutine r1updt.
    !
    end subroutine r1updt

! --------------------------- snsq -------------------------------------------

    subroutine snsq(fcn,jac,iopt,n,x,fvec,fjac,ldfjac,xtol,maxfev,ml,mu,epsfcn,diag,mode,factor,nprint,info,nfev,njev,r,lr,qtf,wa1,wa2,wa3,wa4)
    !***begin prologue  snsq
    !***date written   800301   (yymmdd)
    !***revision date  840405   (yymmdd)
    !***category no.  f2a
    !***keywords  nonlinear square system,powell hybrid method,zero
    !***author  hiebert, k. l., (snla)
    !***purpose  snsq finds to find a zero of a system of n nonlinear
    !            functions in n variables by a modification of the powell
    !            hybrid method.  this code is the combination of the minpack
    !            codes (argonne) hybrd and hybrdj.
    !***description
    !
    ! 1. purpose.
    !
    !       the purpose of snsq is to find a zero of a system of n non-
    !       linear functions in n variables by a modification of the powell
    !       hybrid method.  the user must provide a subroutine which calcu-
    !       lates the functions.  the user has the option of either to
    !       provide a subroutine which calculates the jacobian or to let the
    !       code calculate it by a forward-difference approximation.
    !       this code is the combination of the minpack codes (argonne)
    !       hybrd and hybrdj.
    !
    !
    ! 2. subroutine and type statements.
    !
    !       subroutine snsq(fcn,jac,iopt,n,x,fvec,fjac,ldfjac,xtol,maxfev,
    !      *                 ml,mu,epsfcn,diag,mode,factor,nprint,info,nfev,
    !      *                 njev,r,lr,qtf,wa1,wa2,wa3,wa4)
    !       integer iopt,n,maxfev,ml,mu,mode,nprint,info,nfev,ldfjac,njev,lr
    !       real xtol,epsfcn,factor
    !       real x(n),fvec(n),diag(n),fjac(ldfjac,n),r(lr),qtf(n),
    !      *     wa1(n),wa2(n),wa3(n),wa4(n)
    !       external fcn,jac
    !
    !
    ! 3. parameters.
    !
    !       parameters designated as input parameters must be specified on
    !       entry to snsq and are not changed on exit, while parameters
    !       designated as output parameters need not be specified on entry
    !       and are set to appropriate values on exit from snsq.
    !
    !       fcn is the name of the user-supplied subroutine which calculates
    !         the functions.  fcn must be declared in an external statement
    !         in the user calling program, and should be written as follows.
    !
    !         subroutine fcn(n,x,fvec,iflag)
    !         integer n,iflag
    !         real x(n),fvec(n)
    !         ----------
    !         calculate the functions at x and
    !         return this vector in fvec.
    !         ----------
    !         return
    !         end
    !
    !         the value of iflag should not be changed by fcn unless the
    !         user wants to terminate execution of snsq.  in this case, set
    !         iflag to a negative integer.
    !
    !       jac is the name of the user-supplied subroutine which calculates
    !         the jacobian.  if iopt=1, then jac must be declared in an
    !         external statement in the user calling program, and should be
    !         written as follows.
    !
    !         subroutine jac(n,x,fvec,fjac,ldfjac,iflag)
    !         integer n,ldfjac,iflag
    !         real x(n),fvec(n),fjac(ldfjac,n)
    !         ----------
    !         calculate the jacobian at x and return this
    !         matrix in fjac.  fvec contains the function
    !         values at x and should not be altered.
    !         ----------
    !         return
    !         end
    !
    !         the value of iflag should not be changed by jac unless the
    !         user wants to terminate execution of snsq.  in this case, set
    !         iflag to a negative integer.
    !
    !         if iopt=2, jac can be ignored (treat it as a dummy argument).
    !
    !       iopt is an input variable which specifies how the jacobian will
    !         be calculated.  if iopt=1, then the user must supply the
    !         jacobian through the subroutine jac.  if iopt=2, then the
    !         code will approximate the jacobian by forward-differencing.
    !
    !       n is a positive integer input variable set to the number of
    !         functions and variables.
    !
    !       x is an array of length n.  on input, x must contain an initial
    !         estimate of the solution vector.  on output, x contains the
    !         final estimate of the solution vector.
    !
    !       fvec is an output array of length n which contains the functions
    !         evaluated at the output x.
    !
    !       fjac is an output n by n array which contains the orthogonal
    !         matrix q produced by the qr factorization of the final approx-
    !         imate jacobian.
    !
    !       ldfjac is a positive integer input variable not less than n
    !         which specifies the leading dimension of the array fjac.
    !
    !       xtol is a non-negative input variable.  termination occurs when
    !         the relative error between two consecutive iterates is at most
    !         xtol.  therefore, xtol measures the relative error desired in
    !         the approximate solution.  section 4 contains more details
    !         about xtol.
    !
    !       maxfev is a positive integer input variable.  termination occurs
    !         when the number of calls to fcn is at least maxfev by the end
    !         of an iteration.
    !
    !       ml is a non-negative integer input variable which specifies the
    !         number of subdiagonals within the band of the jacobian matrix.
    !         if the jacobian is not banded or iopt=1, set ml to at
    !         least n - 1.
    !
    !       mu is a non-negative integer input variable which specifies the
    !         number of superdiagonals within the band of the jacobian
    !         matrix.  if the jacobian is not banded or iopt=1, set mu to at
    !         least n - 1.
    !
    !       epsfcn is an input variable used in determining a suitable step
    !         for the forward-difference approximation.  this approximation
    !         assumes that the relative errors in the functions are of the
    !         order of epsfcn.  if epsfcn is less than the machine preci-
    !         sion, it is assumed that the relative errors in the functions
    !         are of the order of the machine precision.  if iopt=1, then
    !         epsfcn can be ignored (treat it as a dummy argument).
    !
    !       diag is an array of length n.  if mode = 1 (see below), diag is
    !         internally set.  if mode = 2, diag must contain positive
    !         entries that serve as implicit (multiplicative) scale factors
    !         for the variables.
    !
    !       mode is an integer input variable.  if mode = 1, the variables
    !         will be scaled internally.  if mode = 2, the scaling is speci-
    !         fied by the input diag.  other values of mode are equivalent
    !         to mode = 1.
    !
    !       factor is a positive input variable used in determining the ini-
    !         tial step bound.  this bound is set to the product of factor
    !         and the euclidean norm of diag*x if nonzero, or else to factor
    !         itself.  in most cases factor should lie in the interval
    !         (.1,100.).  100. is a generally recommended value.
    !
    !       nprint is an integer input variable that enables controlled
    !         printing of iterates if it is positive.  in this case, fcn is
    !         called with iflag = 0 at the beginning of the first iteration
    !         and every nprint iteration thereafter and immediately prior
    !         to return, with x and fvec available for printing. appropriate
    !         print statements must be added to fcn(see example).  if nprint
    !         is not positive, no special calls of fcn with iflag = 0 are
    !         made.
    !
    !       info is an integer output variable.  if the user has terminated
    !         execution, info is set to the (negative) value of iflag.  see
    !         description of fcn and jac. otherwise, info is set as follows.
    !
    !         info = 0  improper input parameters.
    !
    !         info = 1  relative error between two consecutive iterates is
    !                   at most xtol.
    !
    !         info = 2  number of calls to fcn has reached or exceeded
    !                   maxfev.
    !
    !         info = 3  xtol is too small.  no further improvement in the
    !                   approximate solution x is possible.
    !
    !         info = 4  iteration is not making good progress, as measured
    !                   by the improvement from the last five jacobian eval-
    !                   uations.
    !
    !         info = 5  iteration is not making good progress, as measured
    !                   by the improvement from the last ten iterations.
    !
    !         sections 4 and 5 contain more details about info.
    !
    !       nfev is an integer output variable set to the number of calls to
    !         fcn.
    !
    !       njev is an integer output variable set to the number of calls to
    !         jac. (if iopt=2, then njev is set to zero.)
    !
    !       r is an output array of length lr which contains the upper
    !         triangular matrix produced by the qr factorization of the
    !         final approximate jacobian, stored rowwise.
    !
    !       lr is a positive integer input variable not less than
    !         (n*(n+1))/2.
    !
    !       qtf is an output array of length n which contains the vector
    !         (q transpose)*fvec.
    !
    !       wa1, wa2, wa3, and wa4 are work arrays of length n.
    !
    !
    ! 4. successful completion.
    !
    !       the accuracy of snsq is controlled by the convergence parameter
    !       xtol.  this parameter is used in a test which makes a comparison
    !       between the approximation x and a solution xsol.  snsq termi-
    !       nates when the test is satisfied.  if the convergence parameter
    !       is less than the machine precision (as defined by the function
    !       r1mach(4)), then snsq only attempts to satisfy the test
    !       defined by the machine precision.  further progress is not
    !       usually possible.
    !
    !       the test assumes that the functions are reasonably well behaved,
    !       and, if the jacobian is supplied by the user, that the functions
    !       and the jacobian are coded consistently.  if these conditions
    !       are not satisfied, then snsq may incorrectly indicate conver-
    !       gence.  the coding of the jacobian can be checked by the
    !       subroutine chkder. if the jacobian is coded correctly or iopt=2,
    !       then the validity of the answer can be checked, for example, by
    !       rerunning snsq with a tighter tolerance.
    !
    !       convergence test.  if enorm(z) denotes the euclidean norm of a
    !         vector z and d is the diagonal matrix whose entries are
    !         defined by the array diag, then this test attempts to guaran-
    !         tee that
    !
    !               enorm(d*(x-xsol)) <= xtol*enorm(d*xsol).
    !
    !         if this condition is satisfied with xtol = 10**(-k), then the
    !         larger components of d*x have k significant decimal digits and
    !         info is set to 1.  there is a danger that the smaller compo-
    !         nents of d*x may have large relative errors, but the fast rate
    !         of convergence of snsq usually avoids this possibility.
    !         unless high precision solutions are required, the recommended
    !         value for xtol is the square root of the machine precision.
    !
    !
    ! 5. unsuccessful completion.
    !
    !       unsuccessful termination of snsq can be due to improper input
    !       parameters, arithmetic interrupts, an excessive number of func-
    !       tion evaluations, or lack of good progress.
    !
    !       improper input parameters.  info is set to 0 if iopt < 1,
    !         or iopt > 2, or n <= 0, or ldfjac < n, or
    !         xtol < 0.e0, or maxfev <= 0, or ml < 0, or mu < 0,
    !         or factor <= 0.e0, or lr < (n*(n+1))/2.
    !
    !       arithmetic interrupts.  if these interrupts occur in the fcn
    !         subroutine during an early stage of the computation, they may
    !         be caused by an unacceptable choice of x by snsq.  in this
    !         case, it may be possible to remedy the situation by rerunning
    !         snsq with a smaller value of factor.
    !
    !       excessive number of function evaluations.  a reasonable value
    !         for maxfev is 100*(n+1) for iopt=1 and 200*(n+1) for iopt=2.
    !         if the number of calls to fcn reaches maxfev, then this
    !         indicates that the routine is converging very slowly as
    !         measured by the progress of fvec, and info is set to 2.  this
    !         situation should be unusual because, as indicated below, lack
    !         of good progress is usually diagnosed earlier by snsq,
    !         causing termination with info = 4 or info = 5.
    !
    !       lack of good progress.  snsq searches for a zero of the system
    !         by minimizing the sum of the squares of the functions.  in so
    !         doing, it can become trapped in a region where the minimum
    !         does not correspond to a zero of the system and, in this situ-
    !         ation, the iteration eventually fails to make good progress.
    !         in particular, this will happen if the system does not have a
    !         zero.  if the system has a zero, rerunning snsq from a dif-
    !         ferent starting point may be helpful.
    !
    !
    ! 6. characteristics of the algorithm.
    !
    !       snsq is a modification of the powell hybrid method.  two of its
    !       main characteristics involve the choice of the correction as a
    !       convex combination of the newton and scaled gradient directions,
    !       and the updating of the jacobian by the rank-1 method of broy-
    !       den.  the choice of the correction guarantees (under reasonable
    !       conditions) global convergence for starting points far from the
    !       solution and a fast rate of convergence.  the jacobian is
    !       calculated at the starting point by either the user-supplied
    !       subroutine or a forward-difference approximation, but it is not
    !       recalculated until the rank-1 method fails to produce satis-
    !       factory progress.
    !
    !       timing.  the time required by snsq to solve a given problem
    !         depends on n, the behavior of the functions, the accuracy
    !         requested, and the starting point.  the number of arithmetic
    !         operations needed by snsq is about 11.5*(n**2) to process
    !         each evaluation of the functions (call to fcn) and 1.3*(n**3)
    !         to process each evaluation of the jacobian (call to jac,
    !         if iopt = 1).  unless fcn and jac can be evaluated quickly,
    !         the timing of snsq will be strongly influenced by the time
    !         spent in fcn and jac.
    !
    !       storage.  snsq requires (3*n**2 + 17*n)/2 single precision
    !         storage locations, in addition to the storage required by the
    !         program.  there are no internally declared storage arrays.
    !
    !
    ! 7. example.
    !
    !       the problem is to determine the values of x(1), x(2), ..., x(9),
    !       which solve the system of tridiagonal equations
    !
    !       (3-2*x(1))*x(1)           -2*x(2)                   = -1
    !               -x(i-1) + (3-2*x(i))*x(i)         -2*x(i+1) = -1, i=2-8
    !                                   -x(8) + (3-2*x(9))*x(9) = -1
    ! c     **********
    !
    !       program test(input,output,tape6=output)
    ! c
    ! c     driver for snsq example.
    ! c
    !       integer j,iopt,n,maxfev,ml,mu,mode,nprint,info,nfev,ldfjac,lr,
    !      *        nwrite
    !       real xtol,epsfcn,factor,fnorm
    !       real x(9),fvec(9),diag(9),fjac(9,9),r(45),qtf(9),
    !      *     wa1(9),wa2(9),wa3(9),wa4(9)
    !       real enorm,r1mach
    !       external fcn
    !       data nwrite /6/
    ! c
    !       iopt = 2
    !       n = 9
    ! c
    ! c     the following starting values provide a rough solution.
    ! c
    !       do 10 j = 1, 9
    !          x(j) = -1.e0
    !    10    continue
    ! c
    !       ldfjac = 9
    !       lr = 45
    ! c
    ! c     set xtol to the square root of the machine precision.
    ! c     unless high precision solutions are required,
    ! c     this is the recommended setting.
    ! c
    !       xtol = sqrt(r1mach(4))
    ! c
    !       maxfev = 2000
    !       ml = 1
    !       mu = 1
    !       epsfcn = 0.e0
    !       mode = 2
    !       do 20 j = 1, 9
    !          diag(j) = 1.e0
    !    20    continue
    !       factor = 1.e2
    !       nprint = 0
    ! c
    !       call snsq(fcn,jac,iopt,n,x,fvec,fjac,ldfjac,xtol,maxfev,ml,mu,
    !      *           epsfcn,diag,mode,factor,nprint,info,nfev,njev,
    !      *           r,lr,qtf,wa1,wa2,wa3,wa4)
    !       fnorm = enorm(n,fvec)
    !       write (nwrite,1000) fnorm,nfev,info,(x(j),j=1,n)
    !       stop
    !  1000 format (5x,' final l2 norm of the residuals',e15.7 //
    !      *        5x,' number of function evaluations',i10 //
    !      *        5x,' exit parameter',16x,i10 //
    !      *        5x,' final approximate solution' // (5x,3e15.7))
    !       end
    !       subroutine fcn(n,x,fvec,iflag)
    !       integer n,iflag
    !       real x(n),fvec(n)
    !       integer k
    !       real one,temp,temp1,temp2,three,two,zero
    !       data zero,one,two,three /0.e0,1.e0,2.e0,3.e0/
    ! c
    !       if (iflag /= 0) go to 5
    ! c
    ! c     insert print statements here when nprint is positive.
    ! c
    !       return
    !     5 continue
    !       do 10 k = 1, n
    !          temp = (three - two*x(k))*x(k)
    !          temp1 = zero
    !          if (k /= 1) temp1 = x(k-1)
    !          temp2 = zero
    !          if (k /= n) temp2 = x(k+1)
    !          fvec(k) = temp - temp1 - two*temp2 + one
    !    10    continue
    !       return
    !       end
    !
    !       results obtained with different compilers or machines
    !       may be slightly different.
    !
    !       final l2 norm of the residuals  0.1192636e-07
    !
    !       number of function evaluations        14
    !
    !       exit parameter                         1
    !
    !       final approximate solution
    !
    !       -0.5706545e+00 -0.6816283e+00 -0.7017325e+00
    !       -0.7042129e+00 -0.7013690e+00 -0.6918656e+00
    !       -0.6657920e+00 -0.5960342e+00 -0.4164121e+00
    !***references  powell, m. j. d.
    !                 a hybrid method for nonlinear equations.
    !                 numerical methods for nonlinear algebraic equations,
    !                 p. rabinowitz, editor.  gordon and breach, 1970.
    !***routines called  dogleg,enorm,fdjac1,qform,qrfac,r1mach,r1mpyq,
    !                    r1updt,xerror
    !***end prologue  snsq
    
    implicit none
    integer :: iopt,n,maxfev,ml,mu,mode,nprint,info,nfev,ldfjac,lr,njev,i,iflag,iter,j,jm1,l,ncfail,ncsuc,nslow1,nslow2,iwa(1)
    real(8) :: x(n),fvec(n),diag(n),fjac(ldfjac,n),r(lr),qtf(n),wa1(n),wa2(n),wa3(n),wa4(n), one, p1, p5, p001, p0001, zero, epsmch, d1mach, &
        xtol, factor, fnorm, enorm, epsfcn, xnorm, delta, sum, temp, pnorm, fnorm1, actred, prered, ratio
    external fcn
    logical :: jeval,sing
    data one,p1,p5,p001,p0001,zero /1.0d0,1.0d-1,5.0d-1,1.0d-3,1.0d-4,0.0d0/
    !
    !***first executable statement  snsq
    epsmch = d1mach(4)
    !
    info = 0
    iflag = 0
    nfev = 0
    njev = 0
    !
    !     check the input parameters for errors.
    !
    if (iopt < 1 .or. iopt > 2 .or. n <= 0 .or. xtol < zero .or. maxfev <= 0 .or. ml < 0 .or. mu < 0 .or. factor <= zero .or. ldfjac < n .or. lr < (n*(n + 1))/2) go to 300
    if (mode /= 2) go to 20
    do j = 1, n
        if (diag(j) <= zero) go to 300
    end do
20  continue
    !
    !     evaluate the function at the starting point
    !     and calculate its norm.
    !
    iflag = 1
    call fcn(n,x,fvec,iflag)
    nfev = 1
    if (iflag < 0) go to 300
    fnorm = enorm(n,fvec)
    !
    !     initialize iteration counter and monitors.
    !
    iter = 1
    ncsuc = 0
    ncfail = 0
    nslow1 = 0
    nslow2 = 0
    !
    !     beginning of the outer loop.
    !
30  continue
    jeval = .true.
    !
    !        calculate the jacobian matrix.
    !
    if (iopt == 2) go to 31
    !
    !        user supplies jacobian
    !
    call jac(n,x,fvec,fjac,ldfjac,iflag)
    njev = njev+1
    go to 32
    !
    !        code approximates the jacobian
    !
31  iflag = 2
    call fdjac1(fcn,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,wa1,wa2)
    nfev = nfev + min0(ml+mu+1,n)
    !
32  if (iflag < 0) go to 300
    !
    !        compute the qr factorization of the jacobian.
    !
    call qrfac(n,n,fjac,ldfjac,.false.,iwa,1,wa1,wa2,wa3)
    !
    !        on the first iteration and if mode is 1, scale according
    !        to the norms of the columns of the initial jacobian.
    !
    if (iter /= 1) go to 70
    if (mode == 2) go to 50
    do j = 1, n
        diag(j) = wa2(j)
        if (wa2(j) == zero) diag(j) = one
    end do
50  continue
    !
    !        on the first iteration, calculate the norm of the scaled x
    !        and initialize the step bound delta.
    !
    do j = 1, n
        wa3(j) = diag(j)*x(j)
    end do
    xnorm = enorm(n,wa3)
    delta = factor*xnorm
    if (delta == zero) delta = factor
70  continue
    !
    !        form (q transpose)*fvec and store in qtf.
    !
    do i = 1, n
        qtf(i) = fvec(i)
    end do
    do j = 1, n
        if (fjac(j,j) == zero) go to 110
        sum = zero
        do i = j, n
            sum = sum + fjac(i,j)*qtf(i)
        end do
        temp = -sum/fjac(j,j)
        do i = j, n
            qtf(i) = qtf(i) + fjac(i,j)*temp
        end do
110     continue
    end do
    !
    !        copy the triangular factor of the qr factorization into r.
    !
    sing = .false.
    do j = 1, n
        l = j
        jm1 = j - 1
        if (jm1 < 1) go to 140
        do i = 1, jm1
            r(l) = fjac(i,j)
            l = l + n - i
        end do
140     continue
        r(l) = wa1(j)
        if (wa1(j) == zero) sing = .true.
    end do
    !
    !        accumulate the orthogonal factor in fjac.
    !
    call qform(n,n,fjac,ldfjac,wa1)
    !
    !        rescale if necessary.
    !
    if (mode == 2) go to 170
    do 160 j = 1, n
        diag(j) = max(diag(j),wa2(j))
160 continue
170 continue
    !
    !        beginning of the inner loop.
    !
180 continue
    !
    !           if requested, call fcn to enable printing of iterates.
    !
    if (nprint <= 0) go to 190
    iflag = 0
    if (mod(iter-1,nprint) == 0) call fcn(n,x,fvec,iflag)
    if (iflag < 0) go to 300
190 continue
    !
    !           determine the direction p.
    !
    call dogleg(n,r,lr,diag,qtf,delta,wa1,wa2,wa3)
    !
    !           store the direction p and x + p. calculate the norm of p.
    !
    do j = 1, n
        wa1(j) = -wa1(j)
        wa2(j) = x(j) + wa1(j)
        wa3(j) = diag(j)*wa1(j)
    end do
    pnorm = enorm(n,wa3)
    !
    !           on the first iteration, adjust the initial step bound.
    !
    if (iter == 1) delta = min(delta,pnorm)
    !
    !           evaluate the function at x + p and calculate its norm.
    !
    iflag = 1
    call fcn(n,wa2,wa4,iflag)
    nfev = nfev + 1
    if (iflag < 0) go to 300
    fnorm1 = enorm(n,wa4)
    !
    !           compute the scaled actual reduction.
    !
    actred = -one
    if (fnorm1 < fnorm) actred = one - (fnorm1/fnorm)**2
    !
    !           compute the scaled predicted reduction.
    !
    l = 1
    do i = 1, n
        sum = zero
        do j = i, n
            sum = sum + r(l)*wa1(j)
            l = l + 1
        end do
        wa3(i) = qtf(i) + sum
    end do
    temp = enorm(n,wa3)
    prered = zero
    if (temp < fnorm) prered = one - (temp/fnorm)**2
    !
    !           compute the ratio of the actual to the predicted
    !           reduction.
    !
    ratio = zero
    if (prered > zero) ratio = actred/prered
    !
    !           update the step bound.
    !
    if (ratio >= p1) go to 230
    ncsuc = 0
    ncfail = ncfail + 1
    delta = p5*delta
    go to 240
230 continue
    ncfail = 0
    ncsuc = ncsuc + 1
    if (ratio >= p5 .or. ncsuc > 1) delta = max(delta,pnorm/p5)
    if (abs(ratio-one) <= p1) delta = pnorm/p5
240 continue
    !
    !           test for successful iteration.
    !
    if (ratio < p0001) go to 260
    !
    !           successful iteration. update x, fvec, and their norms.
    !
    do j = 1, n
        x(j) = wa2(j)
        wa2(j) = diag(j)*x(j)
        fvec(j) = wa4(j)
    end do
    xnorm = enorm(n,wa2)
    fnorm = fnorm1
    iter = iter + 1
260 continue
    !
    !           determine the progress of the iteration.
    !
    nslow1 = nslow1 + 1
    if (actred >= p001) nslow1 = 0
    if (jeval) nslow2 = nslow2 + 1
    if (actred >= p1) nslow2 = 0
    !
    !           test for convergence.
    !
    if (delta <= xtol*xnorm+xtol .or. fnorm == zero)info = 1
    if (info /= 0) go to 300
    !
    !           tests for termination and stringent tolerances.
    !
    if (nfev >= maxfev) info = 2
    if (p1*max(p1*delta,pnorm) <= epsmch*xnorm) info = 3
    if (nslow2 == 5) info = 4
    if (nslow1 == 10) info = 5
    if (info /= 0) go to 300
    !
    !           criterion for recalculating jacobian
    !
    if (ncfail == 2) go to 290
    !
    !           calculate the rank one modification to the jacobian
    !           and update qtf if necessary.
    !
    do j = 1, n
        sum = zero
        do i = 1, n
            sum = sum + fjac(i,j)*wa4(i)
        end do
        wa2(j) = (sum - wa3(j))/pnorm
        wa1(j) = diag(j)*((diag(j)*wa1(j))/pnorm)
        if (ratio >= p0001) qtf(j) = sum
    end do
    !
    !           compute the qr factorization of the updated jacobian.
    !
    call r1updt(n,n,r,lr,wa1,wa2,wa3,sing)
    call r1mpyq(n,n,fjac,ldfjac,wa2,wa3)
    call r1mpyq(1,n,qtf,1,wa2,wa3)
    !
    !           end of the inner loop.
    !
    jeval = .false.
    go to 180
290 continue
    !
    !        end of the outer loop.
    !
    go to 30
300 continue
    !
    !     termination, either normal or user imposed.
    !
    if (iflag < 0) info = iflag
    iflag = 0
    if (nprint > 0) call fcn(n,x,fvec,iflag)
    if (info < 0) call xerror( 'snsq   -- execution terminated because user set iflag negative.',63,1,1)
    if (info == 0) call xerror( 'snsq   -- invalid input parameter.',34,2,1)
    if (info == 2) call xerror( 'snsq   -- too many function evaluations.',40,9,1)
    if (info == 3) call xerror( 'snsq   -- xtol too small. no further improvement possible.',58,3,1)
    if (info > 4) call xerror( 'snsq   -- iteration not making good progress.',45,1,1)
    return
    !
    !     last card of subroutine snsq.
    !
    end subroutine snsq

! --------------------------- idamax -------------------------------------------

    integer function idamax(n,dx,incx)

    !***begin prologue  idamax
    !***date written   791001   (yymmdd)
    !***revision date  820801   (yymmdd)
    !***category no.  d1a2
    !***keywords  blas,real(8),linear algebra,maximum component, 
    !             vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***purpose  find largest component of d.p. vector
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s) 
    !       dx  real(8) vector with n elements
    !     incx  storage spacing between elements of dx
    !
    !     --output--
    !   idamax  smallest index (zero if n <= 0)
    !
    !     find smallest index of maximum magnitude of real(8) dx.
    !     idamax =  first i, i = 1 to n, to minimize  abs(dx(1-incx+i*incx)
    !***references  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,
    !                 *basic linear algebra subprograms for fortran usage*,
    !                 algorithm no. 539, transactions on mathematical
    !                 software, volume 5, number 3, september 1979, 308-323
    !***routines called  (none)
    !***end prologue  idamax
    !
    implicit none
    real(8) :: dx(*),dmax,xmag
    integer :: n, incx, ns, ii, i
    !***first executable statement  idamax
    idamax = 0
    if(n<=0) return
    idamax = 1
    if(n<=1)return
    if(incx==1)goto 20
    !
    !        code for increments not equal to 1.
    !
    dmax = abs(dx(1))
    ns = n*incx
    ii = 1
    do i = 1,ns,incx 
        xmag = abs(dx(i))
        if(xmag<=dmax) go to 5
        idamax = ii
        dmax = xmag
5       ii = ii + 1
    end do
    return
    !
    !        code for increments equal to 1.
    !
20  dmax = abs(dx(1))
    do i = 2,n 
        xmag = abs(dx(i))
        if(xmag<=dmax) go to 30
        idamax = i
        dmax = xmag
30      continue
    end do
    return
    end 

    real(8) function dasum(n,dx,incx)
    !***begin prologue  dasum
    !***date written   791001   (yymmdd)
    !***revision date  820801   (yymmdd)
    !***category no.  d1a3a
    !***keywords  add,blas,real(8),linear algebra,magnitude,sum,
    !             vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***purpose  sum of magnitudes of d.p. vector components
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s)
    !       dx  real(8) vector with n elements
    !     incx  storage spacing between elements of dx
    !
    !     --output--
    !    dasum  real(8) result (zero if n <= 0)
    !
    !     returns sum of magnitudes of real(8) dx.
    !     dasum = sum from 0 to n-1 of abs(dx(1+i*incx))
    !***references  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,
    !                 *basic linear algebra subprograms for fortran usage*,
    !                 algorithm no. 539, transactions on mathematical
    !                 software, volume 5, number 3, september 1979, 308-323
    !***routines called  (none)
    !***end prologue  dasum
    !
    implicit none
    real(8) :: dx(*)
    integer :: n, i, m, mp1, ns, incx
    
    !***first executable statement  dasum
    dasum = 0.d0
    if(n<=0)return
    if(incx==1)goto 20
    !
    !        code for increments not equal to 1.
    !
    ns = n*incx
    do i=1,ns,incx
        dasum = dasum + abs(dx(i))
    end do
    return
    !
    !        code for increments equal to 1.
    !
    !
    !        clean-up loop so remaining vector length is a multiple of 6.
    !
20  m = mod(n,6)
    if( m == 0 ) go to 40
    do i = 1,m
        dasum = dasum + abs(dx(i))
    end do
    if( n < 6 ) return
40  mp1 = m + 1
    do i = mp1,n,6
        dasum = dasum + abs(dx(i)) + abs(dx(i+1)) + abs(dx(i+2)) + abs(dx(i+3)) + abs(dx(i+4)) + abs(dx(i+5))
    end do
    return
    end

    subroutine daxpy(n,da,dx,incx,dy,incy)
    !***begin prologue  daxpy
    !***date written   791001   (yymmdd)
    !***revision date  820801   (yymmdd)
    !***category no.  d1a7
    !***keywords  blas,real(8),linear algebra,triad,vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***purpose  d.p computation y = a*x + y
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s) 
    !       da  real(8) scalar multiplier
    !       dx  real(8) vector with n elements
    !     incx  storage spacing between elements of dx
    !       dy  real(8) vector with n elements
    !     incy  storage spacing between elements of dy
    !
    !     --output--
    !       dy  real(8) result (unchanged if n <= 0) 
    !
    !     overwrite real(8) dy with real(8) da*dx + dy. 
    !     for i = 0 to n-1, replace  dy(ly+i*incy) with da*dx(lx+i*incx) +
    !       dy(ly+i*incy), where lx = 1 if incx >= 0, else lx = (-incx)*n
    !       and ly is defined in a similar way using incy.
    !***references  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,
    !                 *basic linear algebra subprograms for fortran usage*,
    !                 algorithm no. 539, transactions on mathematical
    !                 software, volume 5, number 3, september 1979, 308-323
    !***routines called  (none)
    !***end prologue  daxpy
    !
    implicit none
    real(8) :: dx(*), dy(*), da
    integer :: n, incx, incy, ix, iy, i, m, mp1, ns
    
    !***first executable statement  daxpy
    if(n<=0.or.da==0.d0) return
    if(incx==incy) if(incx-1) 5,20,60
5   continue
    !
    !        code for nonequal or nonpositive increments.
    !
    ix = 1
    iy = 1
    if(incx<0)ix = (-n+1)*incx + 1 
    if(incy<0)iy = (-n+1)*incy + 1 
    do i = 1,n 
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
    end do
    return
    !
    !        code for both increments equal to 1
    !
    !
    !        clean-up loop so remaining vector length is a multiple of 4. 
    !
20  m = mod(n,4)
    if( m == 0 ) go to 40 
    do i = 1,m 
        dy(i) = dy(i) + da*dx(i)
    end do
    if( n < 4 ) return
40  mp1 = m + 1
    do i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
    end do
    return
    !
    !        code for equal, positive, nonunit increments.
    !
60  continue
    ns = n*incx
    do i=1,ns,incx
        dy(i) = da*dx(i) + dy(i)
    end do
    return
    end 

    real(8) function ddot(n,dx,incx,dy,incy)

    !***begin prologue  ddot
    !***date written   791001   (yymmdd)
    !***revision date  820801   (yymmdd)
    !***category no.  d1a4
    !***keywords  blas,real(8),inner product,linear algebra,vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***purpose  d.p. inner product of d.p. vectors
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s) 
    !       dx  real(8) vector with n elements
    !     incx  storage spacing between elements of dx
    !       dy  real(8) vector with n elements
    !     incy  storage spacing between elements of dy
    !
    !     --output--
    !     ddot  real(8) dot product (zero if n <= 0) 
    !
    !     returns the dot product of real(8) dx and dy.
    !     ddot = sum for i = 0 to n-1 of  dx(lx+i*incx) * dy(ly+i*incy)
    !     where lx = 1 if incx >= 0, else lx = (-incx)*n, and ly is
    !     defined in a similar way using incy.
    !***references  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,
    !                 *basic linear algebra subprograms for fortran usage*,
    !                 algorithm no. 539, transactions on mathematical
    !                 software, volume 5, number 3, september 1979, 308-323
    !***routines called  (none)
    !***end prologue  ddot
    !
    
    implicit none
    real(8) :: dx(*),dy(*)
    integer :: n, incx, incy, ix, iy, i, m, mp1, ns
    
    !***first executable statement  ddot
    ddot = 0.d0
    if(n<=0)return
    if(incx==incy) if(incx-1) 5,20,60
5   continue
    !
    !         code for unequal or nonpositive increments.
    !
    ix = 1
    iy = 1
    if(incx<0)ix = (-n+1)*incx + 1 
    if(incy<0)iy = (-n+1)*incy + 1 
    do 10 i = 1,n 
        ddot = ddot + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
10  continue
    return
    !
    !        code for both increments equal to 1.
    !
    !
    !        clean-up loop so remaining vector length is a multiple of 5. 
    !
20  m = mod(n,5)
    if( m == 0 ) go to 40 
    do 30 i = 1,m 
        ddot = ddot + dx(i)*dy(i)
30  continue
    if( n < 5 ) return
40  mp1 = m + 1
    do i = mp1,n,5
        ddot = ddot + dx(i)*dy(i) + dx(i+1)*dy(i+1) + dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
    end do
    return
    !
    !         code for positive equal increments /=1.
    !
60  continue
    ns = n*incx
    do i=1,ns,incx
        ddot = ddot + dx(i)*dy(i)
    end do
    return
    end 

    real(8) function dnrm2(n,dx,incx)
    !***begin prologue  dnrm2
    !***date written   791001   (yymmdd)
    !***revision date  820801   (yymmdd)
    !***category no.  d1a3b
    !***keywords  blas,real(8),euclidean,l2,length,linear algebra,
    !             norm,vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***purpose  euclidean length (l2 norm) of d.p. vector
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s) 
    !       dx  real(8) vector with n elements
    !     incx  storage spacing between elements of dx
    !
    !     --output--
    !    dnrm2  real(8) result (zero if n <= 0)
    !
    !     euclidean norm of the n-vector stored in dx() with storage
    !     increment incx .
    !     if    n <= 0 return with result = 0.
    !     if n >= 1 then incx must be >= 1
    !
    !           c.l. lawson, 1978 jan 08
    !
    !     four phase method     using two built-in constants that are
    !     hopefully applicable to all machines.
    !         cutlo = maximum of  sqrt(u/eps)  over all known machines.
    !         cuthi = minimum of  sqrt(v)      over all known machines.
    !     where
    !         eps = smallest no. such that eps + 1. > 1.
    !         u   = smallest positive no.   (underflow limit)
    !         v   = largest  no.            (overflow  limit)
    !
    !     brief outline of algorithm..
    !
    !     phase 1    scans zero components. 
    !     move to phase 2 when a component is nonzero and <= cutlo
    !     move to phase 3 when a component is > cutlo
    !     move to phase 4 when a component is >= cuthi/m
    !     where m = n for x() real and m = 2*n for complex.
    !
    !     values for cutlo and cuthi..
    !     from the environmental parameters listed in the imsl converter
    !     document the limiting values are as follows..
    !     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
    !                   univac and dec at 2**(-103)
    !                   thus cutlo = 2**(-51) = 4.44089e-16
    !     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
    !                   thus cuthi = 2**(63.5) = 1.30438e19
    !     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec. 
    !                   thus cutlo = 2**(-33.5) = 8.23181d-11
    !     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
    !     data cutlo, cuthi / 8.232d-11,  1.304d19 /
    !     data cutlo, cuthi / 4.441e-16,  1.304e19 /
    !***references  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,
    !                 *basic linear algebra subprograms for fortran usage*,
    !                 algorithm no. 539, transactions on mathematical
    !                 software, volume 5, number 3, september 1979, 308-323
    !***routines called  (none)
    !***end prologue  dnrm2
    
    implicit none
    integer :: next, n, nn, incx, i, j
    real(8) :: dx(*), cutlo, cuthi, hitest, sum, xmax, zero, one
    data   zero, one /0.0d0, 1.0d0/
    !
    data cutlo, cuthi / 8.232d-11,  1.304d19 /
    !***first executable statement  dnrm2
    if(n > 0) go to 10
    dnrm2  = zero
    go to 300
    !
10  assign 30 to next
    sum = zero
    nn = n * incx 
    !                                                 begin main loop
    i = 1
20  go to next,(30, 50, 70, 110)
30  if( abs(dx(i)) > cutlo) go to 85
    assign 50 to next
    xmax = zero
    !
    !                        phase 1.  sum is zero
    !
50  if( dx(i) == zero) go to 200
    if( abs(dx(i)) > cutlo) go to 85
    !
    !                                prepare for phase 2.
    assign 70 to next
    go to 105
    !
    !                                prepare for phase 4.
    !
100 i = j
    assign 110 to next
    sum = (sum / dx(i)) / dx(i)
105 xmax = abs(dx(i))
    go to 115
    !
    !                   phase 2.  sum is small.
    !                             scale to avoid destructive underflow.
    !
70  if( abs(dx(i)) > cutlo ) go to 75
    !
    !                     common code for phases 2 and 4.
    !                     in phase 4 sum is large.  scale to avoid overflow.
    !
110 if( abs(dx(i)) <= xmax ) go to 115
    sum = one + sum * (xmax / dx(i))**2
    xmax = abs(dx(i))
    go to 200
    !
115 sum = sum + (dx(i)/xmax)**2
    go to 200
    !
    !
    !                  prepare for phase 3. 
    !
75  sum = (sum * xmax) * xmax
    !
    !
    !     for real or d.p. set hitest = cuthi/n
    !     for complex      set hitest = cuthi/(2*n)
    !
85  hitest = cuthi/dble( n )
    !
    !                   phase 3.  sum is mid-range.  no scaling.
    !
    do j =i,nn,incx
        if(abs(dx(j)) >= hitest) go to 100
        sum = sum + dx(j)**2 
    end do
    dnrm2 = sqrt( sum )
    go to 300
    !
200 continue
    i = i + incx
    if ( i <= nn ) go to 20
    !
    !              end of main loop.
    !
    !              compute square root and adjust for scaling.
    !
    dnrm2 = xmax * sqrt(sum)
300 continue
    return
    end 

    subroutine dscal(n,da,dx,incx)
    !***begin prologue  dscal
    !***date written   791001   (yymmdd)
    !***revision date  820801   (yymmdd)
    !***category no.  d1a6
    !***keywords  blas,linear algebra,scale,vector
    !***author  lawson, c. l., (jpl)
    !           hanson, r. j., (snla)
    !           kincaid, d. r., (u. of texas)
    !           krogh, f. t., (jpl)
    !***purpose  d.p. vector scale x = a*x
    !***description
    !
    !                b l a s  subprogram
    !    description of parameters
    !
    !     --input--
    !        n  number of elements in input vector(s) 
    !       da  real(8) scale factor
    !       dx  real(8) vector with n elements
    !     incx  storage spacing between elements of dx
    !
    !     --output--
    !       dx  real(8) result (unchanged if n<=0)
    !
    !     replace real(8) dx by real(8) da*dx.
    !     for i = 0 to n-1, replace dx(1+i*incx) with  da * dx(1+i*incx)
    !***references  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,
    !                 *basic linear algebra subprograms for fortran usage*,
    !                 algorithm no. 539, transactions on mathematical
    !                 software, volume 5, number 3, september 1979, 308-323
    !***routines called  (none)
    !***end prologue  dscal
    !
    
    implicit none
    real(8) :: da,dx(*)
    integer :: n, incx, i, m, mp1, ns
    
    !***first executable statement  dscal
    if(n<=0)return
    if(incx==1)goto 20
    !
    !        code for increments not equal to 1.
    !
    ns = n*incx
    do i = 1,ns,incx 
        dx(i) = da*dx(i)
    end do
    return
    !
    !        code for increments equal to 1.
    !
    !
    !        clean-up loop so remaining vector length is a multiple of 5. 
    !
20  m = mod(n,5)
    if( m == 0 ) go to 40 
    do i = 1,m 
        dx(i) = da*dx(i)
    end do
    if( n < 5 ) return
40  mp1 = m + 1
    do i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
    end do
    return
    end 
    
    subroutine dgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )
    
        
    implicit none
    !     .. scalar arguments ..
    real(8) :: alpha, beta
    integer :: incx, incy, lda, m, n
    character(1) :: trans
    !     .. array arguments ..
    real(8) :: a(lda, *), x(*), y(*)
    !     ..
    !
    !  purpose
    !  =======
    !
    !  dgemv  performs one of the matrix-vector operations
    !
    !     y := alpha*a*x + beta*y,   or   y := alpha*a'*x + beta*y,
    !
    !  where alpha and beta are scalars, x and y are vectors and a is an
    !  m by n matrix.
    !
    !  parameters
    !  ==========
    !
    !  trans  - character*1.
    !           on entry, trans specifies the operation to be performed as
    !           follows:
    !
    !              trans = 'n' or 'n'   y := alpha*a*x + beta*y.
    !
    !              trans = 't' or 't'   y := alpha*a'*x + beta*y.
    !
    !              trans = 'c' or 'c'   y := alpha*a'*x + beta*y.
    !
    !           unchanged on exit.
    !
    !  m      - integer.
    !           on entry, m specifies the number of rows of the matrix a.
    !           m must be at least zero.
    !           unchanged on exit.
    !
    !  n      - integer.
    !           on entry, n specifies the number of columns of the matrix a.
    !           n must be at least zero.
    !           unchanged on exit.
    !
    !  alpha  - real(8).
    !           on entry, alpha specifies the scalar alpha.
    !           unchanged on exit.
    !
    !  a      - real(8) array of dimension ( lda, n ).
    !           before entry, the leading m by n part of the array a must
    !           contain the matrix of coefficients.
    !           unchanged on exit.
    !
    !  lda    - integer.
    !           on entry, lda specifies the first dimension of a as declared
    !           in the calling (sub) program. lda must be at least
    !           max( 1, m ).
    !           unchanged on exit.
    !
    !  x      - real(8) array of dimension at least
    !           ( 1 + ( n - 1 )*abs( incx ) ) when trans = 'n' or 'n'
    !           and at least
    !           ( 1 + ( m - 1 )*abs( incx ) ) otherwise.
    !           before entry, the incremented array x must contain the
    !           vector x.
    !           unchanged on exit.
    !
    !  incx   - integer.
    !           on entry, incx specifies the increment for the elements of
    !           x. incx must not be zero.
    !           unchanged on exit.
    !
    !  beta   - real(8).
    !           on entry, beta specifies the scalar beta. when beta is
    !           supplied as zero then y need not be set on input.
    !           unchanged on exit.
    !
    !  y      - real(8) array of dimension at least
    !           ( 1 + ( m - 1 )*abs( incy ) ) when trans = 'n' or 'n'
    !           and at least
    !           ( 1 + ( n - 1 )*abs( incy ) ) otherwise.
    !           before entry with beta non-zero, the incremented array y
    !           must contain the vector y. on exit, y is overwritten by the
    !           updated vector y.
    !
    !  incy   - integer.
    !           on entry, incy specifies the increment for the elements of
    !           y. incy must not be zero.
    !           unchanged on exit.
    !
    !
    !  level 2 blas routine.
    !
    !  -- written on 22-october-1986.
    !     jack dongarra, argonne national lab.
    !     jeremy du croz, nag central office.
    !     sven hammarling, nag central office.
    !     richard hanson, sandia national labs.
    !
    !

    !     .. parameters ..
    real(8), parameter :: one = 1.0d+0, zero = 0.0d+0
    !     .. local scalars ..
    real(8) :: temp
    integer:: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
    !     .. external functions ..
    logical :: lsame
    external lsame
    !     .. external subroutines ..
    external xerbla
    !     .. intrinsic functions ..
    intrinsic max
    !     ..
    !     .. executable statements ..
    !
    !     test the input parameters.
    !
    
    info = 0
    if (.not.lsame(trans, 'N').and..not.lsame(trans, 'T').and..not.lsame(trans, 'C'))then
        info = 1
    else if( m<0 )then
        info = 2
    else if( n<0 )then
        info = 3
    else if( lda<max( 1, m ) )then
        info = 6
    else if( incx==0 )then
        info = 8
    else if( incy==0 )then
        info = 11
    endif
    if( info/=0 )then
        call xerbla( 'dgemv ', info )
        return
    endif
    !
    !     quick return if possible.
    !
    if( ( m==0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
    !
    !     set  lenx  and  leny, the lengths of the vectors x and y, and set
    !     up the start points in  x  and  y.
    !
    if( lsame( trans, 'N' ) )then
        lenx = n
        leny = m
    else
        lenx = m
        leny = n
    endif
    if( incx>0 )then
        kx = 1
    else
        kx = 1 - ( lenx - 1 )*incx
    endif
    if( incy>0 )then
        ky = 1
    else
        ky = 1 - ( leny - 1 )*incy
    endif
    !
    !     start the operations. in this version the elements of a are
    !     accessed sequentially with one pass through a.
    !
    !     first form  y := beta*y.
    !
    if( beta/=one )then
        if( incy==1 )then
            if( beta==zero )then
                do i = 1, leny
                    y( i ) = zero
                end do
            else
                do i = 1, leny
                    y( i ) = beta*y( i )
                end do
            endif
        else
            iy = ky
            if( beta==zero )then
                do i = 1, leny
                    y( iy ) = zero
                    iy      = iy   + incy
                end do
            else
                do i = 1, leny
                    y( iy ) = beta*y( iy )
                    iy      = iy           + incy
                end do
            endif
        endif
    endif
    if( alpha==zero ) return
    if( lsame( trans, 'N' ) )then
        !
        !        form  y := alpha*a*x + y.
        !
        jx = kx
        if( incy==1 )then
            do j = 1, n
                if( x( jx )/=zero )then
                    temp = alpha*x( jx )
                    do i = 1, m
                        y( i ) = y( i ) + temp*a( i, j )
                    end do
                endif
                jx = jx + incx
            end do
        else
            do j = 1, n
                if( x( jx )/=zero )then
                    temp = alpha*x( jx )
                    iy   = ky
                    do i = 1, m
                        y( iy ) = y( iy ) + temp*a( i, j )
                        iy      = iy      + incy
                    end do
                endif
                jx = jx + incx
            end do
        endif
    else
        !
        !        form  y := alpha*a'*x + y.
        !
        jy = ky
        if( incx==1 )then
            do j = 1, n
                temp = zero
                do i = 1, m
                    temp = temp + a( i, j )*x( i )
                end do
                y( jy ) = y( jy ) + alpha*temp
                jy      = jy      + incy
            end do
        else
            do j = 1, n
                temp = zero
                ix   = kx
                do i = 1, m
                    temp = temp + a( i, j )*x( ix )
                    ix   = ix   + incx
                end do
                y( jy ) = y( jy ) + alpha*temp
                jy      = jy      + incy
            end do
        endif
    endif
    !
    return
    !
    !     end of dgemv .
    !
    end
    
    subroutine dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb,beta, c, ldc )
    
    implicit none
    !     .. scalar arguments ..
    character(1) :: transa, transb
    integer :: m, n, k, lda, ldb, ldc
    real(8) :: alpha, beta
    !     .. array arguments ..
    real(8) :: a(lda, *), b(ldb, *), c(ldc, *)
    !     ..
    !
    !  purpose
    !  =======
    !
    !  dgemm  performs one of the matrix-matrix operations
    !
    !     c := alpha*op( a )*op( b ) + beta*c,
    !
    !  where  op( x ) is one of
    !
    !     op( x ) = x   or   op( x ) = x',
    !
    !  alpha and beta are scalars, and a, b and c are matrices, with op( a )
    !  an m by k matrix,  op( b )  a  k by n matrix and  c an m by n matrix.
    !
    !  parameters
    !  ==========
    !
    !  transa - character*1.
    !           on entry, transa specifies the form of op( a ) to be used in
    !           the matrix multiplication as follows:
    !
    !              transa = 'n' or 'n',  op( a ) = a.
    !
    !              transa = 't' or 't',  op( a ) = a'.
    !
    !              transa = 'c' or 'c',  op( a ) = a'.
    !
    !           unchanged on exit.
    !
    !  transb - character*1.
    !           on entry, transb specifies the form of op( b ) to be used in
    !           the matrix multiplication as follows:
    !
    !              transb = 'n' or 'n',  op( b ) = b.
    !
    !              transb = 't' or 't',  op( b ) = b'.
    !
    !              transb = 'c' or 'c',  op( b ) = b'.
    !
    !           unchanged on exit.
    !
    !  m      - integer.
    !           on entry,  m  specifies  the number  of rows  of the  matrix
    !           op( a )  and of the  matrix  c.  m  must  be at least  zero.
    !           unchanged on exit.
    !
    !  n      - integer.
    !           on entry,  n  specifies the number  of columns of the matrix
    !           op( b ) and the number of columns of the matrix c. n must be
    !           at least zero.
    !           unchanged on exit.
    !
    !  k      - integer.
    !           on entry,  k  specifies  the number of columns of the matrix
    !           op( a ) and the number of rows of the matrix op( b ). k must
    !           be at least  zero.
    !           unchanged on exit.
    !
    !  alpha  - real(8).
    !           on entry, alpha specifies the scalar alpha.
    !           unchanged on exit.
    !
    !  a      - real(8) array of dimension ( lda, ka ), where ka is
    !           k  when  transa = 'n' or 'n',  and is  m  otherwise.
    !           before entry with  transa = 'n' or 'n',  the leading  m by k
    !           part of the array  a  must contain the matrix  a,  otherwise
    !           the leading  k by m  part of the array  a  must contain  the
    !           matrix a.
    !           unchanged on exit.
    !
    !  lda    - integer.
    !           on entry, lda specifies the first dimension of a as declared
    !           in the calling (sub) program. when  transa = 'n' or 'n' then
    !           lda must be at least  max( 1, m ), otherwise  lda must be at
    !           least  max( 1, k ).
    !           unchanged on exit.
    !
    !  b      - real(8) array of dimension ( ldb, kb ), where kb is
    !           n  when  transb = 'n' or 'n',  and is  k  otherwise.
    !           before entry with  transb = 'n' or 'n',  the leading  k by n
    !           part of the array  b  must contain the matrix  b,  otherwise
    !           the leading  n by k  part of the array  b  must contain  the
    !           matrix b.
    !           unchanged on exit.
    !
    !  ldb    - integer.
    !           on entry, ldb specifies the first dimension of b as declared
    !           in the calling (sub) program. when  transb = 'n' or 'n' then
    !           ldb must be at least  max( 1, k ), otherwise  ldb must be at
    !           least  max( 1, n ).
    !           unchanged on exit.
    !
    !  beta   - real(8).
    !           on entry,  beta  specifies the scalar  beta.  when  beta  is
    !           supplied as zero then c need not be set on input.
    !           unchanged on exit.
    !
    !  c      - real(8) array of dimension ( ldc, n ).
    !           before entry, the leading  m by n  part of the array  c must
    !           contain the matrix  c,  except when  beta  is zero, in which
    !           case c need not be set on entry.
    !           on exit, the array  c  is overwritten by the  m by n  matrix
    !           ( alpha*op( a )*op( b ) + beta*c ).
    !
    !  ldc    - integer.
    !           on entry, ldc specifies the first dimension of c as declared
    !           in  the  calling  (sub)  program.   ldc  must  be  at  least
    !           max( 1, m ).
    !           unchanged on exit.
    !
    !
    !  level 3 blas routine.
    !
    !  -- written on 8-february-1989.
    !     jack dongarra, argonne national laboratory.
    !     iain duff, aere harwell.
    !     jeremy du croz, numerical algorithms group ltd.
    !     sven hammarling, numerical algorithms group ltd.
    !
    !
    !     .. external functions ..
    logical :: lsame
    external lsame
    !     .. external subroutines ..
    external xerbla
    !     .. intrinsic functions ..
    intrinsic max
    !     .. local scalars ..
    logical :: nota, notb
    integer :: i, info, j, l, ncola, nrowa, nrowb
    real(8) :: temp
    !     .. parameters ..
    real(8), parameter :: one = 1.0d+0, zero = 0.0d+0
    !     ..
    !     .. executable statements ..
    !
    !     set  nota  and  notb  as  true if  a  and  b  respectively are not
    !     transposed and set  nrowa, ncola and  nrowb  as the number of rows
    !     and  columns of  a  and the  number of  rows  of  b  respectively.
    !
    nota  = lsame( transa, 'N' )
    notb  = lsame( transb, 'N' )
    if( nota )then
        nrowa = m
        ncola = k
    else
        nrowa = k
        ncola = m
    endif
    if( notb )then
        nrowb = k
    else
        nrowb = n
    endif
    !
    !     test the input parameters.
    !
    info = 0
    if((.not.nota).and.(.not.lsame(transa,'C')).and.(.not.lsame(transa,'T')))then
        info = 1
    elseif((.not.notb).and.(.not.lsame(transb,'C')).and.(.not.lsame(transb,'T')))then
        info = 2
    else if( m  <0 )then
        info = 3
    else if( n  <0 )then
        info = 4
    else if( k  <0 )then
        info = 5
    else if( lda<max( 1, nrowa ) )then
        info = 8
    else if( ldb<max( 1, nrowb ) )then
        info = 10
    else if( ldc<max( 1, m     ) )then
        info = 13
    endif
    if( info/=0 )then
        call xerbla( 'dgemm ', info )
        return
    endif
    !
    !     quick return if possible.
    !
    if((m==0).or.(n==0).or.(((alpha==zero).or.(k==0)).and.(beta==one))) return
    !
    !     and if  alpha==zero.
    !
    if( alpha==zero )then
        if( beta==zero )then
            do j = 1, n
                do i = 1, m
                    c( i, j ) = zero
                end do
            end do
        else
            do j = 1, n
                do i = 1, m
                    c( i, j ) = beta*c( i, j )
                end do
            end do
        endif
        return
    endif
    !
    !     start the operations.
    !
    if( notb )then
        if( nota )then
            !
            !           form  c := alpha*a*b + beta*c.
            !
            do j = 1, n
                if( beta==zero )then
                    do i = 1, m
                        c( i, j ) = zero
                    end do
                else if( beta/=one )then
                    do i = 1, m
                        c( i, j ) = beta*c( i, j )
                    end do
                endif
                do l = 1, k
                    if( b( l, j )/=zero )then
                        temp = alpha*b( l, j )
                        do i = 1, m
                            c( i, j ) = c( i, j ) + temp*a( i, l )
                        end do
                    endif
                end do
            end do
        else
            !
            !           form  c := alpha*a'*b + beta*c
            !
            do j = 1, n
                do i = 1, m
                    temp = zero
                    do l = 1, k
                        temp = temp + a( l, i )*b( l, j )
                    end do
                    if( beta==zero )then
                        c( i, j ) = alpha*temp
                    else
                        c( i, j ) = alpha*temp + beta*c( i, j )
                    endif
               end do
            end do
        endif
    else
        if( nota )then
            !
            !           form  c := alpha*a*b' + beta*c
            !
            do j = 1, n
                if( beta==zero )then
                    do i = 1, m
                        c( i, j ) = zero
                    end do
                else if( beta/=one )then
                    do i = 1, m
                        c( i, j ) = beta*c( i, j )
                    end do
                endif
                do l = 1, k
                    if( b( j, l )/=zero )then
                        temp = alpha*b( j, l )
                        do i = 1, m
                            c( i, j ) = c( i, j ) + temp*a( i, l )
                        end do
                    endif
                end do
            end do
        else
            !
            !           form  c := alpha*a'*b' + beta*c
            !
            do j = 1, n
                do i = 1, m
                    temp = zero
                    do l = 1, k
                        temp = temp + a( l, i )*b( j, l )
                    end do
                    if( beta==zero )then
                        c( i, j ) = alpha*temp
                    else
                        c( i, j ) = alpha*temp + beta*c( i, j )
                    endif
                end do
            end do
        endif
    endif
    !
    return
    !
    !     end of dgemm .
    !
    end

    subroutine dgefa(a,lda,n,ipvt,info)
    
    implicit none
    integer :: lda, n, ipvt(*), info
    real(8) :: a(lda,*)
    !
    !     dgefa factors a real(8) matrix by gaussian elimination.
    !
    !     dgefa is usually called by dgeco, but it can be called
    !     directly with a saving in time if  rcond  is not needed.
    !     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
    !
    !     on entry
    !
    !        a       real(8)(lda, n)
    !                the matrix to be factored.
    !
    !        lda     integer
    !                the leading dimension of the array  a .
    !
    !        n       integer
    !                the order of the matrix  a .
    !
    !     on return
    !
    !        a       an upper triangular matrix and the multipliers
    !                which were used to obtain it.
    !                the factorization can be written  a = l*u  where
    !                l  is a product of permutation and unit lower
    !                triangular matrices and  u  is upper triangular.
    !
    !        ipvt    integer(n)
    !                an integer vector of pivot indices.
    !
    !        info    integer
    !                = 0  normal value.
    !                = k  if  u(k,k) == 0.0 .  this is not an error
    !                     condition for this subroutine, but it does
    !                     indicate that dgesl or dgedi will divide by zero
    !                     if called.  use  rcond  in dgeco for a reliable
    !                     indication of singularity.
    !
    !     linpack. this version dated 08/14/78 .
    !     cleve moler, university of new mexico, argonne national lab.
    !
    !     subroutines and functions
    !
    !     blas daxpy,dscal,idamax
    !
    !     internal variables
    !
    real(8) :: t
    integer :: idamax, j, k, kp1, l, nm1
    !
    !
    !     gaussian elimination with partial pivoting
    !
    info = 0
    nm1 = n - 1
    if (nm1>=1) then
        do k = 1, nm1
            kp1 = k + 1
            !
            !        find l = pivot index
            !
            l = idamax(n-k+1,a(k,k),1) + k - 1
            ipvt(k) = l
            !
            !        zero pivot implies this column already triangularized
            !
            if (a(l,k)/=0.0d0) then
                !
                !           interchange if necessary
                !
                if (l/=k) then
                    t = a(l,k)
                    a(l,k) = a(k,k)
                    a(k,k) = t
                endif
                !
                !           compute multipliers
                !
                t = -1.0d0 / a(k,k)
                call dscal(n-k,t,a(k+1,k),1)
                !
                !           row elimination with column indexing
                !
                do j = kp1, n
                    t = a(l,j)
                    if (l/=k) then
                        a(l,j) = a(k,j)
                        a(k,j) = t
                    endif
                    call daxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
                end do
            else
                info = k
            endif
        end do
    endif
    ipvt(n) = n
    if (a(n,n)==0.0d0) info = n
    return
    end
    
    subroutine dgesl(a,lda,n,ipvt,b,job)
    
    implicit none
    integer :: lda, n, ipvt(*), job
    real(8) :: a(lda,*), b(*)
    !
    !     dgesl solves the real(8) system
    !     a * x = b  or  trans(a) * x = b
    !     using the factors computed by dgeco or dgefa.
    !
    !     on entry
    !
    !        a       real(8)(lda, n)
    !                the output from dgeco or dgefa.
    !
    !        lda     integer
    !                the leading dimension of the array  a .
    !
    !        n       integer
    !                the order of the matrix  a .
    !
    !        ipvt    integer(n)
    !                the pivot vector from dgeco or dgefa.
    !
    !        b       real(8)(n)
    !                the right hand side vector.
    !
    !        job     integer
    !                = 0         to solve  a*x = b ,
    !                = nonzero   to solve  trans(a)*x = b  where
    !                            trans(a)  is the transpose.
    !
    !     on return
    !
    !        b       the solution vector  x .
    !
    !     error condition
    !
    !        a division by zero will occur if the input factor contains a
    !        zero on the diagonal.  technically this indicates singularity
    !        but it is often caused by improper arguments or improper
    !        setting of lda .  it will not occur if the subroutines are
    !        called correctly and if dgeco has set rcond > 0.0
    !        or dgefa has set info == 0 .
    !
    !     to compute  inverse(a) * c  where  c  is a matrix
    !     with  p  columns
    !           call dgeco(a,lda,n,ipvt,rcond,z)
    !           if (rcond is too small) go to ...
    !           do 10 j = 1, p
    !              call dgesl(a,lda,n,ipvt,c(1,j),0)
    !        10 continue
    !
    !     linpack. this version dated 08/14/78 .
    !     cleve moler, university of new mexico, argonne national lab.
    !
    !     subroutines and functions
    !
    !     blas daxpy,ddot
    !
    !     internal variables
    !
    real(8) :: ddot, t
    integer :: k, kb, l, nm1
    !
    nm1 = n - 1
    if (job==0) then
        !
        !        job = 0 , solve  a * x = b
        !        first solve  l*y = b
        !
        if (nm1>=1) then
            do k = 1, nm1
                l = ipvt(k)
                t = b(l)
                if (l/=k) then
                    b(l) = b(k)
                    b(k) = t
                endif
                call daxpy(n-k,t,a(k+1,k),1,b(k+1),1)
            end do
        endif
        !
        !        now solve  u*x = y
        !
        do kb = 1, n
            k = n + 1 - kb
            b(k) = b(k) / a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
        end do
    else
        !
        !        job = nonzero, solve  trans(a) * x = b
        !        first solve  trans(u)*y = b
        !
        do k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k)-t) / a(k,k)
        end do
        !
        !        now solve trans(l)*x = y
        !
        if (nm1>=1) then
            do kb = 1, nm1
                k = n - kb
                b(k) = b(k) + ddot(n-k,a(k+1,k),1,b(k+1),1)
                l = ipvt(k)
                if (l/=k) then
                    t = b(l)
                    b(l) = b(k)
                    b(k) = t
                endif
            end do
        endif
    endif
    return
    end
    
    subroutine dgbfa(abd,lda,n,ml,mu,ipvt,info)
    implicit none
    integer :: lda, n, ml, mu, ipvt(*), info
    real(8) :: abd(lda,*)
    !
    !     dgbfa factors a real(8) band matrix by elimination.
    !
    !     dgbfa is usually called by dgbco, but it can be called
    !     directly with a saving in time if  rcond  is not needed.
    !
    !     on entry
    !
    !        abd     real(8)(lda, n)
    !                contains the matrix in band storage.  the columns
    !                of the matrix are stored in the columns of  abd  and
    !                the diagonals of the matrix are stored in rows
    !                ml+1 through 2*ml+mu+1 of  abd .
    !                see the comments below for details.
    !
    !        lda     integer
    !                the leading dimension of the array  abd .
    !                lda must be >= 2*ml + mu + 1 .
    !
    !        n       integer
    !                the order of the original matrix.
    !
    !        ml      integer
    !                number of diagonals below the main diagonal.
    !                0 <= ml < n .
    !
    !        mu      integer
    !                number of diagonals above the main diagonal.
    !                0 <= mu < n .
    !                more efficient if  ml <= mu .
    !     on return
    !
    !        abd     an upper triangular matrix in band storage and
    !                the multipliers which were used to obtain it.
    !                the factorization can be written  a = l*u  where
    !                l  is a product of permutation and unit lower
    !                triangular matrices and  u  is upper triangular.
    !
    !        ipvt    integer(n)
    !                an integer vector of pivot indices.
    !
    !        info    integer
    !                = 0  normal value.
    !                = k  if  u(k,k) == 0.0 .  this is not an error
    !                     condition for this subroutine, but it does
    !                     indicate that dgbsl will divide by zero if
    !                     called.  use  rcond  in dgbco for a reliable
    !                     indication of singularity.
    !
    !     band storage
    !
    !           if  a  is a band matrix, the following program segment
    !           will set up the input.
    !
    !                   ml = (band width below the diagonal)
    !                   mu = (band width above the diagonal)
    !                   m = ml + mu + 1
    !                   do 20 j = 1, n
    !                      i1 = max0(1, j-mu)
    !                      i2 = min0(n, j+ml)
    !                      do 10 i = i1, i2
    !                         k = i - j + m
    !                         abd(k,j) = a(i,j)
    !                10    continue
    !                20 continue
    !
    !           this uses rows  ml+1  through  2*ml+mu+1  of  abd .
    !           in addition, the first  ml  rows in  abd  are used for
    !           elements generated during the triangularization.
    !           the total number of rows needed in  abd  is  2*ml+mu+1 .
    !           the  ml+mu by ml+mu  upper left triangle and the
    !           ml by ml  lower right triangle are not referenced.
    !
    !     linpack. this version dated 08/14/78 .
    !     cleve moler, university of new mexico, argonne national lab.
    !
    !     subroutines and functions
    !
    !     blas daxpy,dscal,idamax
    !     fortran max0,min0
    !
    !     internal variables
    !
    real(8) :: t
    integer :: i, idamax, i0, j, ju, jz, j0, j1, k, kp1, l, lm, m, mm, nm1
    !
    !
    m = ml + mu + 1
    info = 0
    !
    !     zero initial fill-in columns
    !
    j0 = mu + 2
    j1 = min0(n,m) - 1
    if (j1>=j0) then
        do jz = j0, j1
            i0 = m + 1 - jz
            do i = i0, ml
                abd(i,jz) = 0.0d0
            end do
        end do
    endif
    jz = j1
    ju = 0
    !
    !     gaussian elimination with partial pivoting
    !
    nm1 = n - 1
    if (nm1>=1) then
        do k = 1, nm1
            kp1 = k + 1
            !
            !        zero next fill-in column
            !
            jz = jz + 1
            if (jz<=n) then
                if (ml>=1) then
                    do i = 1, ml
                        abd(i,jz) = 0.0d0
                    end do
                endif
            endif
            !
            !        find l = pivot index
            !
            lm = min0(ml,n-k)
            l = idamax(lm+1,abd(m,k),1) + m - 1
            ipvt(k) = l + k - m
            !
            !        zero pivot implies this column already triangularized
            !
            if (abd(l,k)/=0.0d0) then
                !
                !           interchange if necessary
                !
                if (l/=m) then
                    t = abd(l,k)
                    abd(l,k) = abd(m,k)
                    abd(m,k) = t
                endif
                !
                !           compute multipliers
                !
                t = -1.0d0 / abd(m,k)
                call dscal(lm,t,abd(m+1,k),1)
                !
                !           row elimination with column indexing
                !
                ju = min0(max0(ju,mu+ipvt(k)),n)
                mm = m
                if (ju>=kp1) then
                    do j = kp1, ju
                        l = l - 1
                        mm = mm - 1
                        t = abd(l,j)
                        if (l/=mm) then
                            abd(l,j) = abd(mm,j)
                            abd(mm,j) = t
                        endif
                        call daxpy(lm,t,abd(m+1,k),1,abd(mm+1,j),1)
                    end do
                endif
            else
                info = k
            endif
        end do
    endif
    ipvt(n) = n
    if (abd(m,n)==0.0d0) info = n
    return
    end
    
    subroutine dgbsl(abd,lda,n,ml,mu,ipvt,b,job)
    
    implicit none
    integer :: lda, n, ml, mu, ipvt(*), job
    real(8) :: abd(lda,*), b(*)
    !
    !     dgbsl solves the real(8) band system
    !     a * x = b  or  trans(a) * x = b
    !     using the factors computed by dgbco or dgbfa.
    !
    !     on entry
    !
    !        abd     real(8)(lda, n)
    !                the output from dgbco or dgbfa.
    !
    !        lda     integer
    !                the leading dimension of the array  abd .
    !
    !        n       integer
    !                the order of the original matrix.
    !
    !        ml      integer
    !                number of diagonals below the main diagonal.
    !
    !        mu      integer
    !                number of diagonals above the main diagonal.
    !
    !        ipvt    integer(n)
    !                the pivot vector from dgbco or dgbfa.
    !
    !        b       real(8)(n)
    !                the right hand side vector.
    !
    !        job     integer
    !                = 0         to solve  a*x = b ,
    !                = nonzero   to solve  trans(a)*x = b , where
    !                            trans(a)  is the transpose.
    !
    !     on return
    !
    !        b       the solution vector  x .
    !
    !     error condition
    !
    !        a division by zero will occur if the input factor contains a
    !        zero on the diagonal.  technically this indicates singularity
    !        but it is often caused by improper arguments or improper
    !        setting of lda .  it will not occur if the subroutines are
    !        called correctly and if dgbco has set rcond > 0.0
    !        or dgbfa has set info == 0 .
    !
    !     to compute  inverse(a) * c  where  c  is a matrix
    !     with  p  columns
    !           call dgbco(abd,lda,n,ml,mu,ipvt,rcond,z)
    !           if (rcond is too small) go to ...
    !           do 10 j = 1, p
    !              call dgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0)
    !        10 continue
    !
    !     linpack. this version dated 08/14/78 .
    !     cleve moler, university of new mexico, argonne national lab.
    !
    !     subroutines and functions
    !
    !     blas daxpy,ddot
    !     fortran min0
    !
    !     internal variables
    !
    real(8) :: ddot, t
    integer :: k, kb, l, la, lb, lm, m, nm1
    !
    m = mu + ml + 1
    nm1 = n - 1
    if (job==0) then
        !
        !        job = 0 , solve  a * x = b
        !        first solve l*y = b
        !
        if (ml/=0) then
            if (nm1>=1) then
                do k = 1, nm1
                    lm = min0(ml,n-k)
                    l = ipvt(k)
                    t = b(l)
                    if (l/=k) then
                        b(l) = b(k)
                        b(k) = t
                    endif
                    call daxpy(lm,t,abd(m+1,k),1,b(k+1),1)
                end do
            endif
        endif
        !
        !        now solve  u*x = y
        !
        do kb = 1, n
            k = n + 1 - kb
            b(k) = b(k) / abd(m,k)
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = -b(k)
            call daxpy(lm,t,abd(la,k),1,b(lb),1)
        end do
    else
        !
        !        job = nonzero, solve  trans(a) * x = b
        !        first solve  trans(u)*y = b
        !
        do k = 1, n
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = ddot(lm,abd(la,k),1,b(lb),1)
            b(k) = (b(k)-t) / abd(m,k)
        end do
        !
        !        now solve trans(l)*x = y
        !
        if (ml/=0) then
            if (nm1>=1) then
                do kb = 1, nm1
                    k = n - kb
                    lm = min0(ml,n-k)
                    b(k) = b(k) + ddot(lm,abd(m+1,k),1,b(k+1),1)
                    l = ipvt(k)
                    if (l/=k) then
                        t = b(l)
                        b(l) = b(k)
                        b(k) = t
                    endif
                end do
            endif
        endif
    endif
    return
    end

    subroutine gjac
    
    implicit none

    !--------------------------------- nist/bfrl ---------------------------------
    !
    !     routine:     gjac
    !
    !     source file: jacs.sor
    !
    !     functional class:  
    !
    !     description:  dummy routine to keep the linker happy
    !
    !     arguments: 
    !
    !     revision history:
    !        created:  12/2/1992 at 11:28 by par
    !
    !---------------------------- all rights reserved ----------------------------

    !     dummy routine to keep the linker happy

    stop 'internal error in dassl - gjac not instantiated'
    end

    subroutine jac
    
    implicit none

    !--------------------------------- nist/bfrl ---------------------------------
    !
    !     routine:     jac
    !
    !     source file: jacs.sor
    !
    !     functional class:  
    !
    !     description:  dummy routine to keep the linker happy
    !
    !     arguments: 
    !
    !     revision history:
    !        created:  12/2/1992 at 11:28 by par
    !
    !---------------------------- all rights reserved ----------------------------


    !     dummy routine to keep the linker happy

    stop 'internal error in dassl - jac not instantiated'
    end

    integer function jacd()

    !--------------------------------- nist/bfrl ---------------------------------
    !
    !     routine:     jacd
    !
    !     source file: jacs.sor
    !
    !     functional class:  
    !
    !     description:  used by dassl to determine how many columns of the
    !                   jacobian are going to be computed the "hard way".  
    !                   this method avoids adding cfast common blocks to dassl.
    !
    !     arguments: 
    !
    !     revision history:
    !        created:  2/4/1993 at 17:13 by gpf
    !
    !---------------------------- all rights reserved ----------------------------

    use wdervs
    implicit none

    jacd = jacdim
    return
    end function jacd

! --------------------------- setderv -------------------------------------------

    subroutine setderv(j)
    !
    !--------------------------------- nist/bfrl ---------------------------------
    !
    !     routine:     setderv
    !
    !     source file: jacs.sor
    !
    !     functional class:  
    !
    !     description: this routine sets the value of jaccol for use by dassl. 
    !                                                                   
    !     arguments: j       value to be copied into jaccol
    !
    !     revision history:
    !        created:  2/14/1993 by gpf
    !        modified: 2/2/1995 by gpf  removed iderv set option
    !
    !---------------------------- all rights reserved ----------------------------

    use cenviro
    implicit none
    integer :: j
    !
    if(j>-10) jaccol = j
    return
    end

! --------------------------- incjac -------------------------------------------

    subroutine incjac
    !
    !--------------------------------- nist/bfrl ---------------------------------
    !
    !     routine:     injac
    !
    !     source file: jacs.sor
    !
    !     functional class:  
    !
    !     description:  used to increment the jacobian counter from within
    !                   dassl.  this method avoids adding cfast common blocks 
    !                   to dassl.
    !
    !
    !     revision history:
    !        created:  2/14/1993 by gpf
    !
    !---------------------------- all rights reserved ----------------------------

    use cparams
    use opt
    implicit none
    !
    numjac = numjac + 1
    return
    end

! --------------------------- rev_numerics -------------------------------------------

    integer function rev_numerics ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision$'
    character(255), parameter :: maindate='$Date$'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_numerics = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_numerics