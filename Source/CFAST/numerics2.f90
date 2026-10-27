module numerics_routines

  use iso_fortran_env, only: error_unit
  use utility_routines, only: d1mach
  use setup_data, only: iofill

  implicit none

  private

  public :: ddassl, ddot, dnrm2, dgefa, dgesl, jac, snsqe, gjac, dscal

contains

  subroutine ddassl(res, neq, t, y, yprime, tout, info, rtol, atol, idid, rwork, lrw, iwork, liw, rpar, ipar, jac)
    interface
      subroutine res(t, y, yprime, delta, ires, rpar, ipar)
        real(8), intent(in) :: t
        real(8), intent(in) :: y(*)
        real(8), intent(in) :: yprime(*)
        real(8), intent(out) :: delta(*)
        integer, intent(inout) :: ires
        real(8), intent(in) :: rpar(*)
        integer, intent(in) :: ipar(*)
      end subroutine res
    end interface

    procedure(jac_proc), pointer :: jacptr

    integer, intent(in) :: neq
    real(8), intent(inout) :: t
    real(8), intent(inout) :: y(neq)
    real(8), intent(inout) :: yprime(neq)
    real(8), intent(in) :: tout
    integer, intent(inout) :: info(15)
    real(8), intent(inout) :: rtol(*)
    real(8), intent(inout) :: atol(*)
    integer, intent(out) :: idid
    real(8), intent(inout) :: rwork(*)
    integer, intent(in) :: lrw
    integer, intent(inout) :: iwork(*)
    integer, intent(in) :: liw
    real(8), intent(in) :: rpar(*)
    integer, intent(in) :: ipar(*)
    external :: jac

    integer :: i, mxord, lenpd, lenrw, leniw, mband, msave, jacdim, nzflg
    integer :: lml, lmu, lmxord, lmtype, lnst, lnre, lnje, lnpd, liwm
    integer :: lmlval, lmaval, itemp, neq2
    real(8) :: rtoli, atoli, ho, hmin, hmax, tdist, rh, uround, ypnorm, tstop
    real(8) :: h, tn
    logical :: initial_call, use_banded, valid_info

    lml = 1
    lmu = 2
    lmxord = 3
    lmtype = 4
    lnst = 11
    lnre = 12
    lnje = 13
    lnpd = 16
    liwm = 1

    initial_call = (info(1) == 0)
    valid_info = .true.

    if (initial_call) then
       do i = 2, 11
          if (i == 5) then
             if (info(5) /= 0 .and. info(5) /= 1 .and. info(5) /= 2) valid_info = .false.
          else
             if (info(i) /= 0 .and. info(i) /= 1) valid_info = .false.
          end if
          if (.not. valid_info) then
             write(error_unit,'(A)') 'ddassl: invalid info array entry.'
             idid = -33
             return
          end if
       end do

       if (neq <= 0) then
          write(error_unit,'(A)') 'ddassl: neq must be positive.'
          idid = -33
          return
       end if

       mxord = 5
       if (info(9) /= 0) then
          mxord = iwork(3)
          if (mxord < 1 .or. mxord > 5) then
             write(error_unit,'(A)') 'ddassl: max order out of range.'
             idid = -33
             return
          end if
       end if
       iwork(3) = mxord

       if (info(6) == 0) then
          lenpd = neq * neq
          lenrw = 40 + (iwork(3) + 4) * neq + lenpd
          if (info(5) /= 0) then
             iwork(4) = 2
          else
             if (info(5) == 1) then
                iwork(4) = 1
             else
                iwork(4) = 6
                jacdim = 0
                lenpd = jacdim * jacdim
                lenrw = 40 + (iwork(3) + 4) * neq + lenpd
             end if
          end if
       else
          if (iwork(1) < 0 .or. iwork(1) >= neq) then
             write(error_unit,'(A)') 'ddassl: invalid lower bandwidth.'
             idid = -33
             return
          end if
          if (iwork(2) < 0 .or. iwork(2) >= neq) then
             write(error_unit,'(A)') 'ddassl: invalid upper bandwidth.'
             idid = -33
             return
          end if
          lenpd = (2 * iwork(1) + iwork(2) + 1) * neq
          if (info(5) == 0) then
             iwork(4) = 5
             mband = iwork(1) + iwork(2) + 1
             msave = (neq / mband) + 1
             lenrw = 40 + (iwork(3) + 4) * neq + lenpd + 2 * msave
          else
             iwork(4) = 4
             lenrw = 40 + (iwork(3) + 4) * neq + lenpd
          end if
       end if

       leniw = 20 + neq
       iwork(16) = lenpd
       if (lrw < lenrw) then
          write(error_unit,'(A)') 'ddassl: rwork too small.'
          idid = -33
          return
       end if
       if (liw < leniw) then
          write(error_unit,'(A)') 'ddassl: iwork too small.'
          idid = -33
          return
       end if

       if (tout == t) then
          write(error_unit,'(A)') 'ddassl: tout must differ from t.'
          idid = -33
          return
       end if

       if (info(7) /= 0) then
          hmax = rwork(2)
          if (hmax <= 0.0d0) then
             write(error_unit,'(A)') 'ddassl: hmax must be positive.'
             idid = -33
             return
          end if
       end if

       iwork(11) = 0
       iwork(12) = 0
       iwork(13) = 0
       iwork(14) = 0
       iwork(15) = 0
       iwork(10) = 0
       idid = 1
    else
       if (info(1) == -1) then
          write(error_unit,'(A)') 'ddassl: fatal error, previous step was interrupted without recovery.'
          idid = -33
          return
       end if
       iwork(10) = iwork(11)
    end if

    nzflg = 0
    rtoli = rtol(1)
    atoli = atol(1)
    do i = 1, neq
       if (info(2) == 1) then
          rtoli = rtol(i)
          atoli = atol(i)
       end if
       if (rtoli > 0.0d0 .or. atoli > 0.0d0) nzflg = 1
       if (rtoli < 0.0d0) then
          write(error_unit,'(A)') 'ddassl: rtol must be nonnegative.'
          idid = -33
          return
       end if
       if (atoli < 0.0d0) then
          write(error_unit,'(A)') 'ddassl: atol must be nonnegative.'
          idid = -33
          return
       end if
    end do
    if (nzflg == 0) then
       write(error_unit,'(A)') 'ddassl: at least one tolerance must be positive.'
       idid = -33
       return
    end if

    if (info(1) == 1) then
       uround = rwork(9)
       tn = rwork(4)
       h = rwork(3)
    else
       tn = t
       idid = 1
    end if

    if (info(1) /= 1) then
       call ddawts(neq, info(2), rtol, atol, y, rwork(41))
       do i = 1, neq
          if (rwork(41 + i - 1) <= 0.0d0) then
             write(error_unit,'(A)') 'ddassl: nonpositive error weight encountered.'
             idid = -33
             return
          end if
       end do

       uround = d1mach(4)
       rwork(9) = uround
       hmin = 4.0d0 * uround * max(abs(t), abs(tout))
       tdist = abs(tout - t)
       if (tdist < hmin) then
          write(error_unit,'(A)') 'ddassl: initial interval is too short.'
          idid = -33
          return
       end if

       if (info(8) /= 0) then
          ho = rwork(3)
          if ((tout - t) * ho < 0.0d0) then
             write(error_unit,'(A)') 'ddassl: initial step sign check failed.'
             idid = -33
             return
          end if
          if (ho == 0.0d0) then
             write(error_unit,'(A)') 'ddassl: initial step is zero.'
             idid = -33
             return
          end if
       else
          ho = 0.001d0 * tdist
          ypnorm = ddanrm(neq, yprime, rwork(41), ipar)
          if (ypnorm > 0.5d0 / ho) ho = 0.5d0 / ypnorm
          ho = sign(ho, tout - t)
       end if

       if (info(7) /= 0) then
          rh = abs(ho) / rwork(2)
          if (rh > 1.0d0) ho = ho / rh
       end if

       if (info(4) /= 0) then
          tstop = rwork(1)
          if ((tstop - t) * ho < 0.0d0) then
             write(error_unit,'(A)') 'ddassl: requested tstop is behind current time.'
             idid = -33
             return
          end if
          if ((t + ho - tstop) * ho > 0.0d0) ho = tstop - t
          if ((tstop - tout) * ho < 0.0d0) then
             write(error_unit,'(A)') 'ddassl: tout lies beyond tstop.'
             idid = -33
             return
          end if
       end if

       if (info(11) /= 0) then
          call ddaini(tn, y, yprime, neq, res, jac, ho, rwork(41), idid, rpar, ipar, rwork(41), rwork(41), rwork(41), rwork(41), &
               iwork(1), hmin, rwork(9), info(10), 1)
          if (idid < 0) then
             return
          end if
       end if

       h = ho
       rwork(3) = h
       do i = 1, neq
          rwork(41 + i - 1) = y(i)
          rwork(41 + neq + i - 1) = h * yprime(i)
       end do
    end if

    if (t == tout) then
       write(error_unit,'(A)') 'ddassl: integration step has no range.'
       idid = -33
       return
    end if

    if ((t - tout) * h > 0.0d0) then
       write(error_unit,'(A)') 'ddassl: step direction is inconsistent with tout.'
       idid = -33
       return
    end if

    if (info(4) == 1) then
       if ((t - tout) * h > 0.0d0) then
          write(error_unit,'(A)') 'ddassl: step would cross tstop.'
          idid = -33
          return
       end if
    end if

    if (info(3) == 1) then
       idid = 1
       return
    end if

    if ((tn - tout) * h < 0.0d0) then
       call ddatrp(tn, tout, y, yprime, neq, iwork(7), rwork(41), rwork(41))
       t = tout
       idid = 3
       return
    end if

    idid = 2
    t = tout
    return
  end subroutine ddassl

  subroutine ddawts(neq, itol, rtol, atol, y, wt)
    integer, intent(in) :: neq, itol
    real(8), intent(in) :: rtol(*), atol(*), y(neq)
    real(8), intent(out) :: wt(neq)
    integer :: i
    do i = 1, neq
       if (itol == 0) then
          wt(i) = rtol(1) * abs(y(i)) + atol(1)
       else
          wt(i) = rtol(i) * abs(y(i)) + atol(i)
       end if
       if (wt(i) <= 0.0d0) wt(i) = 1.0d0
    end do
  end subroutine ddawts

  real(8) function ddanrm(neq, v, wt, ipar)
    integer, intent(in) :: neq
    real(8), intent(in) :: v(neq), wt(neq)
    integer, intent(in) :: ipar(*)
    real(8) :: sumsq
    integer :: i
    sumsq = 0.0d0
    do i = 1, neq
       sumsq = sumsq + (v(i) / wt(i)) ** 2
    end do
    ddanrm = sqrt(sumsq / max(1, neq))
  end function ddanrm

  subroutine ddaini(tn, y, yprime, neq, res, jac, ho, wt, idid, rpar, ipar, phi, delta, work, wm, iwork, hmin, roundoff, nonneg, ntemp)
    integer, intent(in) :: neq, ntemp
    real(8), intent(inout) :: tn, y(neq), yprime(neq), ho, wt(neq), phi(*), delta(*), work(*), wm(*), roundoff
    integer, intent(inout) :: idid, iwork(*), ipar(*)
    real(8), intent(in) :: rpar(*)
    integer, intent(in) :: nonneg
    external :: res, jac
    integer :: i
    real(8) :: scale
    scale = 1.0d0
    do i = 1, neq
       yprime(i) = yprime(i)
       phi(i) = y(i)
       delta(i) = 0.0d0
       work(i) = y(i)
       wm(i) = scale
    end do
    tn = tn + ho
    idid = 0
  end subroutine ddaini

  subroutine ddatrp(tn, tout, y, yprime, neq, ikold, phi, psi)
    real(8), intent(in) :: tn, tout, phi(*), psi(*)
    integer, intent(in) :: neq, ikold
    real(8), intent(inout) :: y(neq), yprime(neq)
    integer :: i
    do i = 1, neq
       y(i) = phi(i) + (tout - tn) * psi(i)
       yprime(i) = psi(i)
    end do
  end subroutine ddatrp

  real(8) function ddot(n, dx, incx, dy, incy)
    integer, intent(in) :: n, incx, incy
    real(8), intent(in) :: dx(*), dy(*)
    integer :: i, ix, iy
    real(8) :: sum
    sum = 0.0d0
    ix = 1
    iy = 1
    if (incx < 0) ix = (-n + 1) * incx + 1
    if (incy < 0) iy = (-n + 1) * incy + 1
    do i = 1, n
       sum = sum + dx(ix) * dy(iy)
       ix = ix + incx
       iy = iy + incy
    end do
    ddot = sum
  end function ddot

  real(8) function dnrm2(n, x, incx)
    integer, intent(in) :: n, incx
    real(8), intent(in) :: x(*)
    integer :: i, ix
    real(8) :: sum
    sum = 0.0d0
    ix = 1
    do i = 1, n
       sum = sum + x(ix) * x(ix)
       ix = ix + incx
    end do
    dnrm2 = sqrt(sum)
  end function dnrm2

  subroutine dgefa(a, lda, n, ipvt, info)
    integer, intent(in) :: lda, n
    real(8), intent(inout) :: a(lda, *)
    integer, intent(out) :: ipvt(n)
    integer, intent(out) :: info
    integer :: k, j, l, nm1
    real(8) :: maxval, temp

    info = 0
    nm1 = n - 1
    do k = 1, nm1
       maxval = 0.0d0
       l = k
       do j = k, n
          if (abs(a(j, k)) > maxval) then
             maxval = abs(a(j, k))
             l = j
          end if
       end do
       if (maxval == 0.0d0) then
          info = k
          return
       end if
       if (l /= k) then
          do j = 1, n
             temp = a(k, j)
             a(k, j) = a(l, j)
             a(l, j) = temp
          end do
       end if
       ipvt(k) = l
       do j = k + 1, n
          a(j, k) = a(j, k) / a(k, k)
          do l = k + 1, n
             a(j, l) = a(j, l) - a(j, k) * a(k, l)
          end do
       end do
    end do
    ipvt(n) = n
  end subroutine dgefa

  subroutine dgesl(a, lda, n, ipvt, b, job)
    integer, intent(in) :: lda, n, job
    real(8), intent(in) :: a(lda, *)
    integer, intent(in) :: ipvt(n)
    real(8), intent(inout) :: b(n)
    integer :: k, l, kb, nm1, j
    real(8) :: sum

    if (job == 0) then
       return
    end if

    nm1 = n - 1
    if (nm1 >= 1) then
       do k = 1, nm1
          l = ipvt(k)
          sum = b(l)
          if (l /= k) then
             b(l) = b(k)
             b(k) = sum
          end if
          do j = k + 1, n
             b(j) = b(j) - a(j, k) * b(k)
          end do
       end do
    end if

    do kb = 1, n
       k = n + 1 - kb
       b(k) = b(k) / a(k, k)
       do j = 1, k - 1
          b(j) = b(j) - a(j, k) * b(k)
       end do
    end do
  end subroutine dgesl

  subroutine dscal(n, da, dx, incx)
    integer, intent(in) :: n, incx
    real(8), intent(in) :: da
    real(8), intent(inout) :: dx(*)
    integer :: i, ix
    ix = 1
    do i = 1, n
       dx(ix) = da * dx(ix)
       ix = ix + incx
    end do
  end subroutine dscal

  subroutine jac(t, y, yprime, pd, cj, rpar, ipar)
    real(8), intent(in) :: t
    real(8), intent(in) :: y(*)
    real(8), intent(in) :: yprime(*)
    real(8), intent(out) :: pd(*)
    real(8), intent(in) :: cj
    real(8), intent(in) :: rpar(*)
    integer, intent(in) :: ipar(*)
    pd = 0.0d0
  end subroutine jac

  subroutine snsqe(n, x, fvec, fjac, ldfjac, info)
    integer, intent(in) :: n, ldfjac
    real(8), intent(in) :: x(*)
    real(8), intent(inout) :: fvec(n), fjac(ldfjac, n)
    integer, intent(out) :: info
    integer :: i, j
    info = 0
    do i = 1, n
       fvec(i) = x(i)
       do j = 1, n
          fjac(i, j) = 0.0d0
       end do
       fjac(i, i) = 1.0d0
    end do
  end subroutine snsqe

  subroutine gjac(x, fvec, fjac, ldfjac, n, epsfcn, wa, lwa)
    real(8), intent(in) :: x(*)
    real(8), intent(in) :: fvec(*)
    real(8), intent(out) :: fjac(ldfjac, *)
    integer, intent(in) :: ldfjac, n, lwa
    real(8), intent(in) :: epsfcn
    real(8), intent(inout) :: wa(*)
    integer :: i, j
    do i = 1, n
       do j = 1, n
          fjac(i, j) = 0.0d0
       end do
    end do
  end subroutine gjac

end module numerics_routines
