module cenviro
    
    use cparams
    implicit none
    
    integer :: jaccol, neqoff

    integer, parameter :: constvar = 1 ,odevara = 2 ,odevarb = 4, odevarc = 8
    integer, parameter :: eqp = 1, eqpmv = 2, eqtmv = 3, eqtu = 4, eqvu = 5, eqtl = 6, eqoxyl = 7, eqoxyu = 8, eqtt = 9, eqwt = 10

    ! index into izhall
    integer, parameter :: ihroom = 1, ihvelflag = 2, ihdepthflag = 3, ihventnum = 4, ihhalfflag = 5, ihmode = 6, ihxy = 7

    ! index into zzhall
    integer, parameter :: ihtime0 = 1, ihvel = 2, ihdepth = 3, ihmaxlen = 4, ihhalf = 5, ihtemp = 6, ihorg = 7, ihdist = 8

    ! index for hall models
    integer, parameter :: ihbefore = 1, ihduring = 2, ihafter = 3

    logical updatehall, izdtflag

    real*8, dimension(nr) :: zzvmin, zzvmax, zzrelp, zzpabs, zzyflor, zzyceil
    real*8, dimension(nr,2) :: zzvol, zzhlay, zztemp, zzrho, zzmass, zzftemp
    real*8, dimension(nr,2,ns) :: zzgspec, zzcspec
    real*8, dimension(nr,nwal) :: zzwspec
    real*8, dimension(nr,nwal,2) :: zzwtemp
    real*8, dimension(mxhvsys,ns) :: zzhvpr
    real*8, dimension(mxhvsys) :: zzhvm
    real*8, dimension(nr,4) :: zzwarea
    real*8, dimension(nr,10,3) :: zzwcen
    real*8, dimension(nr,10) :: zzwarea2
    real*8, dimension(nr,8) :: zzhall
    real*8, dimension(mxpts,nr) :: zzrvol, zzrarea, zzrhgt
    real*8, dimension(2,nr) :: zzabsb, zzbeam
    real*8, dimension(0:nv+1) :: zzdisc
    real*8, dimension(nr,nr) :: zzhtfrac
    real*8 :: zzdtcrit
 
    integer, dimension(ns+2) :: izpmap
    integer, dimension(2,nr) :: izwmap
    integer, dimension(4,nr) :: izwmap2
    integer, dimension(nr,4) :: izswal
    integer, dimension(4*nr,5) :: izwall
    integer, dimension(mxtarg) :: iztarg
    integer, dimension(maxeq,2) :: izeqmap
    integer, dimension(nr) :: izrvol, izhvac(nr), izcon(nr), iznwall(nr), izshaft(nr)
    integer, dimension(nr,7) :: izhall
    integer, dimension(0:nr) :: izheat
    integer, dimension(nr,0:nr) :: izhtfrac
    integer :: izdtnum,izdtmax, izndisc, nswal
    
end module cenviro
   
    module cfin
    
    implicit none
    
    integer, parameter :: lbufln=1024
    character(lbufln) :: lbuf, cbuf
    
end module cfin

module cfio

    use cfin    
    implicit none
    
    ! input/output data for readas, readin, ...
      integer :: start, first, last, count, type, ix
      logical :: valid
      character(lbufln) :: inbuf
      real :: xi

end module cfio
    
    module fltarget
    use cparams
    implicit none
    
    ! variables for calculation of flux to a target
      
    ! indices into floating point target data structure (XXTARG)      
    integer, parameter :: trgcenx = 1
    integer, parameter :: trgceny = 2
    integer, parameter :: trgcenz = 3
    integer, parameter :: trgnormx = 4
    integer, parameter :: trgnormy = 5
    integer, parameter :: trgnormz = 6
    integer, parameter :: trgk = 7
    integer, parameter :: trgrho = 8
    integer, parameter :: trgcp = 9
    integer, parameter :: trgl = 10
    integer, parameter :: trginterior = 11
    integer, parameter :: trgemis = 12
    integer, parameter :: trgtfluxf = 13
    integer, parameter :: trgtfluxb = 14
    integer, parameter :: trgnfluxf = 15
    integer, parameter :: trgnfluxb = 16

    ! indices into integer target data structure (IXTARG)
    integer, parameter :: trgroom = 1
    integer, parameter :: trglayer = 2
    integer, parameter :: trgwall = 3
    integer, parameter :: trgmeth = 4
    integer, parameter :: trgeq = 5
    integer, parameter :: trgback = 6
    
    integer, parameter :: ode = 1
    integer, parameter :: pde = 2
    integer, parameter :: cylpde = 3
    integer, parameter :: steady = 1
    integer, parameter :: mplicit = 2
    integer, parameter :: xplicit = 3
    integer, parameter :: int = 1
    integer, parameter :: ext =2

    character(8) :: cxtarg(mxtarg)

    real(8), dimension(mxtarg,2) :: qtflux, qtcflux, qtfflux, qtwflux, qtgflux
    real(8), dimension(mxtarg) :: tgtarg
    real(8), dimension(mxtarg,5)  :: gtflux
    integer, dimension(3) :: neqtarg
end module fltarget
    
    module  iofiles

    implicit none
    
!File descriptors for cfast

    character(6), parameter :: heading="VERSN"
	character(64) ::  project
	character(256) :: datapath, exepath, inputfile, outputfile, smvhead, smvdata, smvcsv, &
	      ssflow, ssnormal, ssspecies, sswall, errorlogging, stopfile, solverini, &
	      historyfile, queryfile, statusfile, kernelisrunning

! Work arrays for the csv input routines

    integer, parameter :: nrow=200, ncol=200
    real*8 rarray(nrow,ncol) 
    character(128) :: carray(nrow,ncol)

end module iofiles

module params

    use cparams
    implicit none

!   these are temporary work arrays

!   ex... are the settings for the external ambient
!   qfr,... are the heat balance calculations for resid and cnduct. it is now indexed by fire rather than by compartment
!   the variables ht.. and hf.. are for vertical flow
!   the volume fractions volfru and volfrl are calculated by resid at the beginning of a time step
!   hvfrac is the fraction that a mv duct is in the upper or lower layer

    logical allowed(ns), exset
    integer mapltw(nwal), ihmlar(2,nr,nr), izhvmapi(mnode), izhvmape(mnode), izhvie(mnode), izhvsys(mnode), izhvbsys(mbr), nhvpvar, nhvtvar, nhvsys

    real*8 qfr(mxfire), qfc(2,nr), qscnv(nwal,nr), qdout(nwal,nr), qsradw(nwal,nr), qdin(nwal,nr), qcvent(mxvents,nv), o2n2(ns), hwjdot(nwal,nr), exsal, &
        htot(nr), htflow(nr,2), hmflow(nr,2), htfnet(2,nr,nr), volfru(nr), volfrl(nr), hvfrac(2,mext), expa, exta, exra, &
        hcratt, chv(mbr), dhvprsys(mnode,ns), hvtm(mxhvsys), hvmfsys(mxhvsys),hvdara(mbr), hvt, ductcv

    common exsal, qfr, qfc, qscnv, qdout, qsradw, hmflow, mapltw, qdin, expa, exta, exra, qcvent, o2n2, hwjdot, &
        htot, htflow, htfnet, volfru, volfrl, hvfrac, hcratt, ihmlar, hvmfsys,dhvprsys,hvtm,hvdara,hvt,chv,ductcv, &
        exset, allowed, izhvmapi,izhvmape,izhvie,nhvpvar,nhvtvar, izhvsys,izhvbsys,nhvsys

end module params
