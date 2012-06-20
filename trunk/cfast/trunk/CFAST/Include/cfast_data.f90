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
