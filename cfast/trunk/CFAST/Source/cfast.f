            program cfast

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cfin.fi"
      include "params.fi"
      include "thermp.fi"
      include "objects1.fi"
      include "cenviro.fi"
      include "iofiles77.fi"

      logical error
	integer errorcode
      errorcode = 0

c     initialize the basic memory configuration

      call initmm
      call initob
      call readop
c     
	call readcf1 (errorcode)
c     
	write (logerr, 5000) mpsdatc
	if (errorcode.gt.0) then
		write (*, 5001) errorcode
		stop 
	else
		write (logerr, 5002) project
	endif

c     creation date

      mpsdat(1) = rundat(1)
      mpsdat(2) = rundat(2)
      mpsdat(3) = rundat(3)

!	version information to the log file
	call versionout (version)

      call initslv
c     call the input routine for a normal start

      call readinputfile (errorcode)
	if (errorcode.gt.0) go to 10

!	version information to the output file
      if (header) call disclaim('CFAST')

c     initialize variables associated with species 

      call initspec

      xdelt = nsmax / deltat
      itmmax = xdelt + 1
      tstop = itmmax - 1

c     read the the databases

	call inputtpp (thrmfile, errorcode)
	if (errorcode.ne.0) go to 10

      call initwall(tstop,errorcode)
      if (errorcode.gt.0) go to 10

c     time step criterion

      stime = 0.0d0
      itmstp = 1

      ih = -1

      xdelt = nsmax / deltat
      itmmax = xdelt + 1
      tstop = itmmax - 1

      call outinitial(1)
	call cptime(tbeg)
	call solve(tstop,errorcode)
	call cptime(tend)
	write (logerr,5003) tend - tbeg
	call cfastexit ('CFAST', 0)
      stop

c     errors

   10 call cfastexit ('CFAST', errorcode)

 5000 format ('Date stamp from CFAST initialization ',a14)
 5001 format ('Error encountered in opening data files; code = ',i4)
 5002 format ('The project files are based on the root: ',a64)
 5003 format ('Total execution time = ',1pg10.3,' seconds')
 5020 format ('Error exit during initialization from CFAST main')
      END

      SUBROUTINE F_RCOUNT (LN)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     F_RCOUNT
C
C     Description:  This is an entry to fake out INIPAR.  Therin is a call to
C                   get the compartment count on the fly.  Although we do not
C                   use this capability in the main mode, we need the call to
C                   maintain compatibility with INIPAR for the other modules.
C
C     Arguments: LN number of compartments in the simulation
C
C     Revision History:
C        Created:  11/16/1992 at 8:42 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      ln = n
      return
      end

      block data initcs

c     define the basic parameters

      include "cparams.fi"
      include "cshell.fi"
      include "cplotd.fi"

      data thrmfile/'thermal'/, gfile/'GEOMETRY.DAT'/, 
     . pfile/'PARTITIN.DAT'/, ofile/'OBJECTS.DAT'/, dpath/' '/, 
     . dfile/'DATA.DAT'/, path/' '/, dlen/0/, plen/0/, advfea/.false./,
     . config/'HV6.CF'/, shell/.false./, csopen/.false./,
     . logerr/0/, nnfile/' '/, current/' '/,outfile/'<stdout>'/,
     . ICBG,ICTXT,ICHDR,ICSUB,ICPRO,ICMSG,ICMBG,ICHLP,ICHBG,ICEBG,
     . ICEMS/0,15,14,10,7,15,3,15,2,1,10/, DUMPF/' '/,RFILE/' '/,
     . RMFILE/1/, HEADER/.FALSE./, UNITS/0,5,12,18,31,37,24,27/,
     . ADUMPF/' '/, NOKBD/.FALSE./, GPAUSE/.FALSE./,QUICKEST/.FALSE./,
     . WEBPAGE/' '/, GUISURF/'DEFAULT'/, GUIOBJ/'DEFAULT'/,
     . CPFLSYNCH/'Synchronize gui and base models - do not edit'/,
     . GUIPROJ/' '/, GUIREF/' '/,GUILIC/' '/, initializeonly/.false./,
     . outputformat /0/, IOFILO/6/, REPORTO/0/, IOFILI/1/, LOGERR/3/,
     . OBFILI/99/, AHFILO/98/,WEBFILE/97/,
! Start 6 series January 1, 2005; this is testing only.
!     . VERSION/600/      
! First public release is 6.0.1, June 1, 2005
!     . VERSION/601/      
! Second public release is 6.0.2, July 1, 2005
!     . VERSION/602/      
! Bug fix of 6.0.2, July 20, 2005 is 6.0.3
!     . VERSION/603/      
! Bug fix of 6.0.3, July 31, 2005 (targets) is 6.0.4
!     . VERSION/604/      
! Ceiling jet calculation was backwards (dr,br); add normal calculation to cedit, October 1, 2005
!     . VERSION/605/      
! Error in CSV routine - during read conversion error, the output variable was not set to 0
!     . VERSION/606/
! Error in initialzation of flow variables for printout only; default hall calculation with zero input as well as -1
!     . VERSION/607/
! Error in fire routines that prevented TUHC generation and subsequent door jet burning
!     . VERSION/608/
! Error in printout routine for units of HCN and HCL; igntion time off by 1 s if igntion specified as t=0 s
!     . VERSION/609/
! Error in readinputfile ... subscripts were backwards for SWITCH array for ceiling-floor conduction check     
     . VERSION/6100/
! 
      DATA IDEF /1/, ITODEF /2/, LAYDEF /1/, ISPDEF /3/, ICHRS /4/,
     . IDEVO /6/, IDEVC /1/, SETAXP /.FALSE./, OPENPC /.FALSE./,
     . LSABCI /999/, LAFRAM /1/, IDTYPE /2/, SETPLT /.FALSE./,
     . WCOLOR/.TRUE./, WDASH/.TRUE./, SETCFP/.FALSE./
      
      DATA STYPE /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC',
     . 'H2O', 'OD', 'CT', 'TS'/
       
      END

      block data initcf

      include "precis.fi"
      include "cfast.fi"

      DATA CRDATE/2007,11,29/
      END
