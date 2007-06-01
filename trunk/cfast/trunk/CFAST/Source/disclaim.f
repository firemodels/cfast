        SUBROUTINE DISCLAIM (NAME)

      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"
      CHARACTER NAME*(*), OBUF*14

C     December 15, 1989 - created to specify version, model name and run date
C     Modified 2-11-93 - Arithmetic on PC was giving wrong version numbers.
C                       ( .9999999999 instead 1.00000). Changed calculations
C                        to use integer arithmetic with MOD functions. This
C                        was probably a compiler bug.
C
C     FIRST THE DATE
     
      WRITE(OBUF,2) RUNDAT(1),RUNDAT(2),RUNDAT(3)

C     NOW THE VERSION

	call splitversion (version,majver,minver,minrev)
      IF (MINREV.GT.10) THEN
        write (aminrev,'(a2)') minrev
      else if (minrev.gt.0) then
        write (aminrev,'(a1)') minrev
      else
        aminrev = '  '
      end if
        WRITE (IOFILO,1) NAME, MAJVER,MINVER,AMINREV, OBUF
      	RETURN
1     FORMAT(' **   ',A8,'  Version',3X,2(I1,'.'),A2,2X,A14,4X,'**',/,
     .       ' ** ',46X,' **',/,
     .       ' **             A contribution of the              **',/,
     .       ' ** National Institute of Standards and Technology **',/,
     .       ' **             Gaithersburg, MD  20899            **',/,
     .       ' **             Not subject to Copyright           **',/)
2     FORMAT('Run ',I4.4,'/',I2.2,'/',I2.2)

      END
