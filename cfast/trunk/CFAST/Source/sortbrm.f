      SUBROUTINE SORTBRM(
     I                   X,LX,IX,LIX,NROW,NCOLX,NCOLIX,ISORT,LDP,NROOM,
     O                   IPOINT)

C     Function: Sort the two arrays X and IX by the ISORT'th column of
C               IX which contains room data.  This routine is based on the
C               now obsolete routine SORTFR.  This routine is used
C               to sort fire and detector data structures by room 
C               number.
C
C     Inputs:   X       Floating point info to be sorted
C               LX      Leading dimension of X 
C               IX      Integer info to be sorted
C               LIX     Leading dimension of IX in calling routine
C               NROW    Number of rows in X and IX
C               NCOLX   Number of columns in X
C               NCOLIX  Number of columns in IX
C               ISORT   Column in IX to sort on (usually contains room numbers)
C               LDP     Leading dimension of IPOINT
C               NROOM   number of elements for which ipoint is defined, also
C                       the number of rooms
C
C     Outputs:  IPOINT  Pointer array for sorted X and IX list.
C                       (r,1) = Number of items (fires or detectors so far)
C                               in room r
C                       (r,2) = Pointer to beginning element in IX
C                               and X for fire or detector in room r
C
C     Revision History:
C        Created: 9/3/93 by GPF:
!	   Modified 10/31/2006 by WWJ
!
!				 SORTBRM is used to sort the detector information.
!
C                  This routine was created using SORTFR as a template.
C                  SORTFR was designed specifically to sort fire data 
C                  structures.  This routine is more general.  It is designed
C                  to sort any floating point and integer data structures
C                  by the data given in the ISORT'th colum of IX (usually
C                  room numbers).

      include "precis.fi"
      DIMENSION X(LX,NCOLX), IX(LIX,NCOLIX)
C  
C    IF THE NUMBER OF FIRES, DETECTORS OR ROOMS EVER EXCEEDS 200 THEN 
C    THE FOLLOWING DIMENSION STATEMENT NEEDS TO BE CHANGED
C
      PARAMETER (LWORK=100)
      DIMENSION IPOINT(LDP,*), WORK(LWORK), IWORK(LWORK), IPERM(LWORK)
C
C     CREATE A PERMUTATION VECTOR USING THE ISORT'TH COLUMN OF IX
C
      IF(NROW.GT.LWORK)THEN
              CALL XERROR('NOT ENOUGH WORK SPACE IN SORTBRM',
     .                    0,1,2)
      ENDIF
      DO 2 I = 1, NROW
    2    IPERM(I) = I
      CALL INDEXI(NROW,IX(1,ISORT),IPERM)
C
C     REORDER INTEGER ARRAY USING THE PERMUTATION VECTOR
C
      DO 5 J = 1, NCOLIX
        DO 10 I = 1, NROW
          IWORK(I) = IX(IPERM(I),J)
   10   CONTINUE
        DO 20 I = 1, NROW
          IX(I,J) = IWORK(I)
   20   CONTINUE
    5 CONTINUE
C
C    REORDER THE FLOATING POINT ARRAYS USING THE PERMUTATION VECTOR
C
      DO 50 J = 1, NCOLX
        DO 30 I = 1, NROW
          WORK(I) = X(IPERM(I),J)
   30   CONTINUE
        DO 40 I = 1, NROW
          X(I,J) = WORK(I)
   40   CONTINUE
   50 CONTINUE
C
C     CONSTRUCT THE POINTER ARRAY
C
      DO 60 I = 1, NROOM
        IPOINT(I,1) = 0
        IPOINT(I,2) = 0
   60 CONTINUE
      DO 70 I = 1, NROW
        IROOM = IX(I,ISORT)
        IPOINT(IROOM,1) = IPOINT(IROOM,1) + 1
        IF (IPOINT(IROOM,2).EQ.0) IPOINT(IROOM,2) = I
   70 CONTINUE
      DO 80 I = 1, NROOM
        IF (IPOINT(I,2).EQ.0) IPOINT(I,2) = 1
   80 CONTINUE
      RETURN
      END
