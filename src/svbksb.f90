      SUBROUTINE SVBKSB(U,W,V,M,N,MP,NP,B,X)

      USE CONS_PARAM
      Real(dp), Dimension(:), Allocatable :: TMP
      Integer :: Status
      real(dp) U(MP,NP),W(NP),V(NP,NP),B(MP),X(NP)
      
      Allocate (TMP(N), Stat=Status)
      If (Status .ne. 0) then
        Print *,'Unable to allocate memory for TMP in svbksb.f90'
        Stop
      End if

      DO 12 J=1,N

        S=0.

        IF(W(J).NE.0.)THEN

          DO 11 I=1,M

            S=S+U(I,J)*B(I)

11        CONTINUE

          S=S/W(J)

        ENDIF

        TMP(J)=S

12    CONTINUE

      DO 14 J=1,N

        S=0.

        DO 13 JJ=1,N

          S=S+V(J,JJ)*TMP(JJ)

13      CONTINUE

        X(J)=S

14    CONTINUE

      Deallocate (TMP)

      RETURN

      END

!CVSVERSIONINFO "$Id: svbksb.f90,v 1.5 2012/04/10 22:17:28 keiji Exp $"
