C==== INCLUDE FILE FOR THREE-BODY CALCULATION ==========================
      IMPLICIT REAL*8(A-H,O-Y)
      IMPLICIT COMPLEX*16(Z)
      
      INTEGER NIN,NOUT,NENE,NWAV,NSCT
      INTEGER NTOT,NOT2,NOC,NOD,NOD2
      REAL*8  JTOTI,JTOTF,JTOT_TMP
      PARAMETER(NIN=5,NOUT=6,NENE=15,NWAV=16,NSCT=17)
      PARAMETER(NTOT=3000,
     &          NOT2=4501500,
     &          NOC =20,
     &          NOD =3000,
     &          NOD2=9000000)
      PARAMETER(PI=3.141592653589793D0,
     &          HBC=197.327053D0,
     &          DMASS=938.91897D0,
     &          ALPHA=1.D0/137.0359895D0,
     &          ZI=(0.D0,1.D0))
