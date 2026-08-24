!     $Id: redmas.F,v 1.4 2002/03/27 18:15:19 gwm Exp $                       !/*re          1*/
      subroutine redmas (ixedt,l1,l2)                                           !/*re          2*/
#include <lcs/double.h>                                                         /*!re          3*/
!                                                                               !/*re          4*/
!  REDMAS controls all the initiation phase for the Fermi breakup model.        !/*re          5*/
!  It read the data library, FBRDATA, and establishes the mass data             !/*re          6*/
!  arrays. It calls MASEDT to initialize the branching ratio arrays.            !/*re          7*/
!  Using IXEDT = 0 eliminates the long printout of the data.                    !/*re          8*/
!                                                                               !/*re          9*/
!    REFERENCES.......  FBRK                                                    !/*re         10*/
!    EXTERNALS........  EXITA     MASEDT    BRDTAB                              !/*re         11*/
!                                                                               !/*re         12*/
#include <lcs/parms.h>                                                          /*!re         13*/
#include <lcs/fbrcom.h>                                                         /*!re         14*/
      dimension istat(lstat)                                                    !/*re         15*/
      data rho0 /1.414d0/                                                       !/*re         16*/
      data ierr /0/                                                             !/*re         17*/
      do 10 i=1,lstat                                                           !/*re         18*/
   10 nklev(i)=0                                                                !/*re         19*/
!     L1 = output file; L2 = data file                                          !/*re         20*/
      lout=l1                                                                   !/*re         21*/
      lmass=l2                                                                  !/*re         22*/
      itop=17                                                                   !/*re         23*/
      atop=itop                                                                 !/*re         24*/
      unstab=dp1                                                                !/*re         25*/
      loff=0                                                                    !/*re         26*/
      do 20 i=1,11                                                              !/*re         27*/
      do 20 j=1,13                                                              !/*re         28*/
   20 ip(i,j)=0                                                                 !/*re         29*/
      do 30 i=1,lstat                                                           !/*re         30*/
      wtl(i)=dp1                                                                !/*re         31*/
   30 continue                                                                  !/*re         32*/
      read (lmass) nr,nr1,nr2                                                   !/*re         33*/
#ifdef LCS_PRINT                                                                /*!re         34*/
      write (lout,35) nr,nr1                                                    !/*re         35*/
   35 format (/3x,' no. of particle stable nuclei =',i2/                        !/*re         36*/
     & ' no. of particle unstable res.nuc.=',i2/' in FBRDATA library')          !/*re         37*/
#endif                                                                          /*!re         38*/
      nrm=lpart                                                                 !/*re         39*/
!                                                                               !/*re         40*/
!       read particle stable nuclei                                             !/*re         41*/
!                                                                               !/*re         42*/
      i=0                                                                       !/*re         43*/
      do 50 i1=1,nr                                                             !/*re         44*/
      read (lmass) zin,ain,xmin,nsin                                            !/*re         45*/
      if (ain.gt.atop) then                                                     !/*re         46*/
        call fixm (zin,ain,xmin)                                                !/*re         47*/
        read (lmass) idmm                                                       !/*re         48*/
        go to 50                                                                !/*re         49*/
      endif                                                                     !/*re         50*/
      i=i+1                                                                     !/*re         51*/
      a(i)=ain                                                                  !/*re         52*/
      z(i)=zin                                                                  !/*re         53*/
      xm(i)=xmin                                                                !/*re         54*/
      ns(i)=nsin                                                                !/*re         55*/
      jz(i)=z(i)                                                                !/*re         56*/
      ja(i)=a(i)                                                                !/*re         57*/
      axm(i)=xm(i)+a(i)*931.478d0                                               !/*re         58*/
      xminv(i)=dp1/axm(i)                                                       !/*re         59*/
      iz1=z(i)+1.01d0                                                           !/*re         60*/
      in1=a(i)-z(i)+1.01d0                                                      !/*re         61*/
      ip(iz1,in1)=i                                                             !/*re         62*/
      if (i.ge.8.or.i.eq.6) call fixm (zin,ain,xmin)                            !/*re         63*/
      nss=ns(i)                                                                 !/*re         64*/
      jns(i)=loff                                                               !/*re         65*/
      tmin=dph*(ja(i)-2*jz(i))                                                  !/*re         66*/
      tmin=abs(tmin)                                                            !/*re         67*/
      lo=loff+1                                                                 !/*re         68*/
      loff=loff+nss                                                             !/*re         69*/
      if (loff.gt.lstat) go to 120                                              !/*re         70*/
      read (lmass) (e(j),istat(j),tiso(j),j=lo,loff)                            !/*re         71*/
      do 40 j=lo,loff                                                           !/*re         72*/
      tiso(j)=max(tiso(j),tmin)                                                 !/*re         73*/
   40 continue                                                                  !/*re         74*/
   50 continue                                                                  !/*re         75*/
      nr=i                                                                      !/*re         76*/
#ifdef LCS_PRINT                                                                /*!re         77*/
      write (lout,55) nr,loff                                                   !/*re         78*/
   55 format (//' new nr =',i4,' required array length lstat=',i6)              !/*re         79*/
#endif                                                                          /*!re         80*/
      do 60 i=1,loff                                                            !/*re         81*/
      parity(i)=dp1                                                             !/*re         82*/
      if (istat(i).lt.0) parity(i)=-parity(i)                                   !/*re         83*/
      istat(i)=iabs(istat(i))                                                   !/*re         84*/
      aj(i)=dph*(istat(i)-1)                                                    !/*re         85*/
   60 continue                                                                  !/*re         86*/
      roh(1)=dp0                                                                !/*re         87*/
      do 70 i=2,nr                                                              !/*re         88*/
      if (a(i).gt.a(i-1)) go to 70                                              !/*re         89*/
      if (a(i).eq.a(i-1).and.z(i).gt.z(i-1)) go to 70                           !/*re         90*/
      write (lout,65) a(i),z(i)                                                 !/*re         91*/
   65 format (/' stable mass a=',i3,' z=',i3,' is out of order')                !/*re         92*/
      ierr=ierr+1                                                               !/*re         93*/
   70 continue                                                                  !/*re         94*/
      if (ierr.ne.0) go to 130                                                  !/*re         95*/
!                                                                               !/*re         96*/
!       read particle unstable res.nuc.                                         !/*re         97*/
!                                                                               !/*re         98*/
      i3=nr+1                                                                   !/*re         99*/
      i4=nr+nr1                                                                 !/*re        100*/
      i=0                                                                       !/*re        101*/
      do 80 j=i3,i4                                                             !/*re        102*/
      read (lmass) zin,ain,xmin                                                 !/*re        103*/
      call fixm (zin,ain,xmin)                                                  !/*re        104*/
      if (ain.gt.atop) go to 80                                                 !/*re        105*/
      i=i+1                                                                     !/*re        106*/
      i2=i+nr                                                                   !/*re        107*/
      a(i2)=ain                                                                 !/*re        108*/
      z(i2)=zin                                                                 !/*re        109*/
      xm(i2)=xmin                                                               !/*re        110*/
      jz(i2)=z(i2)                                                              !/*re        111*/
      ns(i2)=0                                                                  !/*re        112*/
      ja(i2)=a(i2)                                                              !/*re        113*/
      axm(i2)=xm(i2)+a(i2)*931.478d0                                            !/*re        114*/
      iz1=z(i2)+dp1                                                             !/*re        115*/
      in1=a(i2)-z(i2)+dp1                                                       !/*re        116*/
      ip(iz1,in1)=i2                                                            !/*re        117*/
   80 continue                                                                  !/*re        118*/
      nr1=i                                                                     !/*re        119*/
#ifdef LCS_PRINT                                                                /*!re        120*/
      write (lout,85) nr1                                                       !/*re        121*/
   85 format (//' new nr1=',i4)                                                 !/*re        122*/
#endif                                                                          /*!re        123*/
      i4=nr+nr1                                                                 !/*re        124*/
      do 90 i=1,i4                                                              !/*re        125*/
      ia=ja(i)                                                                  !/*re        126*/
      if (ia.lt.10) then                                                        !/*re        127*/
        if (ia.eq.9) roh(i)=3.25d0                                              !/*re        128*/
        if (ia.eq.8) roh(i)=2.83d0                                              !/*re        129*/
        if (ia.eq.7) roh(i)=2.42d0                                              !/*re        130*/
        if (ia.eq.6) roh(i)=2.02d0                                              !/*re        131*/
        if (ia.eq.5) roh(i)=2.02d0                                              !/*re        132*/
        if (ia.le.4) roh(i)=1.2d0                                               !/*re        133*/
      else                                                                      !/*re        134*/
        roh(i)=rho0*a(i)**dpth+dp1                                              !/*re        135*/
      endif                                                                     !/*re        136*/
   90 continue                                                                  !/*re        137*/
      call masedt (ixedt)                                                       !/*re        138*/
      n=0                                                                       !/*re        139*/
      do 100 i=1,lmas                                                           !/*re        140*/
      if (i.gt.nr) go to 100                                                    !/*re        141*/
      if (nklev(jns(i)+1).gt.0) go to 100                                       !/*re        142*/
      n=n+1                                                                     !/*re        143*/
  100 continue                                                                  !/*re        144*/
      if (n.gt.lmas1) call erprnt(1,1,0,0,0,0,0,0,                              !/*re        145*/
     & '19hREDMAS.  n.gt.lmas1')                                                !/*re        146*/
      do 110 i=1,nr2                                                            !/*re        147*/
      read (lmass) zin,ain,xmin                                                 !/*re        148*/
      call fixm (zin,ain,xmin)                                                  !/*re        149*/
  110 continue                                                                  !/*re        150*/
      return                                                                    !/*re        151*/
  120 continue                                                                  !/*re        152*/
      write (lout,125)                                                          !/*re        153*/
  125 format (' array length LSTAT must be increased')                          !/*re        154*/
      go to 140                                                                 !/*re        155*/
  130 continue                                                                  !/*re        156*/
      write (lout,135) ierr                                                     !/*re        157*/
  135 format (2x,i4,' errors in reading FBRDATA library')                       !/*re        158*/
  140 call erprnt(1,1,0,0,0,0,0,0,'7hREDMAS.')                                  !/*re        159*/
!                                                                               !/*re        160*/
      return                                                                    !/*re        161*/
      end                                                                       !/*re        162*/
