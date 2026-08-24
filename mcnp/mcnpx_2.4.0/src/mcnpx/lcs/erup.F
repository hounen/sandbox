!     $Id: erup.F,v 1.4 2002/03/27 18:15:02 gwm Exp $                         !/*er          1*/
      subroutine erup (name,wt)                                                 !/*er          2*/
#include <lcs/double.h>                                                         /*!er          3*/
#include <lcs/lptS.h>                                                           /*!er          4*/
#include <lcs/rucom.h>                                                          /*!er          5*/
#include <lcs/cacom.h>                                                          /*!er          6*/
#include <lcs/ercom.h>                                                          /*!er          7*/
#include <lcs/inout.h>                                                          /*!er          8*/
      dimension exma(6)                                                         !/*er          9*/
      data ncbal /0/                                                            !/*er         10*/
      data ballim /0.1d0/                                                       !/*er         11*/
      nph=0                                                                     !/*er         12*/
      mnuc=0                                                                    !/*er         13*/
      if (apr.eq.dp0) then                                                      !/*er         14*/
        do 10 i=1,6                                                             !/*er         15*/
   10   npart(i)=0                                                              !/*er         16*/
        hevsum=dp0                                                              !/*er         17*/
        uu=dp0                                                                  !/*er         18*/
        go to 110                                                               !/*er         19*/
      endif                                                                     !/*er         20*/
      m2=apr                                                                    !/*er         21*/
      if (ncbal.le.10) then                                                     !/*er         22*/
        ex1=energy(apr,zpr)                                                     !/*er         23*/
        aa1=apr                                                                 !/*er         24*/
        zz1=zpr                                                                 !/*er         25*/
        casbal=ex+erec+ex1                                                      !/*er         26*/
      endif                                                                     !/*er         27*/
      m3=zpr                                                                    !/*er         28*/
      ifiss=0                                                                   !/*er         29*/
      if (ilvden.lt.0.or.ievap.eq.1) then                                       !/*er         30*/
        if (m3.ge.71) then                                                      !/*er         31*/
          bz0=bzero                                                             !/*er         32*/
          yz0=yzero                                                             !/*er         33*/
        else                                                                    !/*er         34*/
          bz0=bzere                                                             !/*er         35*/
          yz0=yzere                                                             !/*er         36*/
        endif                                                                   !/*er         37*/
      endif                                                                     !/*er         38*/
      wtfis=wt                                                                  !/*er         39*/
      call dres (m2,m3,wtfis,exma,ecv,ex,fiswt,icc,ifisct,ipht,mnuc             !/*er         40*/
     & ,nocas,nph,cosevp,coshev,epart,hepart,npart,maxph,aprr,ecvr,elv          !/*er         41*/
     & ,eph,erecr,tph,ur,uur,vr,wr,zprr,l12,l13)                                !/*er         42*/
      if (ncbal.gt.10.or.apr.le.dp0) go to 30                                   !/*er         43*/
      do 20 i=1,mnuc                                                            !/*er         44*/
      if (aprr(i).gt.dp0) casbal=casbal-energy(aprr(i),zprr(i))                 !/*er         45*/
      casbal=casbal-uur(i)-ecvr(i)-erecr(i)                                     !/*er         46*/
   20 continue                                                                  !/*er         47*/
   30 continue                                                                  !/*er         48*/
      do 40 i=1,6                                                               !/*er         49*/
      if (i.gt.2) then                                                          !/*er         50*/
        lmtxx=l13                                                               !/*er         51*/
      else                                                                      !/*er         52*/
        lmtxx=l12                                                               !/*er         53*/
      endif                                                                     !/*er         54*/
      if (npart(i).gt.lmtxx) then                                               !/*er         55*/
        nx=i                                                                    !/*er         56*/
        write (io,130) nx,lmtxx                                                 !/*er         57*/
        call expirx(1,'ERUP','npart(i).gt.lmtxx')                               !/*er         58*/
      endif                                                                     !/*er         59*/
   40 continue                                                                  !/*er         60*/
      do 80 k=1,6                                                               !/*er         61*/
      np=npart(k)                                                               !/*er         62*/
      if (np.eq.0) go to 80                                                     !/*er         63*/
      if (k.gt.2) go to 60                                                      !/*er         64*/
      do 50 j=1,np                                                              !/*er         65*/
      if (epart(j,k).gt.dp0) casbal=casbal-epart(j,k)-exma(k)                   !/*er         66*/
      t1=costh*cosevp(1,j,k)+sinth*cosevp(3,j,k)                                !/*er         67*/
      u1=cosphi*t1-sinphi*cosevp(2,j,k)                                         !/*er         68*/
      cosevp(2,j,k)=sinphi*t1+cosphi*cosevp(2,j,k)                              !/*er         69*/
      cosevp(3,j,k)=costh*cosevp(3,j,k)-sinth*cosevp(1,j,k)                     !/*er         70*/
      cosevp(1,j,k)=u1                                                          !/*er         71*/
   50 continue                                                                  !/*er         72*/
      go to 80                                                                  !/*er         73*/
   60 continue                                                                  !/*er         74*/
      l=k-2                                                                     !/*er         75*/
      if (l.eq.2.and.apr.ge.1.d3) np=np-14                                      !/*er         76*/
      do 70 j=1,np                                                              !/*er         77*/
      if (hepart(j,l).gt.dp0) casbal=casbal-hepart(j,l)-exma(k)                 !/*er         78*/
      t1=costh*coshev(1,j,l)+sinth*coshev(3,j,l)                                !/*er         79*/
      u1=cosphi*t1-sinphi*coshev(2,j,l)                                         !/*er         80*/
      coshev(2,j,l)=sinphi*t1+cosphi*coshev(2,j,l)                              !/*er         81*/
      coshev(3,j,l)=costh*coshev(3,j,l)-sinth*coshev(1,j,l)                     !/*er         82*/
      coshev(1,j,l)=u1                                                          !/*er         83*/
   70 continue                                                                  !/*er         84*/
   80 continue                                                                  !/*er         85*/
      if (ipht.eq.0.or.nph.eq.0) go to 100                                      !/*er         86*/
      do 90 k=1,nph                                                             !/*er         87*/
      casbal=casbal-eph(k)                                                      !/*er         88*/
   90 continue                                                                  !/*er         89*/
  100 continue                                                                  !/*er         90*/
      if (ncbal.le.10.and.abs(casbal).gt.ballim) then                           !/*er         91*/
        ncbal=ncbal+1                                                           !/*er         92*/
        write (io,120) nocas,name,fkey,aa1,zz1,casbal                           !/*er         93*/
        write (itty,120) nocas,name,fkey,aa1,zz1,casbal                         !/*er         94*/
      endif                                                                     !/*er         95*/
  110 continue                                                                  !/*er         96*/
      return                                                                    !/*er         97*/
!                                                                               !/*er         98*/
  120 format (/5x,' dres imbalance: nocas=',i6,' name',i4,' fkey=',f3.1/        !/*er         99*/
     &' aa1=',f5.0,' zz1=',f5.0,' bal=',1pe10.3)                                !/*er        100*/
  130 format (7h0npart(,i1,9h) exceeds,i5,8h in erup)                           !/*er        101*/
      end                                                                       !/*er        102*/
