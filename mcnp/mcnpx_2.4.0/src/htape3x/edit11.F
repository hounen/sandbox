!     $Id: edit11.F,v 1.5 2002/03/27 18:13:57 gwm Exp $                       !/*ed          1*/
      subroutine edit11 (tlly,tlsq,cov,nerg,nerg1,l,k)                          !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/label.h>                                                      /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/bndy.h>                                                       /*!ed          6*/
#include <htape3x/ctconv.h>                                                     /*!ed          7*/
      dimension tlly(3,nerg1,1), tlsq(3,nerg1,1), cov(3,nerg1,1)                !/*ed          8*/
#include <htape3x/kpt.h>                                                        /*!ed          9*/
      dimension val(6), sd(6)                                                   !/*ed         10*/
      entry edit12(tlly,tlsq,cov,nerg,nerg1,l,k)                                !/*ed         11*/
      ityp=itip(l)+1                                                            !/*ed         12*/
      do 50 iseg=1,nseg1                                                        !/*ed         13*/
      do 40 ia=1,nfprm1                                                         !/*ed         14*/
      fmult=dfparm(ia)/dnparm(k)                                                !/*ed         15*/
      if (.not.lbunch) m=ia+nfprm1*(iseg-1+nseg1*(k-1+nparm*(l-1)))             !/*ed         16*/
      if (lbunch) m=ia+nfprm1*(iseg-1+nseg1*(l-1))                              !/*ed         17*/
      if (tlly(1,nerg1,m).eq.0) go to 40                                        !/*ed         18*/
      tttt=1.0/fnorm                                                            !/*ed         19*/
      if (lbunch) then                                                          !/*ed         20*/
        if (ia.lt.nfprm1) write (io,140) kpart(ityp),ia,tttt,(lparm(i),i        !/*ed         21*/
     &  =1,nparm)                                                               !/*ed         22*/
        if (ia.ge.nfprm1) write (io,150) kpart(ityp),tttt,(lparm(i),i=1         !/*ed         23*/
     &  ,nparm)                                                                 !/*ed         24*/
      else                                                                      !/*ed         25*/
        if (ia.lt.nfprm1) write (io,90) kpart(ityp),lparm(k),ia,tttt            !/*ed         26*/
        if (ia.ge.nfprm1) write (io,100) kpart(ityp),lparm(k),tttt              !/*ed         27*/
      endif                                                                     !/*ed         28*/
      if (iopt.eq.11.and.nseg.gt.0) write (io,110) iseg                         !/*ed         29*/
      if (iopt.eq.12.and.iseg.eq.1) write (io,120)                              !/*ed         30*/
      if (iopt.eq.12.and.iseg.ne.1) write (io,130)                              !/*ed         31*/
      if (lsnprm) write (io,160) dnparm(k)                                      !/*ed         32*/
      write (io,80)                                                             !/*ed         33*/
      nlo=1                                                                     !/*ed         34*/
      if (nerg1.eq.2) nlo=2                                                     !/*ed         35*/
      do 30 j=nlo,nerg1                                                         !/*ed         36*/
      do 10 i=1,6                                                               !/*ed         37*/
      val(i)=dp0                                                                !/*ed         38*/
   10 sd(i)=dp0                                                                 !/*ed         39*/
      val(1)=tlly(1,j,m)                                                        !/*ed         40*/
      if (val(1).le.dp0) go to 20                                               !/*ed         41*/
      sd(1)=tlsq(1,j,m)                                                         !/*ed         42*/
      v0=sd(1)**2                                                               !/*ed         43*/
      v1=tlsq(2,j,m)**2                                                         !/*ed         44*/
      v2=tlsq(3,j,m)**2                                                         !/*ed         45*/
      c01=cov(1,j,m)                                                            !/*ed         46*/
      c12=cov(2,j,m)                                                            !/*ed         47*/
      c20=cov(3,j,m)                                                            !/*ed         48*/
      val(2)=tlly(2,j,m)/val(1)                                                 !/*ed         49*/
      sd(2)=v0+v1-2.0*c01                                                       !/*ed         50*/
      val(3)=tlly(3,j,m)/val(1)                                                 !/*ed         51*/
      sd(3)=v0+v2-2.0*c20                                                       !/*ed         52*/
      temp=val(3)-val(2)**2                                                     !/*ed         53*/
      if (temp.le.dp0) go to 20                                                 !/*ed         54*/
      val(4)=sqrt(temp)                                                         !/*ed         55*/
!      cv2v3 is the relative covariance between val(2) and val(3)               !/*ed         56*/
      cv2v3=v0-c01+c12-c20                                                      !/*ed         57*/
      sd(4)=(sd(2)*val(2)**4+0.25*sd(3)*val(3)**2-val(3)*val(2)**2*cv2v3        !/*ed         58*/
     & )/val(4)**4                                                              !/*ed         59*/
!      cv1v4 is the relative covariance between val(1) and val(4)               !/*ed         60*/
      cv1v4=(0.5*val(3)*(c20-v0)-val(2)**2*(c01-v0))/val(4)**2                  !/*ed         61*/
      val(5)=val(1)/val(4)**2                                                   !/*ed         62*/
      sd(5)=4.0*sd(4)+v0-4.0*cv1v4                                              !/*ed         63*/
      val(6)=val(1)/val(4)**3                                                   !/*ed         64*/
      sd(6)=9.0*sd(4)+v0-6.0*cv1v4                                              !/*ed         65*/
   20 continue                                                                  !/*ed         66*/
      val(1)=val(1)*fmult*dnerg(j)                                              !/*ed         67*/
      val(5)=val(5)*fmult*dnerg(j)                                              !/*ed         68*/
      val(6)=val(6)*fmult*dnerg(j)                                              !/*ed         69*/
      sd(2)=sqrt(max(sd(2),dp0))                                                !/*ed         70*/
      sd(4)=sqrt(max(sd(4),dp0))                                                !/*ed         71*/
      sd(5)=sqrt(max(sd(5),dp0))                                                !/*ed         72*/
      sd(6)=sqrt(max(sd(6),dp0))                                                !/*ed         73*/
      if (j.lt.nerg1) write (io,60) ergb(j),val(1),sd(1),val(2),sd(2),          !/*ed         74*/
     & (val(i),sd(i),i=4,6)                                                     !/*ed         75*/
      if (j.eq.nerg1) write (io,70) val(1),sd(1),val(2),sd(2),(val(i),sd        !/*ed         76*/
     & (i),i=4,6)                                                               !/*ed         77*/
   30 continue                                                                  !/*ed         78*/
   40 continue                                                                  !/*ed         79*/
   50 continue                                                                  !/*ed         80*/
      return                                                                    !/*ed         81*/
!                                                                               !/*ed         82*/
   60 format (1pd11.4,5(1pd14.4,0pf7.4))                                        !/*ed         83*/
   70 format (6x,'total',5(1pd14.4,0pf7.4))                                     !/*ed         84*/
   80 format (/4x,'energy',11x,'current',15x,'ave(t)',14x,'sigma',13x,          !/*ed         85*/
     & 'f.o.m. 1',13x,'f.o.m. 2')                                               !/*ed         86*/
   90 format (/10x,a8,1x,'time distribution across surface',i5,                 !/*ed         87*/
     & ' for angular bin ',i3,' (',1pg8.1,'*nanoseconds)')                      !/*ed         88*/
  100 format (/10x,a8,1x,'time distribution across surface',i5,                 !/*ed         89*/
     & ' for all angles ',' (',1pg8.1,'*nanoseconds)')                          !/*ed         90*/
  110 format (20x,'in segment ',i3)                                             !/*ed         91*/
  120 format (20x,'inside window')                                              !/*ed         92*/
  130 format (20x,'outside window')                                             !/*ed         93*/
  140 format (/10x,a8,' time distribution for angular bin ',i3,' (',            !/*ed         94*/
     & 1pg8.1,'*nanoseconds)'/10x,' across surfaces',15i4/(26x,15i4))           !/*ed         95*/
  150 format (/10x,a8,' time distribution for all angles ',' (',1pg8.1,         !/*ed         96*/
     & '*nanoseconds)'/10x,' across surfaces',15i4/(26x,15i4))                  !/*ed         97*/
  160 format (/35x,'tally divided by',1pd14.5)                                  !/*ed         98*/
      end                                                                       !/*ed         99*/
