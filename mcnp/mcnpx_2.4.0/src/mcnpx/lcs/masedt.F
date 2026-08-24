!     $Id: masedt.F,v 1.5 2002/03/27 18:15:10 gwm Exp $                       !/*ma          1*/
      subroutine masedt (ixedt)                                                 !/*ma          2*/
#include <lcs/double.h>                                                         /*!ma          3*/
!.......................................................................        !/*ma          4*/
!  MASEDT initializes the channel probability arrays by calling                 !/*ma          5*/
!  CBRSET and LEVSET.  When called with a non-zero arguement, it will           !/*ma          6*/
!  print out the nuclear data library with branching ratios for each            !/*ma          7*/
!  level.                                                                       !/*ma          8*/
!.......................................................................        !/*ma          9*/
!                                                                               !/*ma         10*/
!    REFERENCES.......  REDMAS                                                  !/*ma         11*/
!    EXTERNALS........  GMS       LEVBRK    EXITA                               !/*ma         12*/
!                                                                               !/*ma         13*/
#include <lcs/parms.h>                                                          /*!ma         14*/
#include <lcs/fbrcom.h>                                                         /*!ma         15*/
      dimension ja3(nmx), il3(nmx), jz3(nmx)                                    !/*ma         16*/
      dimension est0(20)                                                        !/*ma         17*/
!.......................................................................        !/*ma         18*/
      data est0 /13*100,7*40/                                                   !/*ma         19*/
! The definition of est0 allows all possible channels for masses le. 13,        !/*ma         20*/
! but only those open at 40 MeV for masses gt. 13.                              !/*ma         21*/
!.......................................................................        !/*ma         22*/
      call gms                                                                  !/*ma         23*/
      nmax=lchan                                                                !/*ma         24*/
      if (ixedt.ne.0) write (lout,100)                                          !/*ma         25*/
      do 60 ia=1,itop                                                           !/*ma         26*/
      iup=min(ia+1,11)                                                          !/*ma         27*/
      do 50 iz1=1,iup                                                           !/*ma         28*/
      iz=iz1-1                                                                  !/*ma         29*/
      in=ia-iz                                                                  !/*ma         30*/
      in1=in+1                                                                  !/*ma         31*/
      if (in1.gt.13.or.in1.lt.1) go to 50                                       !/*ma         32*/
      ipt=ip(iz1,in1)                                                           !/*ma         33*/
      if (ipt.eq.0) then                                                        !/*ma         34*/
        if (ixedt.ne.0) write (lout,90) iz,ia                                   !/*ma         35*/
        go to 50                                                                !/*ma         36*/
      endif                                                                     !/*ma         37*/
      est=est0(ia)                                                              !/*ma         38*/
      ira=ia                                                                    !/*ma         39*/
      irz=iz                                                                    !/*ma         40*/
      call cbrset (iz,ia,est,nmax)                                              !/*ma         41*/
      nss=ns(ipt)                                                               !/*ma         42*/
      if (ixedt.ne.0) write (lout,70) iz,ia,xm(ipt),nss,roh(ipt),rh(ia)         !/*ma         43*/
      if (nss.eq.0.or.ia.eq.itop) go to 50                                      !/*ma         44*/
      lo=jns(ipt)+1                                                             !/*ma         45*/
      lhi=lo+nss-1                                                              !/*ma         46*/
      do 40 i=lo,lhi                                                            !/*ma         47*/
      if (nklev(i).ne.0) call erprnt(1,1,0,0,0,0,0,0,                           !/*ma         48*/
     & '22hMASEDT.  nklev(i).ne.0')                                             !/*ma         49*/
      kk=i-lo+1                                                                 !/*ma         50*/
      call levset (ipt,kk)                                                      !/*ma         51*/
      nk=nklev(i)                                                               !/*ma         52*/
      if (nk.le.0) go to 30                                                     !/*ma         53*/
      nlo=il0(i)+1                                                              !/*ma         54*/
      nup=nlo+nk-1                                                              !/*ma         55*/
      wtot=wf(nup)                                                              !/*ma         56*/
      wtl(i)=unstab                                                             !/*ma         57*/
      if (ixedt.eq.0) go to 40                                                  !/*ma         58*/
      write (lout,110) e(i),aj(i),parity(i),tiso(i),wtl(i),nk                   !/*ma         59*/
      if (ixedt.ne.2) go to 40                                                  !/*ma         60*/
      prob=dp1                                                                  !/*ma         61*/
      do 20 k=nlo,nup                                                           !/*ma         62*/
      if (nk.gt.1) then                                                         !/*ma         63*/
        if (k.eq.nlo) prob=wf(k)/wtot                                           !/*ma         64*/
        if (k.gt.nlo) prob=(wf(k)-wf(k-1))/wtot                                 !/*ma         65*/
      endif                                                                     !/*ma         66*/
      q=q1(k)                                                                   !/*ma         67*/
      n=n1(k)                                                                   !/*ma         68*/
      do 10 l=1,n                                                               !/*ma         69*/
      iiip=izap1(l,k)                                                           !/*ma         70*/
      ja3(l)=ja(iiip)                                                           !/*ma         71*/
      jz3(l)=jz(iiip)                                                           !/*ma         72*/
      il3(l)=1                                                                  !/*ma         73*/
   10 continue                                                                  !/*ma         74*/
      jjix=klev1(k)                                                             !/*ma         75*/
      ix=mod(jjix,3)+1                                                          !/*ma         76*/
      il3(ix)=jjix/3+1                                                          !/*ma         77*/
      write (lout,120) prob,q,(jz3(l),ja3(l),il3(l),l=1,n)                      !/*ma         78*/
   20 continue                                                                  !/*ma         79*/
      go to 40                                                                  !/*ma         80*/
   30 continue                                                                  !/*ma         81*/
      wtl(i)=dp1                                                                !/*ma         82*/
      if (ixedt.ne.0) write (lout,80) e(i),aj(i),parity(i),tiso(i),wtl(i        !/*ma         83*/
     & )                                                                        !/*ma         84*/
   40 continue                                                                  !/*ma         85*/
   50 continue                                                                  !/*ma         86*/
   60 continue                                                                  !/*ma         87*/
      return                                                                    !/*ma         88*/
!                                                                               !/*ma         89*/
   70 format (/5x,'z =',i3,'    a =',i3,'    mass excess =',f10.5,              !/*ma         90*/
     & '    number of states =',i3,'    r(coul) =',f6.2,'    r(int) =',         !/*ma         91*/
     & f6.2)                                                                    !/*ma         92*/
   80 format (10x,'ex =',f10.5,'  j =',f4.1,'  pi = ',f4.1,'  t =',f4.1,        !/*ma         93*/
     &'  wtl =',g11.5)                                                          !/*ma         94*/
   90 format (/5x,'z =',i3,'    a =',i3,'    not tabulated')                    !/*ma         95*/
  100 format (1h1,25x,'edit of mass data input'//)                              !/*ma         96*/
  110 format (10x,'ex =',f10.5,'  j =',f4.1,'  pi = ',f4.1,'  t =',f4.1,        !/*ma         97*/
     &'  wtl =',g11.5,'   unstable -',i4,' channels')                           !/*ma         98*/
  120 format (15x,'prob=',g12.5,'  q=',g12.5,4(' -- ',3i3))                     !/*ma         99*/
      end                                                                       !/*ma        100*/
