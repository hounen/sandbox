!     $Id: bx.F,v 1.6 2002/03/27 18:16:23 gwm Exp $                           !/*bx          1*/
      subroutine bx (tlly1,tlsq1,hold1,ergb,xmu,itype)                          !/*bx          2*/
! binning routine for program XSEX                                              !/*bx          3*/
#include <htape3x/com32b.h>                                                     /*!bx          4*/
#include <htape3x/label.h>                                                      /*!bx          5*/
#include <htape3x/contrl.h>                                                     /*!bx          6*/
#include <htape3x/exm.h>                                                        /*!bx          7*/
#include <xsex3/charxs.h>                                                       /*!bx          8*/
      dimension hold1(1), tlly1(1), tlsq1(1), ergb(1), xmu(1), itype(1),        !/*bx          9*/
     & nc3(8), ikb(lpt)                                                         !/*bx         10*/
      data nc3 /0,1,0,0,0,1,0,0/                                                !/*bx         11*/
      data ikb /1,2,3,4,5,0,0,6,7,8,9,10,11,12,12,13,14,15,4*0/                 !/*bx         12*/
   10 continue                                                                  !/*bx         13*/
      call rednmt                                                               !/*bx         14*/
      if (ncol.gt.0) then                                                       !/*bx         15*/
!        event record read                                                      !/*bx         16*/
        if (nocas.ne.nocas0) go to 60                                           !/*bx         17*/
        go to 80                                                                !/*bx         18*/
      endif                                                                     !/*bx         19*/
   20 continue                                                                  !/*bx         20*/
      mgo=ncol+5                                                                !/*bx         21*/
      go to (60,30,40,50,40), mgo                                               !/*bx         22*/
   30 continue                                                                  !/*bx         23*/
! end of batch                                                                  !/*bx         24*/
      nocas0=0                                                                  !/*bx         25*/
      go to 10                                                                  !/*bx         26*/
   40 continue                                                                  !/*bx         27*/
! ncol not used                                                                 !/*bx         28*/
      call errorf ('bx.1    ')                                                  !/*bx         29*/
   50 continue                                                                  !/*bx         30*/
! start of run                                                                  !/*bx         31*/
      if (n1col.ne.-1.or.mxmat.ne.1) call errorf ('bx.2    ')                   !/*bx         32*/
      call rednmt                                                               !/*bx         33*/
      ktip=tip+1.0                                                              !/*bx         34*/
      nocas0=nocas                                                              !/*bx         35*/
      go to 80                                                                  !/*bx         36*/
   60 continue                                                                  !/*bx         37*/
! update tally arrays                                                           !/*bx         38*/
      do 70 i=1,nsize(ncase+1)/ndp2                                             !/*bx         39*/
      if (hold1(i).eq.dp0) go to 70                                             !/*bx         40*/
      tlly1(i)=tlly1(i)+hold1(i)                                                !/*bx         41*/
      tlsq1(i)=tlsq1(i)+hold1(i)**2                                             !/*bx         42*/
   70 hold1(i)=dp0                                                              !/*bx         43*/
      nocas0=nocas                                                              !/*bx         44*/
      if (ncol.eq.-4) return                                                    !/*bx         45*/
   80 continue                                                                  !/*bx         46*/
      if (ncol.ne.2.and.ncol.ne.6) go to 100                                    !/*bx         47*/
      call recels (ielstc)                                                      !/*bx         48*/
      del=geomxs*wt                                                             !/*bx         49*/
      tltot=tltot+del                                                           !/*bx         50*/
      tstot=tstot+del*del                                                       !/*bx         51*/
      if (ielstc.ne.1) then                                                     !/*bx         52*/
        tlnon=tlnon+del                                                         !/*bx         53*/
        tsnon=tsnon+del*del                                                     !/*bx         54*/
        zpr0=zz(mel(nmed)+lelem-1)                                              !/*bx         55*/
        if (zpr0.ge.70.d0.and.mnuc.eq.2) then                                   !/*bx         56*/
          tlfis=tlfis+del                                                       !/*bx         57*/
          tsfis=tsfis+del*del                                                   !/*bx         58*/
        endif                                                                   !/*bx         59*/
      else                                                                      !/*bx         60*/
        tlels=tlels+del                                                         !/*bx         61*/
        tsels=tsels+del*del                                                     !/*bx         62*/
      endif                                                                     !/*bx         63*/
      do 90 nc=1,ncase                                                          !/*bx         64*/
      imom=imom0(nc)                                                            !/*bx         65*/
      nerg=nnerg(nc)                                                            !/*bx         66*/
      nang=nnang(nc)                                                            !/*bx         67*/
      ntype=ntype1(nc)                                                          !/*bx         68*/
      call bx1 (iblk(iphld+nsize(nc)),nang1(nc),nerg1(nc),iblk(iperg            !/*bx         69*/
     & +lerg(nc)),iblk(ipang+lang(nc)),iblk(iptyp+ltyp(nc)),ielstc)             !/*bx         70*/
!     subroutine bx1 (hold,ndim1,ndim2,ergb,xmu,itype)                          !/*bx         71*/
   90 continue                                                                  !/*bx         72*/
  100 continue                                                                  !/*bx         73*/
      call srchn (nc3)                                                          !/*bx         74*/
      if (ncol.le.0) go to 20                                                   !/*bx         75*/
      if (nocas.ne.nocas0) go to 60                                             !/*bx         76*/
      call errorf ('bx.3    ')                                                  !/*bx         77*/
      end                                                                       !/*bx         78*/
