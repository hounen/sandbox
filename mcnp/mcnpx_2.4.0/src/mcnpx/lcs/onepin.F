!     $Id: onepin.F,v 1.4 2002/03/27 18:15:12 gwm Exp $                       !/*on          1*/
      subroutine onepin (ecm,id1in,id2in,id1out,id2out,misobr,mpart)            !/*on          2*/
#include <lcs/double.h>                                                         /*!on          3*/
!     ==================================================================        !/*on          4*/
!     onepin determines the mass and type of isobar(misobr and id1out)          !/*on          5*/
!     and the mass and type of nucleon(mpart and id2out) in a nucleon-          !/*on          6*/
!     nucleon collision between the two types of nucleons (id1in                !/*on          7*/
!     and id2in) with total c.m. energy,ecm.the choice of isobar type           !/*on          8*/
!     is based on the assumption of the formation of a (3/2,3/2)                !/*on          9*/
!     isobar and the conservation of isotopic spin in the collision.            !/*on         10*/
!     The model is patterned after that discussed by S.Lindenbaum               !/*on         11*/
!     and R.Sternheimer,phys.rev.105,1874(1957) for single                      !/*on         12*/
!     pion production.                                                          !/*on         13*/
!                                                                               !/*on         14*/
! ONEPIN calls   FLTRNF  PHASE  PTOTAL                                          !/*on         15*/
! called by GAMMA                                                               !/*on         16*/
!     ==================================================================        !/*on         17*/
!                                                                               !/*on         18*/
#ifdef CHEAP                                                                    /*!on         19*/
      double precision id1in, id2in, id1out, id2out, misobr, mpart,             !/*on         20*/
     & massn, massp, mimin, mimax, mmax, mn                                     !/*on         21*/
#else                                                                           /*!on         22*/
      real id1in, id2in, id1out, id2out, misobr, mpart, massn, massp,           !/*on         23*/
     & mimin, mimax, mmax, mn                                                   !/*on         24*/
#endif                                                                          /*!on         25*/
!                                                                               !/*on         26*/
      data massn, massp, mimin, mimax /939.516d0,938.223d0,1080.d0,1573         !/*on         27*/
     & .d0/                                                                     !/*on         28*/
!                                                                               !/*on         29*/
! first decide what type of collision is occurring and then decide              !/*on         30*/
! what type isobar and nucleon result from the collision.                       !/*on         31*/
      if (id1in.ne.id2in) go to 40                                              !/*on         32*/
      if (id1in.eq.dp1) go to 20                                                !/*on         33*/
! a neutron-neutron collision occurred                                          !/*on         34*/
      eta=rang()                                                                !/*on         35*/
      if (eta.le..75d0) go to 10                                                !/*on         36*/
! an i0 and a neutron have been created                                         !/*on         37*/
      id1out=24.d0                                                              !/*on         38*/
      id2out=dp2                                                                !/*on         39*/
      mpart=massn                                                               !/*on         40*/
      go to 60                                                                  !/*on         41*/
   10 continue                                                                  !/*on         42*/
! an i- and a proton have been created                                          !/*on         43*/
      id1out=25.d0                                                              !/*on         44*/
      id2out=dp1                                                                !/*on         45*/
      mpart=massp                                                               !/*on         46*/
      go to 60                                                                  !/*on         47*/
   20 continue                                                                  !/*on         48*/
! a proton-proton collision occurred                                            !/*on         49*/
      eta=rang()                                                                !/*on         50*/
      if (eta.le..75d0) go to 30                                                !/*on         51*/
! an i+ and a proton have been created                                          !/*on         52*/
      id1out=14.d0                                                              !/*on         53*/
      id2out=dp1                                                                !/*on         54*/
      mpart=massp                                                               !/*on         55*/
      go to 60                                                                  !/*on         56*/
   30 continue                                                                  !/*on         57*/
! an i++ and a neutron have been created                                        !/*on         58*/
      id1out=13.d0                                                              !/*on         59*/
      id2out=dp2                                                                !/*on         60*/
      mpart=massn                                                               !/*on         61*/
      go to 60                                                                  !/*on         62*/
   40 continue                                                                  !/*on         63*/
! a neutron-proton collision occurred                                           !/*on         64*/
      eta=rang()                                                                !/*on         65*/
      if (eta.le.dph) go to 50                                                  !/*on         66*/
! an i0 and a proton have been created                                          !/*on         67*/
      id1out=24.d0                                                              !/*on         68*/
      id2out=dp1                                                                !/*on         69*/
      mpart=massp                                                               !/*on         70*/
      go to 60                                                                  !/*on         71*/
   50 continue                                                                  !/*on         72*/
! an i+ and a neutron have been created                                         !/*on         73*/
      id1out=14.d0                                                              !/*on         74*/
      id2out=dp2                                                                !/*on         75*/
      mpart=massn                                                               !/*on         76*/
   60 continue                                                                  !/*on         77*/
! now select the mass of the isobar                                             !/*on         78*/
      mn=mpart                                                                  !/*on         79*/
      mmax=ecm-mn                                                               !/*on         80*/
      if (mmax.gt.mimax) mmax=mimax                                             !/*on         81*/
      rangem=mmax-mimin                                                         !/*on         82*/
      pmax=phase(ecm,mn,mimin)*2.d+02                                           !/*on         83*/
!                                                                               !/*on         84*/
!        fix to avoid zero-divides of marginal isobars                          !/*on         85*/
!        ie in call to "phase/pmax", when pmax =0  [mrc 12 june 83]             !/*on         86*/
!                                                                               !/*on         87*/
      misobr=mimin                                                              !/*on         88*/
      if (pmax.le.dp0) return                                                   !/*on         89*/
!                                                                               !/*on         90*/
! the max of p(mi) is less than or equal (max.sig33)*(max.phase)                !/*on         91*/
! for a given ecm.sig33 is the pi+ + proton cross section.                      !/*on         92*/
! now select the isobar mass by the von neuman rejection method.                !/*on         93*/
   70 continue                                                                  !/*on         94*/
      eta1=rang()                                                               !/*on         95*/
      eta2=rang()                                                               !/*on         96*/
      misobr=mimin+rangem*eta1                                                  !/*on         97*/
      tcm=misobr-1077.811d0                                                     !/*on         98*/
      izero=0                                                                   !/*on         99*/
      ione=1                                                                    !/*on        100*/
      call ptotal (tcm,ione,izero,izero,sig33,absor)                            !/*on        101*/
      pm=sig33*phase(ecm,mn,misobr)/pmax                                        !/*on        102*/
      if (eta2.gt.pm) go to 70                                                  !/*on        103*/
      return                                                                    !/*on        104*/
      end                                                                       !/*on        105*/
