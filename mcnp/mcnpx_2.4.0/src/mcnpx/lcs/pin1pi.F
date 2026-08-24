!     $Id: pin1pi.F,v 1.4 2002/03/27 18:15:15 gwm Exp $                       !/*pi          1*/
      subroutine pin1pi (ecm,id1in,id2in,id1out,id2out,misobr,mpart,df)         !/*pi          2*/
#include <lcs/double.h>                                                         /*!pi          3*/
!     ==================================================================        !/*pi          4*/
!     pin1pi determines the mass and type of isobar (misobr and id1out)         !/*pi          5*/
!     and the mass and type of pion (mpart and id2out) in a pion-               !/*pi          6*/
!     nucleon collision between a pion of type, id1in, and a nucleon            !/*pi          7*/
!     of type, id2in, with total c.m. energy, ecm.  the choice of               !/*pi          8*/
!     isobar type is based on the assumption of the formation                   !/*pi          9*/
!     of a (3/2,3/2) isobar and the conservation of isotopic spin in            !/*pi         10*/
!     the collision.the model is patterned after that discussed by              !/*pi         11*/
!     S.Lindenbaum and R.Sternheimer, phys.rev.109,1723(1958)                   !/*pi         12*/
!     for single pion production.                                               !/*pi         13*/
!                                                                               !/*pi         14*/
! PIN1PI calls   ERRORF  FLTRNF  PHASE  PTOTAL                                  !/*pi         15*/
! called by GAMMA                                                               !/*pi         16*/
!     ==================================================================        !/*pi         17*/
#include <lcs/inout.h>                                                          /*!pi         18*/
!                                                                               !/*pi         19*/
#ifdef CHEAP                                                                    /*!pi         20*/
      double precision id1in, id2in, id1out, id2out, misobr, mpart,             !/*pi         21*/
     & mimin, mimax, mmax, mn, mpi0, mpichg                                     !/*pi         22*/
#else                                                                           /*!pi         23*/
      real id1in, id2in, id1out, id2out, misobr, mpart, mimin, mimax,           !/*pi         24*/
     & mmax, mn, mpi0, mpichg                                                   !/*pi         25*/
#endif                                                                          /*!pi         26*/
      data mpi0, mpichg, mimin, mimax /134.96d0,139.567d0,1080.d0,1573          !/*pi         27*/
     & .d0/                                                                     !/*pi         28*/
!                                                                               !/*pi         29*/
! first decide what type of collision is occurring and then decide              !/*pi         30*/
! what type isobar and pion result from the collision                           !/*pi         31*/
!                                                                               !/*pi         32*/
      df=dp0                                                                    !/*pi         33*/
      if (id1in.eq.dp3) go to 10                                                !/*pi         34*/
      if (id1in.eq.dp4) go to 60                                                !/*pi         35*/
      if (id1in.eq.dp5) go to 120                                               !/*pi         36*/
      write (io,190) id1in                                                      !/*pi         37*/
      call expirx(1,'PIN1PI','id1in.eq.dp3')                                    !/*pi         38*/
   10 continue                                                                  !/*pi         39*/
      if (id2in.eq.dp2) go to 30                                                !/*pi         40*/
! this is a pi+proton collision                                                 !/*pi         41*/
      eta=rang()                                                                !/*pi         42*/
      if (eta.gt..6d0) go to 20                                                 !/*pi         43*/
! an i++ and a pi0 have been created                                            !/*pi         44*/
      id1out=13.d0                                                              !/*pi         45*/
      id2out=dp4                                                                !/*pi         46*/
      mpart=mpi0                                                                !/*pi         47*/
      go to 170                                                                 !/*pi         48*/
   20 continue                                                                  !/*pi         49*/
! an i+ and a pi+ have been created                                             !/*pi         50*/
      id1out=14.d0                                                              !/*pi         51*/
      id2out=dp3                                                                !/*pi         52*/
      mpart=mpichg                                                              !/*pi         53*/
      go to 170                                                                 !/*pi         54*/
   30 continue                                                                  !/*pi         55*/
! this is a pi+ neutron collision                                               !/*pi         56*/
      eta=rang()                                                                !/*pi         57*/
      if (eta.gt..2701393886d0) go to 40                                        !/*pi         58*/
! an i0 and a pi+ have been created                                             !/*pi         59*/
      id1out=24.d0                                                              !/*pi         60*/
      id2out=dp3                                                                !/*pi         61*/
      mpart=mpichg                                                              !/*pi         62*/
      go to 170                                                                 !/*pi         63*/
   40 continue                                                                  !/*pi         64*/
      if (eta.gt..6222428337d0) go to 50                                        !/*pi         65*/
! an i+ and a pi0 have been created                                             !/*pi         66*/
      id1out=14.d0                                                              !/*pi         67*/
      id2out=dp4                                                                !/*pi         68*/
      mpart=mpi0                                                                !/*pi         69*/
      df=-dp1                                                                   !/*pi         70*/
      go to 170                                                                 !/*pi         71*/
   50 continue                                                                  !/*pi         72*/
! an i++ and and a pi- have been created                                        !/*pi         73*/
      id1out=13.d0                                                              !/*pi         74*/
      id2out=dp5                                                                !/*pi         75*/
      mpart=mpichg                                                              !/*pi         76*/
      df=-dp1                                                                   !/*pi         77*/
      go to 170                                                                 !/*pi         78*/
   60 continue                                                                  !/*pi         79*/
      eta=rang()                                                                !/*pi         80*/
      if (eta.gt..6643997206d0) go to 80                                        !/*pi         81*/
      if (id2in.eq.dp2) go to 70                                                !/*pi         82*/
! this is a pi0-proton collision giving an i++ and a pi-                        !/*pi         83*/
      id1out=13.d0                                                              !/*pi         84*/
      id2out=dp5                                                                !/*pi         85*/
      mpart=mpichg                                                              !/*pi         86*/
      go to 170                                                                 !/*pi         87*/
   70 continue                                                                  !/*pi         88*/
! this is a pi0-neutron collision giving an i- and a pi+                        !/*pi         89*/
      id1out=25.d0                                                              !/*pi         90*/
      id2out=dp3                                                                !/*pi         91*/
      mpart=mpichg                                                              !/*pi         92*/
      go to 170                                                                 !/*pi         93*/
   80 continue                                                                  !/*pi         94*/
      if (eta.gt..8708624797d0) go to 100                                       !/*pi         95*/
      if (id2in.eq.dp2) go to 90                                                !/*pi         96*/
! this is a pi0-proton collision giving an i+ and a pi0                         !/*pi         97*/
      id1out=14.d0                                                              !/*pi         98*/
      id2out=dp4                                                                !/*pi         99*/
      mpart=mpi0                                                                !/*pi        100*/
      go to 170                                                                 !/*pi        101*/
   90 continue                                                                  !/*pi        102*/
! this is a pi0-neutron collision giving an i0 and a pi0                        !/*pi        103*/
      id1out=24.d0                                                              !/*pi        104*/
      id2out=dp4                                                                !/*pi        105*/
      mpart=mpi0                                                                !/*pi        106*/
      go to 170                                                                 !/*pi        107*/
  100 continue                                                                  !/*pi        108*/
      if (id2in.eq.dp2) go to 110                                               !/*pi        109*/
! this is a pi0-proton collision giving an i0 and a pi+                         !/*pi        110*/
      id1out=24.d0                                                              !/*pi        111*/
      id2out=dp3                                                                !/*pi        112*/
      mpart=mpichg                                                              !/*pi        113*/
      df=dp1                                                                    !/*pi        114*/
      go to 170                                                                 !/*pi        115*/
  110 continue                                                                  !/*pi        116*/
! this is a pi0-neutron collision giving an i+ and a pi-                        !/*pi        117*/
      id1out=14.d0                                                              !/*pi        118*/
      id2out=dp5                                                                !/*pi        119*/
      mpart=mpichg                                                              !/*pi        120*/
      df=-dp1                                                                   !/*pi        121*/
      go to 170                                                                 !/*pi        122*/
  120 continue                                                                  !/*pi        123*/
      if (id2in.eq.dp1) go to 140                                               !/*pi        124*/
! this is a pi- neutron collision                                               !/*pi        125*/
      eta=rang()                                                                !/*pi        126*/
      if (eta.gt..6d0) go to 130                                                !/*pi        127*/
! an i- and a pi0 have been created                                             !/*pi        128*/
      id1out=25.d0                                                              !/*pi        129*/
      id2out=dp4                                                                !/*pi        130*/
      mpart=mpi0                                                                !/*pi        131*/
      go to 170                                                                 !/*pi        132*/
  130 continue                                                                  !/*pi        133*/
! an i0 and a pi- have been created                                             !/*pi        134*/
      id1out=24.d0                                                              !/*pi        135*/
      id2out=dp5                                                                !/*pi        136*/
      mpart=mpichg                                                              !/*pi        137*/
      go to 170                                                                 !/*pi        138*/
  140 continue                                                                  !/*pi        139*/
! this is a pi- proton collision                                                !/*pi        140*/
      eta=rang()                                                                !/*pi        141*/
      if (eta.gt..2701393886d0) go to 150                                       !/*pi        142*/
! an i+ and a pi- have been created                                             !/*pi        143*/
      id1out=14.d0                                                              !/*pi        144*/
      id2out=dp5                                                                !/*pi        145*/
      mpart=mpichg                                                              !/*pi        146*/
      go to 170                                                                 !/*pi        147*/
  150 continue                                                                  !/*pi        148*/
      if (eta.gt..6222428337d0) go to 160                                       !/*pi        149*/
! an i0 and a pi0 have been created                                             !/*pi        150*/
      id1out=24.d0                                                              !/*pi        151*/
      id2out=dp4                                                                !/*pi        152*/
      mpart=mpi0                                                                !/*pi        153*/
      df=dp1                                                                    !/*pi        154*/
      go to 170                                                                 !/*pi        155*/
  160 continue                                                                  !/*pi        156*/
! an i- and a pi+ have been created                                             !/*pi        157*/
      id1out=25.d0                                                              !/*pi        158*/
      id2out=dp3                                                                !/*pi        159*/
      mpart=mpichg                                                              !/*pi        160*/
      df=dp1                                                                    !/*pi        161*/
      go to 170                                                                 !/*pi        162*/
  170 continue                                                                  !/*pi        163*/
! now select the mass of the isobar                                             !/*pi        164*/
      mn=mpart                                                                  !/*pi        165*/
      mmax=ecm-mn                                                               !/*pi        166*/
      if (mmax.gt.mimax) mmax=mimax                                             !/*pi        167*/
      rangem=mmax-mimin                                                         !/*pi        168*/
      pmax=phase(ecm,mn,mimin)*2.d+02                                           !/*pi        169*/
!                                                                               !/*pi        170*/
!        fix to avoid zero-divides of marginal isobars                          !/*pi        171*/
!        ie in call to "phase/pmax", when pmax =0  [mrc 12 june 83]             !/*pi        172*/
!                                                                               !/*pi        173*/
      misobr=mimin                                                              !/*pi        174*/
      if (pmax.le.dp0) return                                                   !/*pi        175*/
!                                                                               !/*pi        176*/
! the max of p(mi) is less than or equal (max.sig33)*(max.phase)                !/*pi        177*/
! for a given ecm.sig33 is the pi+ + proton cross section.                      !/*pi        178*/
! now select the isobar mass by the von neuman rejection method.                !/*pi        179*/
  180 continue                                                                  !/*pi        180*/
      eta1=rang()                                                               !/*pi        181*/
      eta2=rang()                                                               !/*pi        182*/
      misobr=mimin+rangem*eta1                                                  !/*pi        183*/
      tcm=misobr-1077.811d0                                                     !/*pi        184*/
      izero=0                                                                   !/*pi        185*/
      ione=1                                                                    !/*pi        186*/
      call ptotal (tcm,ione,izero,izero,sig33,absor)                            !/*pi        187*/
      pm=sig33*phase(ecm,mn,misobr)/pmax                                        !/*pi        188*/
      if (eta2.gt.pm) go to 180                                                 !/*pi        189*/
      return                                                                    !/*pi        190*/
!                                                                               !/*pi        191*/
  190 format ('0the following pi code is not valid',f10.5)                      !/*pi        192*/
      end                                                                       !/*pi        193*/
