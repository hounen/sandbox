!     $Id: cascad.F,v 1.8 2002/03/27 18:14:56 gwm Exp $                       !/*ca          1*/
      subroutine cascad (ecl,name,med,emn,                                      !/*ca          2*/
#ifdef CEM                                                                      /*!ca          3*/
     & iccem,                                                                   !/*ca          4*/
#endif                                                                          /*!ca          5*/
     & a1,z1,idt,fmf1,hsigg,xsiso,lmc,lmxs_)                                    !/*ca          6*/
#include <lcs/double.h>                                                         /*!ca          7*/
!     ==================================================================        !/*ca          8*/
!     cascad   calls the following subroutines and functions                    !/*ca          9*/
!        BCASC      ELSCAT     ERROR      HYD        PDEVNT                     !/*ca         10*/
!     ==================================================================        !/*ca         11*/
!.......................................................................        !/*ca         12*/
!     ityp    particle      sigg index    hbr value                             !/*ca         13*/
!      1      proton             1            t                                 !/*ca         14*/
!      2      neutron            1            t                                 !/*ca         15*/
!      3      pi+                1            t                                 !/*ca         16*/
!      4      pi0                1            f                                 !/*ca         17*/
!      5      pi-                1            t                                 !/*ca         18*/
!      6      mu+                             f                                 !/*ca         19*/
!      7      mu-                             f                                 !/*ca         20*/
!      8      d                  3            t                                 !/*ca         21*/
!      9      t                  4            t                                 !/*ca         22*/
!     10      he-3               5            t                                 !/*ca         23*/
!     11      alpha              6            t                                 !/*ca         24*/
!     12      photon                          f                                 !/*ca         25*/
!     13      k+                              t                                 !/*ca         26*/
!     14      k0 long                         t                                 !/*ca         27*/
!     15      k0 short                        t                                 !/*ca         28*/
!     16      k-                              t                                 !/*ca         29*/
!     17      p-bar              2            t                                 !/*ca         30*/
!     18      n-bar              2            t                                 !/*ca         31*/
!.......................................................................        !/*ca         32*/
      logical lmc                                                               !/*ca         33*/
      external cabd                                                             !/*ca         34*/
#include <lcs/inout.h>                                                          /*!ca         35*/
#include <lcs/lptS.h>                                                           /*!ca         36*/
#include <lcs/rucom.h>                                                          /*!ca         37*/
#include <lcs/cacom.h>                                                          /*!ca         38*/
#include <lcs/banka.h>                                                          /*!ca         39*/
#ifdef CEM                                                                      /*!ca         40*/
#include <lcs/fltolh.h>                                                         /*!ca         41*/
#include <lcs/ercom.h>                                                          /*!ca         42*/
#include <fluka89/part1.h>                                                      /*!ca         43*/
#endif                                                                          /*!ca         44*/
      dimension hsigg(lpt,1), xsiso(2,1)                                        !/*ca         45*/
      dimension itt(lpt)                                                        !/*ca         46*/
      data itt /5*1,0,0,3,4,5,6,0,4*1,2,2,4*0/                                  !/*ca         47*/
      data lx /200/, lx1 /20/                                                   !/*ca         48*/
!     ************************************************************              !/*ca         49*/
#ifdef CEM                                                                      /*!ca         50*/
      iccem = 0                                                                 !/*ca         51*/
#endif                                                                          /*!ca         52*/
      lelem0=lelem                                                              !/*ca         53*/
      uu=dp0                                                                    !/*ca         54*/
      erec=dp0                                                                  !/*ca         55*/
      if (itt(ityp).eq.0) call expirx(1,'CASCAD','itt(ityp).eq.0')              !/*ca         56*/
      ielevt=0                                                                  !/*ca         57*/
      ehicut=ehipi                                                              !/*ca         58*/
      if (ityp.le.2) ehicut=ehin                                                !/*ca         59*/
      ihie=1                                                                    !/*ca         60*/
      if (ecl.le.ehicut) ihie=0                                                 !/*ca         61*/
      if (ityp.ne.5.or.ecl.gt.emn) go to 10                                     !/*ca         62*/
!                                                                               !/*ca         63*/
!     ************************************************************              !/*ca         64*/
!                                                                               !/*ca         65*/
! --- Pi- capture                                                               !/*ca         66*/
!                                                                               !/*ca         67*/
      nopart=-1                                                                 !/*ca         68*/
      if (lelem.eq.-1) call expirx(1,'CASCAD','lelem.eq.-1')                    !/*ca         69*/
      if (iexisa.ge.2) go to 100                                                !/*ca         70*/
      go to 90                                                                  !/*ca         71*/
!                                                                               !/*ca         72*/
!     ************************************************************              !/*ca         73*/
!                                                                               !/*ca         74*/
! --- Coulomb barrier                                                           !/*ca         75*/
!                                                                               !/*ca         76*/
   10 continue                                                                  !/*ca         77*/
      if (.not.lmc.and.                                                         !/*ca         78*/
     &    jcoul.ne.0.and.charge(ityp).gt.dp0.and.lelem.ne.-1) then              !/*ca         79*/
        call coulmb (ecl,z1,a1,iflag)                                           !/*ca         80*/
        if (iflag.ne.0) go to 130                                               !/*ca         81*/
      endif                                                                     !/*ca         82*/
!                                                                               !/*ca         83*/
!     ************************************************************              !/*ca         84*/
!                                                                               !/*ca         85*/
! --- Select interaction model                                                  !/*ca         86*/
!                                                                               !/*ca         87*/
      ncnt=0                                                                    !/*ca         88*/
   20 continue                                                                  !/*ca         89*/
      ncnt=ncnt+1                                                               !/*ca         90*/
      go to (50,50,50,50,50,130,130,30,30,30,30,130,40,40,40,40,40,40,          !/*ca         91*/
     & 130,130,130,130), ityp                                                   !/*ca         92*/
   30 continue                                                                  !/*ca         93*/
!                                                                               !/*ca         94*/
! --- Ions                                                                      !/*ca         95*/
!                                                                               !/*ca         96*/
      if (iexisa.le.0) go to 130                                                !/*ca         97*/
      if (ityp.eq.11) then                                                      !/*ca         98*/
        e1=4000.d0                                                              !/*ca         99*/
      elseif (ityp.eq.8) then                                                   !/*ca        100*/
        e1=2000.d0                                                              !/*ca        101*/
      else                                                                      !/*ca        102*/
        e1=3000.d0                                                              !/*ca        103*/
      endif                                                                     !/*ca        104*/
      if (ecl.gt.e1) go to 130                                                  !/*ca        105*/
      go to 100                                                                 !/*ca        106*/
   40 continue                                                                  !/*ca        107*/
!                                                                               !/*ca        108*/
! --- Kaons and antinucleons                                                    !/*ca        109*/
!                                                                               !/*ca        110*/
      if (iexisa.le.0) then                                                     !/*ca        111*/
        if (ecl.le.60.d0) go to 130                                             !/*ca        112*/
        go to 110                                                               !/*ca        113*/
      endif                                                                     !/*ca        114*/
      go to 70                                                                  !/*ca        115*/
!                                                                               !/*ca        116*/
   50 continue                                                                  !/*ca        117*/
! --- Nucleons and pions                                                        !/*ca        118*/
!                                                                               !/*ca        119*/
      if (ityp.eq.4) then                                                       !/*ca        120*/
!        In the current version, do not transport pi0 at all.                   !/*ca        121*/
         go to 130                                                              !/*ca        122*/
      endif                                                                     !/*ca        123*/
!                                                                               !/*ca        124*/
      if (iexisa.ge.2) go to 70                                                 !/*ca        125*/
   60 continue                                                                  !/*ca        126*/
      it=0                                                                      !/*ca        127*/
      if (ityp.gt.2) it=2                                                       !/*ca        128*/
      e1=flenb(it+1)                                                            !/*ca        129*/
      e2=flenb(it+2)                                                            !/*ca        130*/
      if (ecl.le.e1) go to 90                                                   !/*ca        131*/
      if (ecl.ge.e2) go to 110                                                  !/*ca        132*/
      wbcasc=(ecl-e1)/(e2-e1)                                                   !/*ca        133*/
      if (rang().le.wbcasc) go to 110                                           !/*ca        134*/
      go to 90                                                                  !/*ca        135*/
   70 continue                                                                  !/*ca        136*/
      if (ecl.le.flenb(5)) go to 100                                            !/*ca        137*/
      if (ecl.ge.flenb(6)) go to 80                                             !/*ca        138*/
      wbcasc=(ecl-flenb(5))/(flenb(6)-flenb(5))                                 !/*ca        139*/
      if (rang().le.wbcasc) go to 80                                            !/*ca        140*/
      go to 100                                                                 !/*ca        141*/
   80 continue                                                                  !/*ca        142*/
      if (ityp.le.3.or.ityp.eq.5) go to 60                                      !/*ca        143*/
      go to 110                                                                 !/*ca        144*/
!                                                                               !/*ca        145*/
! --- Bertini model or CEM model.                                               !/*ca        146*/
!                                                                               !/*ca        147*/
   90 continue                                                                  !/*ca        148*/
      if (lelem.eq.-1) then                                                     !/*ca        149*/
!                                                                               !/*ca        150*/
!        hi-enegry H1 interaction force to FLUKA model.                         !/*ca        151*/
         if (ihie.ne.0) go to 110                                               !/*ca        152*/
!                                                                               !/*ca        153*/
         call hyd (ecl,fmf1,hsigg(ityp,med),lmxs_)                              !/*ca        154*/
#ifdef CEM                                                                      /*!ca        155*/
      else if (icem.ne.0.and.a1.ge.dp12.and.z1.ge.dp6) then                     !/*ca        156*/
!        Use the CEM model.                                                     !/*ca        157*/
         iccem = 1                                                              !/*ca        158*/
         cm0d = pmass(ityp)                                                     !/*ca        159*/
         me0d = charge(ityp)                                                    !/*ca        160*/
         mb0d = ibar(itypfl(ityp))                                              !/*ca        161*/
         rand=rang()                                                            !/*ca        162*/
#ifdef OLDM                                                                     /*!ca        163*/
!        QUESTION: The new version eliminates the line "ihie=0"                 !/*ca        164*/
!                  Is it known never to be needed?                              !/*ca        165*/
         ihie=0                                                                 !/*ca        166*/
#endif                                                                          /*!ca        167*/
         call ccasc(cm0d,ecl,a1,z1,rand,me0d,mb0d,nofis,nell,exc0)              !/*ca        168*/
         if(nell.gt.0) nopart=-1                                                !/*ca        169*/
#endif                                                                          /*!ca        170*/
      else                                                                      !/*ca        171*/
!        Use the Bertini model.                                                 !/*ca        172*/
         call bcasc (ecl,med,emn,a1,z1)                                         !/*ca        173*/
      endif                                                                     !/*ca        174*/
      if (lmc.and.nopart.eq.-1) then                                            !/*ca        175*/
        if (ncnt.gt.lx.or.ityp.ge.8.and.ityp.le.11.and.ncnt.gt.lx1) then        !/*ca        176*/
          write (itty,140) ityp,ecl,a1,z1                                       !/*ca        177*/
          write (io,140) ityp,ecl,a1,z1                                         !/*ca        178*/
        else                                                                    !/*ca        179*/
          lelem=lelem0                                                          !/*ca        180*/
          go to 20                                                              !/*ca        181*/
        endif                                                                   !/*ca        182*/
      endif                                                                     !/*ca        183*/
      return                                                                    !/*ca        184*/
!                                                                               !/*ca        185*/
!     ************************************************************              !/*ca        186*/
!                                                                               !/*ca        187*/
! --- ISABEL model                                                              !/*ca        188*/
!                                                                               !/*ca        189*/
  100 continue                                                                  !/*ca        190*/
      if (lelem.eq.-1) then                                                     !/*ca        191*/
        idt=1                                                                   !/*ca        192*/
      endif                                                                     !/*ca        193*/
      if (ityp.ge.8.and.ityp.le.11.and.ecl.lt.dp3*pmass(ityp)/pmass(1))         !/*ca        194*/
     & then                                                                     !/*ca        195*/
        nopart=-1                                                               !/*ca        196*/
        return                                                                  !/*ca        197*/
      endif                                                                     !/*ca        198*/
      call hcasc (ecl,med,emn,a1,z1,idt,lmc)                                    !/*ca        199*/
      if (lmc.and.nopart.eq.-1) then                                            !/*ca        200*/
        if (ncnt.gt.lx.or.ityp.ge.8.and.ityp.le.11.and.ncnt.gt.lx1) then        !/*ca        201*/
          write (itty,150) ityp,ecl,a1,z1                                       !/*ca        202*/
          write (io,150) ityp,ecl,a1,z1                                         !/*ca        203*/
        else                                                                    !/*ca        204*/
          lelem=lelem0                                                          !/*ca        205*/
          go to 20                                                              !/*ca        206*/
        endif                                                                   !/*ca        207*/
      endif                                                                     !/*ca        208*/
      return                                                                    !/*ca        209*/
!                                                                               !/*ca        210*/
!     ************************************************************              !/*ca        211*/
!                                                                               !/*ca        212*/
! --- FLUKA model                                                               !/*ca        213*/
!                                                                               !/*ca        214*/
  110 continue                                                                  !/*ca        215*/
      ihie=0                                                                    !/*ca        216*/
#ifdef LCS                                                                      /*!ca        217*/
      call fcasc (ecl,a1,z1)                                                    !/*ca        218*/
#else                                                                           /*!ca        219*/
      geom_xs=(xsiso(1,iexp)-xsiso(2,iexp))/fmf1                                !/*ca        220*/
      call fcasc (ecl,a1,z1,geom_xs,lmxs_)                                      !/*ca        221*/
#endif                                                                          /*!ca        222*/
      if (lmc.and.nopart.eq.-1) then                                            !/*ca        223*/
        if (ncnt.gt.lx.or.ityp.ge.8.and.ityp.le.11.and.ncnt.gt.lx1) then        !/*ca        224*/
          write (itty,160) ityp,ecl,a1,z1                                       !/*ca        225*/
          write (io,160) ityp,ecl,a1,z1                                         !/*ca        226*/
        else                                                                    !/*ca        227*/
          lelem=lelem0                                                          !/*ca        228*/
          go to 20                                                              !/*ca        229*/
        endif                                                                   !/*ca        230*/
      endif                                                                     !/*ca        231*/
      return                                                                    !/*ca        232*/
!                                                                               !/*ca        233*/
!     ************************************************************              !/*ca        234*/
!                                                                               !/*ca        235*/
! --- Transparency (pseudocollision)                                            !/*ca        236*/
!                                                                               !/*ca        237*/
  130 continue                                                                  !/*ca        238*/
      nopart=-1                                                                 !/*ca        239*/
      return                                                                    !/*ca        240*/
!                                                                               !/*ca        241*/
  140 format (' unable to interact in BCASC/HYD: ityp=',i2,' energy=',          !/*ca        242*/
     & f10.3,' a=',f4.0,' z=',f4.0)                                             !/*ca        243*/
  150 format (' unable to interact in HCASC: ityp=',i2,' energy=',              !/*ca        244*/
     & f10.3,' a=',f4.0,' z=',f4.0)                                             !/*ca        245*/
  160 format (' unable to interact in FCASC: ityp=',i2,' energy=',              !/*ca        246*/
     & f10.3,' a=',f4.0,' z=',f4.0)                                             !/*ca        247*/
      end                                                                       !/*ca        248*/
