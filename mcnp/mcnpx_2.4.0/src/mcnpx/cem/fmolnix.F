      function fmolnix (iz, inn, itype)                                         !/*fm          1*/
! ======================================================================        !/*fm          2*/
!                                                                               !/*fm          3*/
!   This function extracts either the ground-state microscopic                  !/*fm          4*/
!   correction, for itype = 1; the experimental (or theoretical, if             !/*fm          5*/
!   Mex is not measured) mass excess for itype = 2; or the energy               !/*fm          6*/
!   shift from the Moller, Nix & Kratz calculated pairing gaps, for             !/*fm          7*/
!   itype = 3  from the tables contained in the block data subroutine           !/*fm          8*/
!   MOLLNIX. The inputs are the proton and neutron numbers, and the             !/*fm          9*/
!   value of itype.                                                             !/*fm         10*/
!                                                                               !/*fm         11*/
!   Called by: DELTAM PRECOF SHELL                                              !/*fm         12*/
!                                                                               !/*fm         13*/
!   Calls: EXCEED, FMNMACRO                                                     !/*fm         14*/
!                                                                               !/*fm         15*/
!   Written by A. J. Sierk  LANL  T-2  May, 1996.                               !/*fm         16*/
!   Modifications; July, October, 1997.                                         !/*fm         17*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*fm         18*/
!   Corrected bug (AJS); September, 1998                                        !/*fm         19*/
!                                                                               !/*fm         20*/
! ======================================================================        !/*fm         21*/
#include <cem/cdouble.h>                                                        /*!fm         22*/
#ifndef CEM                                                                     /*!fm         23*/
      fmolnix=dp0                                                               !/*fm         24*/
#else                                                                           /*!fm         25*/
#include <lcs/inout.h>                                                          /*!fm         26*/
      logical nok                                                               !/*fm         27*/
      common /pairc/  cevap, cfis                                               !/*fm         28*/
      common /emcnix/ emic1(30,27), emic2(35,13), emic3(40,17),                 !/*fm         29*/
     &                emic4(45,36)                                              !/*fm         30*/
      common /emxnix/ emx1(30,27), emx2(35,13), emx3(40,17), emx4(45,36)        !/*fm         31*/
      common /eprnix/ epair1(30,27), epair2(35,13), epair3(40,17),              !/*fm         32*/
     &                epair4(45,36)                                             !/*fm         33*/
      common /mxnix2/ emxa(15,7), nmina(7), nmaxa(7)                            !/*fm         34*/
      common /mcnix2/ emica(15,7), epaira(15,7)                                 !/*fm         35*/
      common /nsmnix/ nmin(93), nmax(93)                                        !/*fm         36*/
! ======================================================================        !/*fm         37*/
      un = inn                                                                  !/*fm         38*/
      z = iz                                                                    !/*fm         39*/
      a = z + un                                                                !/*fm         40*/
      iz1 = iz - 7                                                              !/*fm         41*/
      iz2 = iz - 34                                                             !/*fm         42*/
      iz3 = iz - 47                                                             !/*fm         43*/
      iz4 = iz - 64                                                             !/*fm         44*/
      nok = .true.                                                              !/*fm         45*/
      if (iz.lt.8) then                                                         !/*fm         46*/
        nok = inn.ge.nmina(iz) .and. inn.le.nmaxa(iz)                           !/*fm         47*/
        ina = inn - nmina(iz) + 1                                               !/*fm         48*/
        nmi = nmina(iz)                                                         !/*fm         49*/
        nma = nmaxa(iz)                                                         !/*fm         50*/
        nmx = nmaxa(iz) - nmina(iz) + 1                                         !/*fm         51*/
      else                                                                      !/*fm         52*/
        nok = inn.ge.nmin(iz1) .and. inn.le.nmax(iz1)                           !/*fm         53*/
        in1 = inn - nmin(iz1) + 1                                               !/*fm         54*/
        nmi = nmin(iz1)                                                         !/*fm         55*/
        nma = nmax(iz1)                                                         !/*fm         56*/
        nmx = nmax(iz1) - nmin(iz1) + 1                                         !/*fm         57*/
      endif                                                                     !/*fm         58*/
      fmolnix = 0.0                                                             !/*fm         59*/
      if (iz.gt.100) then                                                       !/*fm         60*/
        write (io, 1200) iz, inn                                                !/*fm         61*/
        write (itty, 1200) iz, inn                                              !/*fm         62*/
        return                                                                  !/*fm         63*/
      endif                                                                     !/*fm         64*/
      if (nok) then                                                             !/*fm         65*/
        if (iz.lt.8) then                                                       !/*fm         66*/
          if (itype.eq.1) then                                                  !/*fm         67*/
            fmolnix = emica(ina,iz)                                             !/*fm         68*/
          elseif (itype.eq.2) then                                              !/*fm         69*/
            fmolnix = emxa(ina,iz)                                              !/*fm         70*/
          elseif (itype.eq.3) then                                              !/*fm         71*/
            fmolnix = epaira(ina,iz)                                            !/*fm         72*/
          endif                                                                 !/*fm         73*/
        elseif (iz.lt.35 .and. iz.ge.8) then                                    !/*fm         74*/
          if (itype.eq.1) then                                                  !/*fm         75*/
            fmolnix = emic1(in1,iz1)                                            !/*fm         76*/
          elseif (itype.eq.2) then                                              !/*fm         77*/
            fmolnix = emx1(in1,iz1)                                             !/*fm         78*/
          elseif (itype.eq.3) then                                              !/*fm         79*/
            fmolnix = epair1(in1,iz1)                                           !/*fm         80*/
          endif                                                                 !/*fm         81*/
        elseif (iz.lt.48 .and. iz.ge.35) then                                   !/*fm         82*/
          if (itype.eq.1) then                                                  !/*fm         83*/
            fmolnix = emic2(in1,iz2)                                            !/*fm         84*/
          elseif (itype.eq.2) then                                              !/*fm         85*/
            fmolnix = emx2(in1,iz2)                                             !/*fm         86*/
          elseif (itype.eq.3) then                                              !/*fm         87*/
            fmolnix = epair2(in1,iz2)                                           !/*fm         88*/
          endif                                                                 !/*fm         89*/
        elseif (iz.lt.65 .and. iz.ge.48) then                                   !/*fm         90*/
          if (itype.eq.1) then                                                  !/*fm         91*/
            fmolnix = emic3(in1,iz3)                                            !/*fm         92*/
          elseif (itype.eq.2) then                                              !/*fm         93*/
            fmolnix = emx3(in1,iz3)                                             !/*fm         94*/
          elseif (itype.eq.3) then                                              !/*fm         95*/
            fmolnix = epair3(in1,iz3)                                           !/*fm         96*/
          endif                                                                 !/*fm         97*/
        elseif (iz.lt.101 .and. iz.ge.65) then                                  !/*fm         98*/
          if (itype.eq.1) then                                                  !/*fm         99*/
            fmolnix = emic4(in1,iz4)                                            !/*fm        100*/
          elseif (itype.eq.2) then                                              !/*fm        101*/
            fmolnix = emx4(in1,iz4)                                             !/*fm        102*/
          elseif (itype.eq.3) then                                              !/*fm        103*/
            fmolnix = epair4(in1,iz4)                                           !/*fm        104*/
          endif                                                                 !/*fm        105*/
        endif                                                                   !/*fm        106*/
      else                                                                      !/*fm        107*/
!  (nok is false) IN is outside the table for this value of IZ:                 !/*fm        108*/
        if (itype.eq.2) then                                                    !/*fm        109*/
          fmolnix = fmnmacro(iz, inn)                                           !/*fm        110*/
!         call exceed (iz, inn)                                                 !/*fm        111*/
        elseif (itype.eq.1) then                                                !/*fm        112*/
!  For nuclei outside the table, set the microscopic correction to that         !/*fm        113*/
!  for the first or last nuclide (for this value of iz) in the table.           !/*fm        114*/
          if (iz.lt.8) then                                                     !/*fm        115*/
            if (inn.lt.nmina(iz) .and. inn.gt.0)                                !/*fm        116*/
     &                                fmolnix = emica(ina, iz)                  !/*fm        117*/
            if (inn.gt.nmaxa(iz)) fmolnix = emica(nmx, iz)                      !/*fm        118*/
          elseif (iz.ge.8 .and. iz.lt.35) then                                  !/*fm        119*/
            if (inn.lt.nmin(iz1) .and. inn.gt.0) fmolnix = emic1(1, iz1)        !/*fm        120*/
            if (inn.gt.nmax(iz1)) fmolnix = emic1(nmx, iz1)                     !/*fm        121*/
          elseif (iz.ge.35 .and. iz.lt.48) then                                 !/*fm        122*/
            if (inn.lt.nmin(iz1) .and. inn.gt.0) fmolnix = emic2(1, iz2)        !/*fm        123*/
            if (inn.gt.nmax(iz1)) fmolnix = emic2(nmx, iz2)                     !/*fm        124*/
          elseif (iz.ge.48 .and. iz.lt.65) then                                 !/*fm        125*/
            if (inn.lt.nmin(iz1) .and. inn.gt.0) fmolnix = emic3(1, iz3)        !/*fm        126*/
            if (inn.gt.nmax(iz1)) fmolnix = emic3(nmx, iz3)                     !/*fm        127*/
          elseif (iz.ge.65 .and. iz.lt.101) then                                !/*fm        128*/
            if (inn.lt.nmin(iz1) .and. inn.gt.0) fmolnix = emic4(1, iz4)        !/*fm        129*/
            if (inn.gt.nmax(iz1)) fmolnix = emic4(nmx, iz4)                     !/*fm        130*/
          endif                                                                 !/*fm        131*/
        elseif (itype.eq.3) then                                                !/*fm        132*/
          if (iz.lt.8) then                                                     !/*fm        133*/
            eprl1 = epaira(1, iz)                                               !/*fm        134*/
            eprl2 = epaira(2, iz)                                               !/*fm        135*/
            eprh1 = epaira(nmx-1, iz)                                           !/*fm        136*/
            eprh2 = epaira(nmx, iz)                                             !/*fm        137*/
          elseif (iz.lt.35 .and. iz.ge.8) then                                  !/*fm        138*/
            eprl1 = epair1(1, iz1)                                              !/*fm        139*/
            eprl2 = epair1(2, iz1)                                              !/*fm        140*/
            eprh1 = epair1(nmx-1, iz1)                                          !/*fm        141*/
            eprh2 = epair1(nmx, iz1)                                            !/*fm        142*/
          elseif (iz.lt.48 .and. iz.ge.35) then                                 !/*fm        143*/
            eprl1 = epair2(1, iz2)                                              !/*fm        144*/
            eprl2 = epair2(2, iz2)                                              !/*fm        145*/
            eprh1 = epair2(nmx-1, iz2)                                          !/*fm        146*/
            eprh2 = epair2(nmx, iz2)                                            !/*fm        147*/
          elseif (iz.lt.65 .and. iz.ge.48) then                                 !/*fm        148*/
            eprl1 = epair3(1, iz3)                                              !/*fm        149*/
            eprl2 = epair3(2, iz3)                                              !/*fm        150*/
            eprh1 = epair3(nmx-1, iz3)                                          !/*fm        151*/
            eprh2 = epair3(nmx, iz3)                                            !/*fm        152*/
          elseif (iz.lt.101 .and. iz.ge.65) then                                !/*fm        153*/
            eprl1 = epair4(1, iz4)                                              !/*fm        154*/
            eprl2 = epair4(2, iz4)                                              !/*fm        155*/
            eprh1 = epair4(nmx-1, iz4)                                          !/*fm        156*/
            eprh2 = epair4(nmx, iz4)                                            !/*fm        157*/
          endif                                                                 !/*fm        158*/
          if (inn.lt.nmi .and. inn.gt.0) then                                   !/*fm        159*/
            if (inn.eq.nmi-1 .or. inn.eq.nmi-3 .or. inn.eq.nmi-5) then          !/*fm        160*/
              fmolnix = eprl2                                                   !/*fm        161*/
            elseif (inn.eq.nmi-2.or.inn.eq.nmi-4 .or. inn.eq.nmi-6) then        !/*fm        162*/
              fmolnix = eprl1                                                   !/*fm        163*/
            else                                                                !/*fm        164*/
              izo2 = iz/2                                                       !/*fm        165*/
              ino2 = inn/2                                                      !/*fm        166*/
              pair = (dp1 - z + dp2*izo2) + (dp1 - un + dp2*ino2)               !/*fm        167*/
              fmolnix = cevap*pair/sqrt(a)                                      !/*fm        168*/
            endif                                                               !/*fm        169*/
          elseif (inn.gt.nma) then                                              !/*fm        170*/
            if (inn.eq.nma+1 .or. inn.eq.nma+3 .or. inn.eq.nma+5) then          !/*fm        171*/
              fmolnix = eprh1                                                   !/*fm        172*/
            elseif (inn.eq.nma+2.or.inn.eq.nma+4 .or. inn.eq.nma+6) then        !/*fm        173*/
              fmolnix = eprh2                                                   !/*fm        174*/
            else                                                                !/*fm        175*/
              izo2 = iz/2                                                       !/*fm        176*/
              ino2 = inn/2                                                      !/*fm        177*/
              pair = (dp1 - z + dp2*izo2) + (dp1 - un + dp2*ino2)               !/*fm        178*/
              fmolnix = cevap*pair/sqrt(a)                                      !/*fm        179*/
            endif                                                               !/*fm        180*/
          endif                                                                 !/*fm        181*/
        endif                                                                   !/*fm        182*/
      endif                                                                     !/*fm        183*/
! ======================================================================        !/*fm        184*/
 1200 format (1x,'MOLNIX called with Z,N = ',i3,',',i4,'; outside the ',        !/*fm        185*/
     &       'range of the table in MOLLNIX.')                                  !/*fm        186*/
! ======================================================================        !/*fm        187*/
#endif                                                                          /*!fm        188*/
      return                                                                    !/*fm        189*/
      end                                                                       !/*fm        190*/
