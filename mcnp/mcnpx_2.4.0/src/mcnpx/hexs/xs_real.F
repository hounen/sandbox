!     $Id: xs_real.F,v 1.10 2002/03/27 18:14:51 gwm Exp $                      !/*xs          1*/
      subroutine xs_real (ityp0,e0,a0,z0,fmf_,xs_abs,xs_els,xs_tot)             !/*xs          2*/
#include <lcs/double.h>                                                         /*!xs          3*/
      parameter (prm1=51.d0)                                                    !/*xs          4*/
      parameter (e10k=1.0d4)                                                    !/*xs          5*/
      parameter (ieused=5)                                                      !/*xs          6*/
#include <lcs/lptS.h>                                                           /*!xs          7*/
#include <lcs/rucom.h>                                                          /*!xs          8*/
#include <lcs/elstic.h>                                                         /*!xs          9*/
#include <lcs/fltolh.h>                                                         /*!xs         10*/
!.......................................................................        !/*xs         11*/
      dimension bar(lpt)                                                        !/*xs         12*/
      data bar /2*dp1,5*dp0,dp2,2*dp3,dp4,5*dp0,-1.d0,-1.d0,4*dp0/              !/*xs         13*/
      data a_pi /0.1d0/                                                         !/*xs         14*/
      e=e0                                                                      !/*xs         15*/
      a=a0                                                                      !/*xs         16*/
      z=z0                                                                      !/*xs         17*/
      it=ityp0                                                                  !/*xs         18*/
      eused=e                                                                   !/*xs         19*/
   30 continue                                                                  !/*xs         20*/
      ia=a                                                                      !/*xs         21*/
      xs_abs=dp0                                                                !/*xs         22*/
      xs_els=dp0                                                                !/*xs         23*/
! ___ PROTON                                                                    !/*xs         24*/
      if (it.eq.1) then                                                         !/*xs         25*/
        zp=dp1                                                                  !/*xs         26*/
        ap=dp1                                                                  !/*xs         27*/
        if (ia.ge.2) then                                                       !/*xs         28*/
          if (eused.gt.1.0d03) then                                             !/*xs         29*/
            call xsnizl (itypfl(it),a,z,eused,sig,sigela)                       !/*xs         30*/
            sighold=sig                                                         !/*xs         31*/
          endif                                                                 !/*xs         32*/
#ifndef OLDM                                                                    /*!xs         33*/
          if (eused.le.1.0d04) then                                             !/*xs         34*/
#else                                                                           /*!xs         35*/
          if (eused.le.1.0d03) then                                             !/*xs         36*/
#endif                                                                          /*!xs         37*/
            if(ia.eq.2) then                                                    !/*xs         38*/
              sig=sighad(2,ap,zp,a,z,eused)                                     !/*xs         39*/
            elseif (ia.le.11.or.ia.gt.210.or.eused.gt.400.d0.or.                !/*xs         40*/
     &        eused.le.50.d0) then                                              !/*xs         41*/
#ifndef OLDM                                                                    /*!xs         42*/
              sig=max(xabs(zp,ap,max(z,dp2),a,eused),dp0)                       !/*xs         43*/
#else                                                                           /*!xs         44*/
              sig=max(xabs(zp,ap,z,a,eused),dp0)                                !/*xs         45*/
#endif                                                                          /*!xs         46*/
            else                                                                !/*xs         47*/
              sig=1000.d0*fnonel(1,a,eused)                                     !/*xs         48*/
            endif                                                               !/*xs         49*/
          endif                                                                 !/*xs         50*/
#ifndef OLDM                                                                    /*!xs         51*/
          if (eused.gt.1.0d03.and.eused.le.1.0d04) then                         !/*xs         52*/
            sig1=sig                                                            !/*xs         53*/
            f=log10(eused/1.0d03)                                               !/*xs         54*/
            sig=f*(sighold-sig1)+sig1                                           !/*xs         55*/
          endif                                                                 !/*xs         56*/
#endif                                                                          /*!xs         57*/
          xs_abs=fmf_*0.001d0*sig                                               !/*xs         58*/
          if (ielas.ge.2) then                                                  !/*xs         59*/
            if (eused.gt.e10k) then                                             !/*xs         60*/
#ifndef OLDM                                                                    /*!xs         61*/
              call nasapx (a,e10k,sig)                                          !/*xs         62*/
              call xsnizl (itypfl(it),a,z,e10k,sig10k,sigela)                   !/*xs         63*/
              sig=sig*sighold/sig10k                                            !/*xs         64*/
#else                                                                           /*!xs         65*/
              call nasapx (a,eused,sig)                                         !/*xs         66*/
#endif                                                                          /*!xs         67*/
            else                                                                !/*xs         68*/
              call nasapx (a,eused,sig)                                         !/*xs         69*/
            endif                                                               !/*xs         70*/
            xs_els=fmf_*sig                                                     !/*xs         71*/
          endif                                                                 !/*xs         72*/
          go to 20                                                              !/*xs         73*/
! ___ H1 SECTION                                                                !/*xs         74*/
        else                                                                    !/*xs         75*/
          call sgm2 (1,eused,ii,ee,hsig)                                        !/*xs         76*/
          if(eused.le.20.d0) then                                               !/*xs         77*/
            xs_els=fmf_*1.d+24*hsig                                             !/*xs         78*/
            xs_abs=dp0                                                          !/*xs         79*/
          else                                                                  !/*xs         80*/
            xs_els=fmf_*1.d+24*xsecl(3,1,min(eused,3500.d0))                    !/*xs         81*/
            xs_abs=fmf_*1.d+24*hsig-xs_els                                      !/*xs         82*/
          endif                                                                 !/*xs         83*/
          go to 20                                                              !/*xs         84*/
        endif                                                                   !/*xs         85*/
! ___ NEUTRON                                                                   !/*xs         86*/
      elseif (it.eq.2) then                                                     !/*xs         87*/
        zp=dp0                                                                  !/*xs         88*/
        ap=dp1                                                                  !/*xs         89*/
        if (ia.ge.2) then                                                       !/*xs         90*/
          if (eused.gt.1.0d03) then                                             !/*xs         91*/
            call xsnizl (itypfl(it),a,z,eused,sig,sigela)                       !/*xs         92*/
            sigine=0.001d0*sig                                                  !/*xs         93*/
            sighold=sigine                                                      !/*xs         94*/
          endif                                                                 !/*xs         95*/
#ifndef OLDM                                                                    /*!xs         96*/
          if (eused.le.1.0d04) then                                             !/*xs         97*/
#else                                                                           /*!xs         98*/
          if (eused.le.1.0d03) then                                             !/*xs         99*/
#endif                                                                          /*!xs        100*/
            if (eused.lt.20.d0) then                                            !/*xs        101*/
              sigine=0.d0                                                       !/*xs        102*/
            elseif (ia.eq.2) then                                               !/*xs        103*/
              if (eused.le.100.d0) then                                         !/*xs        104*/
                call evalna (a,eused,sigine)                                    !/*xs        105*/
              elseif (eused.gt.300.d0) then                                     !/*xs        106*/
                sigine=0.001d0*sighad(2,ap,zp,a,z,eused)                        !/*xs        107*/
              else                                                              !/*xs        108*/
                call evalna (a,100.d0,sig1)                                     !/*xs        109*/
                sig2=0.001d0*sighad(2,ap,zp,a,z,300.d0)                         !/*xs        110*/
                f=(eused-100.d0)/200.d0                                         !/*xs        111*/
                sigine=f*(sig2-sig1)+sig1                                       !/*xs        112*/
              endif                                                             !/*xs        113*/
            elseif (ia.le.7) then                                               !/*xs        114*/
              sigine=0.001d0*sighad(2,ap,zp,a,z,eused)                          !/*xs        115*/
            elseif (ia.le.11) then                                              !/*xs        116*/
              if (eused.le.100.d0) then                                         !/*xs        117*/
                call evalna (a,eused,sigine)                                    !/*xs        118*/
              else                                                              !/*xs        119*/
                sigine=0.001d0*xabs(zp,ap,z,a,eused)                            !/*xs        120*/
              endif                                                             !/*xs        121*/
            elseif (ia.gt.210) then                                             !/*xs        122*/
              if (eused.le.100.d0) then                                         !/*xs        123*/
                call evalna (a,eused,sigine)                                    !/*xs        124*/
              else                                                              !/*xs        125*/
#ifndef OLDM                                                                    /*!xs        126*/
                sigine=0.001d0*sighad(2,ap,zp,a,z,eused)                        !/*xs        127*/
#else                                                                           /*!xs        128*/
                sigine=0.001d0*xabs(zp,ap,z,a,eused)                            !/*xs        129*/
#endif                                                                          /*!xs        130*/
              endif                                                             !/*xs        131*/
            else                                                                !/*xs        132*/
              if (eused.le.150.d0) then                                         !/*xs        133*/
                call evalna (a,eused,sigine)                                    !/*xs        134*/
              elseif (eused.le.400.d0) then                                     !/*xs        135*/
                sigine=fnonel(2,a,eused)                                        !/*xs        136*/
              else                                                              !/*xs        137*/
                sigine=0.001d0*xabs(zp,ap,z,a,eused)                            !/*xs        138*/
              endif                                                             !/*xs        139*/
            endif                                                               !/*xs        140*/
          endif                                                                 !/*xs        141*/
#ifndef OLDM                                                                    /*!xs        142*/
          if (eused.gt.1.0d03.and.eused.le.1.0d04) then                         !/*xs        143*/
            f=log10(eused/1.0d03)                                               !/*xs        144*/
            sigine=f*(sighold-sigine)+sigine                                    !/*xs        145*/
          endif                                                                 !/*xs        146*/
#endif                                                                          /*!xs        147*/
          xs_abs=fmf_*sigine                                                    !/*xs        148*/
          if (ielas.ge.1) then                                                  !/*xs        149*/
            if (eused.lt.20.d0) then                                            !/*xs        150*/
              sig=0.d0                                                          !/*xs        151*/
            elseif (eused.lt.50.d0) then                                        !/*xs        152*/
              call evalne (a,eused,sig)                                         !/*xs        153*/
            elseif (eused.le.100.d0) then                                       !/*xs        154*/
              call evalne (a,eused,sig1)                                        !/*xs        155*/
              call nasanx (a,eused,sig2)                                        !/*xs        156*/
              f=(eused-50.d0)/50.d0                                             !/*xs        157*/
              sig=f*(sig2-sig1)+sig1                                            !/*xs        158*/
            elseif (eused.le.e10k) then                                         !/*xs        159*/
              call nasanx (a,eused,sig)                                         !/*xs        160*/
            else                                                                !/*xs        161*/
#ifndef OLDM                                                                    /*!xs        162*/
              call nasanx (a,e10k,sig)                                          !/*xs        163*/
              call xsnizl (itypfl(it),a,z,e10k,sig10k,sigela)                   !/*xs        164*/
              sig10k=0.001d0*sig10k                                             !/*xs        165*/
              sig=sig*sighold/sig10k                                            !/*xs        166*/
#else                                                                           /*!xs        167*/
              call nasanx (a,eused,sig)                                         !/*xs        168*/
#endif                                                                          /*!xs        169*/
            endif                                                               !/*xs        170*/
            xs_els=fmf_*sig                                                     !/*xs        171*/
          endif                                                                 !/*xs        172*/
          go to 20                                                              !/*xs        173*/
! ___ H1 SECTION                                                                !/*xs        174*/
        elseif (ia.eq.1) then                                                   !/*xs        175*/
          call sgm2 (2,eused,ii,ee,hsig)                                        !/*xs        176*/
          if(eused.le.20.d0) then                                               !/*xs        177*/
            xs_els=fmf_*1.d+24*hsig                                             !/*xs        178*/
            xs_abs=dp0                                                          !/*xs        179*/
          else                                                                  !/*xs        180*/
            xs_els=fmf_*1.d+24*xsecl(3,2,min(eused,3500.d0))                    !/*xs        181*/
            xs_abs=fmf_*1.d+24*hsig-xs_els                                      !/*xs        182*/
          endif                                                                 !/*xs        183*/
          go to 20                                                              !/*xs        184*/
!                                                                               !/*xs        185*/
        endif                                                                   !/*xs        186*/
! ___ PIONS                                                                     !/*xs        187*/
      elseif (it.ge.3.and.it.le.5) then                                         !/*xs        188*/
        if (ia.gt.1) then                                                       !/*xs        189*/
#ifndef OLDM                                                                    /*!xs        190*/
          if (eused.gt.1.0d03) then                                             !/*xs        191*/
            call xsnizl (itypfl(it),a,z,eused,sighold,sigela)                   !/*xs        192*/
          endif                                                                 !/*xs        193*/
#endif                                                                          /*!xs        194*/
          z1=charge(it)                                                         !/*xs        195*/
#ifndef OLDM                                                                    /*!xs        196*/
          etemp=min(eused,3.0d03)                                               !/*xs        197*/
#else                                                                           /*!xs        198*/
          etemp=eused                                                           !/*xs        199*/
#endif                                                                          /*!xs        200*/
          sigine=sighad(2,a_pi,z1,a,z,etemp)                                    !/*xs        201*/
          sigela=max(sighad(1,a_pi,z1,a,z,etemp)-sigine,dp0)                    !/*xs        202*/
#ifndef OLDM                                                                    /*!xs        203*/
! +++     QUESTION: This TEMPORARY expedient avoids a system-dependent          !/*xs        204*/
! +++               floating-point error when sigine == 0.                      !/*xs        205*/
! +++               However, we should re-examine the logic later.              !/*xs        206*/
          if (sigine.gt.dp0) then                                               !/*xs        207*/
            r_scat=sigela/sigine                                                !/*xs        208*/
          else                                                                  !/*xs        209*/
            r_scat=dp0                                                          !/*xs        210*/
          endif                                                                 !/*xs        211*/
! +++                                                                           !/*xs        212*/
          if (eused.gt.1.0d04) then                                             !/*xs        213*/
            sigine=sighold                                                      !/*xs        214*/
          elseif (eused.gt.1.0d03) then                                         !/*xs        215*/
            f=log10(eused/1.0d03)                                               !/*xs        216*/
            sigine=f*(sighold-sigine)+sigine                                    !/*xs        217*/
          endif                                                                 !/*xs        218*/
          if (eused.gt.1.0d03) sigela=r_scat*sigine                             !/*xs        219*/
#endif                                                                          /*!xs        220*/
          go to 10                                                              !/*xs        221*/
! ___ H1 SECTION                                                                !/*xs        222*/
        elseif (ia.eq.1) then                                                   !/*xs        223*/
          itt=it                                                                !/*xs        224*/
          if (itt.gt.3) itt=itt-1                                               !/*xs        225*/
          call sgm2 (itt,eused,ii,ee,hsig)                                      !/*xs        226*/
          if(eused.le.20.d0) then                                               !/*xs        227*/
            xs_els=fmf_*1.d+24*hsig                                             !/*xs        228*/
            xs_abs=dp0                                                          !/*xs        229*/
          else                                                                  !/*xs        230*/
            xs_els=fmf_*1.d+24*xsecl(3,itt,min(eused,2500.d0))                  !/*xs        231*/
            xs_abs=fmf_*1.d+24*hsig-xs_els                                      !/*xs        232*/
          endif                                                                 !/*xs        233*/
          go to 20                                                              !/*xs        234*/
!                                                                               !/*xs        235*/
        endif                                                                   !/*xs        236*/
! ___ IONS                                                                      !/*xs        237*/
      elseif (it.ge.8.and.it.le.11) then                                        !/*xs        238*/
        if (ia.eq.1) then                                                       !/*xs        239*/
          eused=eused*pmass(1)/pmass(it)                                        !/*xs        240*/
          a=bar(it)                                                             !/*xs        241*/
          z=charge(it)                                                          !/*xs        242*/
          it=1                                                                  !/*xs        243*/
          go to 30                                                              !/*xs        244*/
        else                                                                    !/*xs        245*/
          z1=charge(it)                                                         !/*xs        246*/
          a1=bar(it)                                                            !/*xs        247*/
          sigine=xabs(z1,a1,z,a,eused)                                          !/*xs        248*/
          sigela=dp0                                                            !/*xs        249*/
          go to 10                                                              !/*xs        250*/
        endif                                                                   !/*xs        251*/
! ___ ANTINUCLEONS                                                              !/*xs        252*/
      elseif (it.eq.17.or.it.eq.18) then                                        !/*xs        253*/
        eused=max(e,prm1)                                                       !/*xs        254*/
      endif                                                                     !/*xs        255*/
      call xsnizl (itypfl(it),a,z,eused,sigine,sigela)                          !/*xs        256*/
   10 continue                                                                  !/*xs        257*/
      xs_abs=fmf_*0.001d0*sigine                                                !/*xs        258*/
      xs_els=fmf_*0.001d0*sigela                                                !/*xs        259*/
   20 continue                                                                  !/*xs        260*/
      if (ielas.eq.0.or.it.eq.1.and.ielas.le.1.or.it.gt.2.and.                  !/*xs        261*/
     &  ielas.le.2) then                                                        !/*xs        262*/
        if (a0.gt.dp1) xs_els=dp0                                               !/*xs        263*/
      endif                                                                     !/*xs        264*/
      xs_tot=xs_abs+xs_els                                                      !/*xs        265*/
      return                                                                    !/*xs        266*/
      end                                                                       !/*xs        267*/
