!     $Id: amfp.F,v 1.4 2002/03/27 18:14:54 gwm Exp $                         !/*am          1*/
      subroutine amfp (en,dens)                                                 !/*am          2*/
#include <lcs/double.h>                                                         /*!am          3*/
!     ==================================================================        !/*am          4*/
!     amfp calculates the mean free path of the particle.                       !/*am          5*/
!                                                                               !/*am          6*/
! AMFP   calls   CII  CIJ  SQRT  PTOTKL  PTOTAL  CALCU  AITOTA                  !/*am          7*/
!                ALOG  EXP                                                      !/*am          8*/
! called by ISABEL  DELTA                                                       !/*am          9*/
!     ==================================================================        !/*am         10*/
!                                                                               !/*am         11*/
#include <lcs/vegas.h>                                                          /*!am         12*/
#include <lcs/inout.h>                                                          /*!am         13*/
!       ap-scattering and total cross-sections (from old sub "aptota")          !/*am         14*/
      aptat(e)=exp(4.5485063d0*exp(-.06010215d0*log(max(1.d-3,1.d-3*e)))        !/*am         15*/
     & )                                                                        !/*am         16*/
!                                                                               !/*am         17*/
      amfpa=timef*dens                                                          !/*am         18*/
! e is the kinetic energy of the particle in mev.                               !/*am         19*/
      if (csa(1).eq.dp0.and.proj(13).lt.dp0) go to 60                           !/*am         20*/
   10 e=(en-ws(4))                                                              !/*am         21*/
      if (ws(3).le.-dp1) go to 200                                              !/*am         22*/
      if (ws(3)-dp10) 20,160,160                                                !/*am         23*/
   20 if (ws(3)-2.d0) 30,30,90                                                  !/*am         24*/
! statement 3 calculates the cross section for nucleon interaction.             !/*am         25*/
   30 continue                                                                  !/*am         26*/
      if (ws(3).eq.dp1) t=(cii(e)*csa(4)+cij(e)*csa(7))/csa(3)                  !/*am         27*/
      if (ws(3).eq.dp2) t=(cii(e)*csa(7)+cij(e)*csa(4))/csa(3)                  !/*am         28*/
! cross section are needed to calculate the characteristic time of a            !/*am         29*/
!  particle.                                                                    !/*am         30*/
   40 amfpb=dp1/(t*amfpa)                                                       !/*am         31*/
!      chosen to force at least 4 samples in the thinnest region                !/*am         32*/
      amfpb=min(dp4m3,amfpb)                                                    !/*am         33*/
      ws(13)=amfpb*en/sqrt(en*en-ws(4)*ws(4))                                   !/*am         34*/
      if (ws(13).gt.dp0) go to 50                                               !/*am         35*/
      write (io,210)                                                            !/*am         36*/
      kcan=1                                                                    !/*am         37*/
      lkcan='amfp'                                                              !/*am         38*/
!                                                                               !/*am         39*/
   50 if (csa(14).gt.dp0) return                                                !/*am         40*/
      if (csa(1).eq.dp0) go to 70                                               !/*am         41*/
      csa(14)=ws(13)                                                            !/*am         42*/
      return                                                                    !/*am         43*/
!                                                                               !/*am         44*/
   60 ws(3)=dp1                                                                 !/*am         45*/
!-----------------------------------------------------------------------        !/*am         46*/
!     to prevent SQRT from blowing up at low energies                           !/*am         47*/
      ws(4)=dph*(amass(1)+amass(2))                                             !/*am         48*/
      go to 10                                                                  !/*am         49*/
   70 if (ws(3).eq.dp2) go to 80                                                !/*am         50*/
      ws(3)=dp2                                                                 !/*am         51*/
!-----------------------------------------------------------------------        !/*am         52*/
      proj(13)=ws(13)                                                           !/*am         53*/
      go to 10                                                                  !/*am         54*/
   80 proj(14)=ws(13)                                                           !/*am         55*/
      return                                                                    !/*am         56*/
!                                                                               !/*am         57*/
   90 continue                                                                  !/*am         58*/
      if (ws(3)-6.d0) 110,110,100                                               !/*am         59*/
  100 continue                                                                  !/*am         60*/
      plab=sqrt(en*en-ws(4)**2)                                                 !/*am         61*/
      e=e                                                                       !/*am         62*/
      typ=ws(3)-8.d0                                                            !/*am         63*/
      call ptotkl (plab,typ,scp,scn,abp,abn)                                    !/*am         64*/
!        csa(8) = z/a                                                           !/*am         65*/
      amfpc2=dp1-csa(8)                                                         !/*am         66*/
      t=scn*amfpc2+scp*csa(8)                                                   !/*am         67*/
      go to 40                                                                  !/*am         68*/
! statement 89 to not including 130 calculates the cross section                !/*am         69*/
!  for pion interaction.                                                        !/*am         70*/
! the following change was made 10/16/70 because ptotal is tabulated in         !/*am         71*/
! terms of the k.e.in the c.m.not the k.e.in the laboratory                     !/*am         72*/
! to calculate the k.e.in the c.m.the average of a proton and a neutron         !/*am         73*/
! mass was used for the nucleon mass.                                           !/*am         74*/
  110 continue                                                                  !/*am         75*/
      ampn=dph*(amass(1)+amass(2))                                              !/*am         76*/
      e=sqrt(ws(4)*ws(4)+ampn*(ampn+dp2*en))-ws(4)-ampn                         !/*am         77*/
!-----------------------------------------------------------------------        !/*am         78*/
!      to prevent to many jumps in SELPT for low energy pions                   !/*am         79*/
      e=max(e,dp30)                                                             !/*am         80*/
!-----------------------------------------------------------------------        !/*am         81*/
      if (ws(3)-dp4) 120,150,120                                                !/*am         82*/
  120 ind(9)=1                                                                  !/*am         83*/
      call ptotal (e,ind(9),ind(10),ind(11),scat,absor)                         !/*am         84*/
      ind(9)=0                                                                  !/*am         85*/
      ind(10)=1                                                                 !/*am         86*/
      call ptotal (e,ind(9),ind(10),ind(11),scatt,absor)                        !/*am         87*/
      ind(10)=0                                                                 !/*am         88*/
      amfpc2=dp1-csa(8)                                                         !/*am         89*/
      if (ws(3)-5.d0) 130,140,130                                               !/*am         90*/
  130 t=scatt*amfpc2+scat*csa(8)                                                !/*am         91*/
      go to 40                                                                  !/*am         92*/
  140 t=scat*amfpc2+scatt*csa(8)                                                !/*am         93*/
      go to 40                                                                  !/*am         94*/
  150 ind(11)=1                                                                 !/*am         95*/
      call ptotal (e,ind(9),ind(10),ind(11),t,absor)                            !/*am         96*/
      ind(11)=0                                                                 !/*am         97*/
      go to 40                                                                  !/*am         98*/
! statement 4 to , not including , 25 calculates the cross section for          !/*am         99*/
!  isobar interaction.                                                          !/*am        100*/
  160 if (ws(3)-13.d0) 170,180,170                                              !/*am        101*/
  170 if (ws(3)-25.d0) 190,180,190                                              !/*am        102*/
  180 ind(12)=1                                                                 !/*am        103*/
  190 aiso=sqrt(en*en-ws(4)*ws(4))/en                                           !/*am        104*/
      aiso1=dens                                                                !/*am        105*/
      wsp(3)=dp1                                                                !/*am        106*/
      call calcu (amass(1),en,ws(4),u,v)                                        !/*am        107*/
      call aitota (e,aiso,aiso1,u,v,wsp)                                        !/*am        108*/
      amfpi1=csa(8)*aitot                                                       !/*am        109*/
      wsp(3)=dp2                                                                !/*am        110*/
      call calcu (amass(2),en,ws(4),u,v)                                        !/*am        111*/
      call aitota (e,aiso,aiso1,u,v,wsp)                                        !/*am        112*/
      t=(dp1-csa(8))*aitot+amfpi1                                               !/*am        113*/
      ind(12)=0                                                                 !/*am        114*/
      go to 40                                                                  !/*am        115*/
!  statement 25 calculates the cross section for ap interaction                 !/*am        116*/
  200 t=aptat(e)                                                                !/*am        117*/
      go to 40                                                                  !/*am        118*/
!                                                                               !/*am        119*/
  210 format (' ws(13) negative in amfp. cancelling cascade')                   !/*am        120*/
      end                                                                       !/*am        121*/
