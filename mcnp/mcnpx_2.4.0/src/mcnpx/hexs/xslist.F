!     $Id: xslist.F,v 1.7 2002/03/27 18:14:51 gwm Exp $                       !/*xs          1*/
      subroutine xslist (lun)                                                   !/*xs          2*/
#include <lcs/double.h>                                                         /*!xs          3*/
#include <lcs/lptS.h>                                                           /*!xs          4*/
#include <lcs/rucom.h>                                                          /*!xs          5*/
#include <lcs/fltolh.h>                                                         /*!xs          6*/
!.......................................................................        !/*xs          7*/
      parameter (neng=70, nel=51, ntype=13, long=5)                             !/*xs          8*/
      dimension a(nel), z(nel)                                                  !/*xs          9*/
      dimension e(neng), xnon(neng,long), xels(neng,long)                       !/*xs         10*/
      dimension xnmax(long), xemax(long)                                        !/*xs         11*/
      dimension itype(ntype)                                                    !/*xs         12*/
      data itype /1,2,17,18,3,4,5,13,16,8,9,10,11/                              !/*xs         13*/
      data a /1.,2.,3.,3.,4.,6.,7.,9.,10.,11.,12.,14.,16.,19.,23.,24.,26        !/*xs         14*/
     & .,27.,28.,36.,40.,40.,48.,48.,51.,56.,58.,63.,65.,65.,72.,81.,90.        !/*xs         15*/
     & ,93.,96.,107.,112.,120.,127.,138.,140.,159.,181.,182.,184.,186.,1        !/*xs         16*/
     & 97.,206.,208.,238.,250./                                                 !/*xs         17*/
      data z /1.,1.,1.,2.,2.,3.,3.,4.,5.,5.,6.,7.,8.,9.,11.,12.,12.,13.,        !/*xs         18*/
     & 14.,16.,18.,20.,20.,22.,23.,26.,28.,29.,29.,30.,32.,35.,40.,41.,4        !/*xs         19*/
     & 2.,47.,48.,50.,53.,56.,58.,65.,73.,74.,74.,74.,79.,82.,82.,92.,98        !/*xs         20*/
     & ./                                                                       !/*xs         21*/
      data e /0.5d0,1.d0,1.5d0,2.d0,2.5d0,3.d0,3.5d0,4.d0,4.5d0,5.d0,5.5        !/*xs         22*/
     & d0,6.d0,6.5d0,7.d0,7.5d0,8.d0,9.d0,10.d0,11.d0,12.d0,13.d0,14.d0,        !/*xs         23*/
     & 15.d0,17.5d0,20.d0,22.5d0,25.d0,30.d0,35.d0,40.d0,45.d0,50.d0,51         !/*xs         24*/
     & .d0,55.d0,60.d0,70.d0,80.d0,90.d0,100.d0,110.d0,120.d0,140.d0,160        !/*xs         25*/
     & .d0,180.d0,200.d0,225.d0,250.d0,275.d0,300.d0,325.d0,350.d0,375          !/*xs         26*/
     & .d0,400.d0,500.d0,700.d0,1000.d0,1500.d0,3000.d0,4000.d0,5000.d0,        !/*xs         27*/
     & 7000.d0,10000.d0,15000.d0,40000.d0,100000.d0,150000.d0,400000.d0,        !/*xs         28*/
     & 1000000.d0,3000000.d0,10000000.d0/                                       !/*xs         29*/
      data fmf_ /1.d0/                                                          !/*xs         30*/
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        !/*xs         31*/
      do 50 i=1,nel                                                             !/*xs         32*/
!                                                                               !/*xs         33*/
      write (lun,160) a(i),z(i)                                                 !/*xs         34*/
      do 10 k=1,4                                                               !/*xs         35*/
      xnmax(k)=0.d0                                                             !/*xs         36*/
      xemax(k)=0.d0                                                             !/*xs         37*/
   10 continue                                                                  !/*xs         38*/
      do 40 j=1,neng                                                            !/*xs         39*/
      do 20 k=1,4                                                               !/*xs         40*/
      it=itype(k)                                                               !/*xs         41*/
      call xs_real (it,e(j),a(i),z(i),fmf_,xs_abs,xs_els,xs_tot)                !/*xs         42*/
      xnon(j,k)=1000.d0*xs_abs                                                  !/*xs         43*/
      xels(j,k)=1000.d0*xs_els                                                  !/*xs         44*/
   20 continue                                                                  !/*xs         45*/
!                                                                               !/*xs         46*/
      write (lun,190) e(j),(xnon(j,k),k=1,4),(xels(j,k),k=1,4)                  !/*xs         47*/
      do 30 k=1,4                                                               !/*xs         48*/
      xnmax(k)=max(xnmax(k),xnon(j,k))                                          !/*xs         49*/
      xemax(k)=max(xemax(k),xels(j,k))                                          !/*xs         50*/
   30 continue                                                                  !/*xs         51*/
   40 continue                                                                  !/*xs         52*/
      write (lun,200) (xnmax(k),k=1,4),(xemax(k),k=1,4)                         !/*xs         53*/
      write (lun,210)                                                           !/*xs         54*/
   50 continue                                                                  !/*xs         55*/
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        !/*xs         56*/
      do 100 i=1,nel                                                            !/*xs         57*/
!                                                                               !/*xs         58*/
      write (lun,170) a(i),z(i)                                                 !/*xs         59*/
      do 60 k=1,5                                                               !/*xs         60*/
      xnmax(k)=0.d0                                                             !/*xs         61*/
      xemax(k)=0.d0                                                             !/*xs         62*/
   60 continue                                                                  !/*xs         63*/
      do 90 j=1,neng                                                            !/*xs         64*/
      do 70 k=1,5                                                               !/*xs         65*/
      it=itype(k+4)                                                             !/*xs         66*/
      call xs_real (it,e(j),a(i),z(i),fmf_,xs_abs,xs_els,xs_tot)                !/*xs         67*/
      xnon(j,k)=1000.d0*xs_abs                                                  !/*xs         68*/
      xels(j,k)=1000.d0*xs_els                                                  !/*xs         69*/
   70 continue                                                                  !/*xs         70*/
!                                                                               !/*xs         71*/
      write (lun,190) e(j),(xnon(j,k),k=1,3),(xels(j,k),k=1,3),(xnon(j,k        !/*xs         72*/
     & ),k=4,5),(xels(j,k),k=4,5)                                               !/*xs         73*/
      do 80 k=1,5                                                               !/*xs         74*/
      xnmax(k)=max(xnmax(k),xnon(j,k))                                          !/*xs         75*/
      xemax(k)=max(xemax(k),xels(j,k))                                          !/*xs         76*/
   80 continue                                                                  !/*xs         77*/
   90 continue                                                                  !/*xs         78*/
      write (lun,200) (xnmax(k),k=1,3),(xemax(k),k=1,3),(xnmax(k),k=4,5)        !/*xs         79*/
     & ,(xemax(k),k=4,5)                                                        !/*xs         80*/
      write (lun,210)                                                           !/*xs         81*/
  100 continue                                                                  !/*xs         82*/
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        !/*xs         83*/
      do 150 i=1,nel                                                            !/*xs         84*/
!                                                                               !/*xs         85*/
      write (lun,180) a(i),z(i)                                                 !/*xs         86*/
      do 110 k=1,4                                                              !/*xs         87*/
      xnmax(k)=0.d0                                                             !/*xs         88*/
      xemax(k)=0.d0                                                             !/*xs         89*/
  110 continue                                                                  !/*xs         90*/
      do 140 j=1,neng                                                           !/*xs         91*/
      do 120 k=1,4                                                              !/*xs         92*/
      it=itype(k+9)                                                             !/*xs         93*/
      call xs_real (it,e(j),a(i),z(i),fmf_,xs_abs,xs_els,xs_tot)                !/*xs         94*/
      xnon(j,k)=1000.d0*xs_abs                                                  !/*xs         95*/
      xels(j,k)=1000.d0*xs_els                                                  !/*xs         96*/
  120 continue                                                                  !/*xs         97*/
!                                                                               !/*xs         98*/
      write (lun,190) e(j),(xnon(j,k),k=1,4),(xels(j,k),k=1,4)                  !/*xs         99*/
      do 130 k=1,4                                                              !/*xs        100*/
      xnmax(k)=max(xnmax(k),xnon(j,k))                                          !/*xs        101*/
      xemax(k)=max(xemax(k),xels(j,k))                                          !/*xs        102*/
  130 continue                                                                  !/*xs        103*/
  140 continue                                                                  !/*xs        104*/
      write (lun,200) (xnmax(k),k=1,4),(xemax(k),k=1,4)                         !/*xs        105*/
      write (lun,210)                                                           !/*xs        106*/
  150 continue                                                                  !/*xs        107*/
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        !/*xs        108*/
      return                                                                    !/*xs        109*/
!                                                                               !/*xs        110*/
  160 format (15x,' Cross Sections (mb) for A = ',f5.0,' Z = ',f5.0//           !/*xs        111*/
     & 8x,'MeV   Prot_n   Neut_n   Pbar_n   Nbar_n   Prot_e   Neut_e',          !/*xs        112*/
     & '   Pbar_e   Nbar_e')                                                    !/*xs        113*/
  170 format (15x,' Cross Sections (mb) for A = ',f5.0,' Z = ',f5.0//           !/*xs        114*/
     & 8x,'MeV    Pi+_n    Pi0_n    Pi-_n    Pi+_e    Pi0_e    Pi-_e',          !/*xs        115*/
     & '     K+_n     K-_n     K+_e     K-_e')                                  !/*xs        116*/
  180 format (15x,' Cross Sections (mb) for A = ',f5.0,' Z = ',f5.0//           !/*xs        117*/
     & 8x,'MeV   Deut_n   Trit_n   He-3_n   Alph_n   Deut_e   Trit_e',          !/*xs        118*/
     & '   He-3_e   Alph_e')                                                    !/*xs        119*/
  190 format (1x,f10.1,10f9.0)                                                  !/*xs        120*/
  200 format ('   maximum ',10f9.0)                                             !/*xs        121*/
  210 format (1h1)                                                              !/*xs        122*/
      end                                                                       !/*xs        123*/
