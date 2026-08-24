!     $Id: refl.F,v 1.5 2002/03/27 18:15:20 gwm Exp $                         !/*re          1*/
      subroutine refl (ipisw,delen,coul,iref)                                   !/*re          2*/
#include <lcs/double.h>                                                         /*!re          3*/
!     ==================================================================        !/*re          4*/
!     refl reflects or refracts particles at region boundary                    !/*re          5*/
!     in proper models. in a no reflection refraction model                     !/*re          6*/
!     pions which cannot cross the boundary (ipisw=1) are                       !/*re          7*/
!     reflected                                                                 !/*re          8*/
!                                                                               !/*re          9*/
! REFL   calls   SQRT  PLT  LOOK                                                !/*re         10*/
! called by RLAR  POE  PSEL                                                     !/*re         11*/
!     ==================================================================        !/*re         12*/
!                                                                               !/*re         13*/
#include <lcs/inout.h>                                                          /*!re         14*/
#include <lcs/vegas.h>                                                          /*!re         15*/
      dimension p(4), pt(4), xx(3)                                              !/*re         16*/
!                                                                               !/*re         17*/
      icheck=0                                                                  !/*re         18*/
      dist=1.d-4                                                                !/*re         19*/
      dele=delen                                                                !/*re         20*/
      iref=0                                                                    !/*re         21*/
      itarg=0                                                                   !/*re         22*/
      if (ind(8).gt.0.and.ws(31).ne.dp0) go to 20                               !/*re         23*/
      if (ind(8).eq.0.and.istept.eq.istpt) go to 20                             !/*re         24*/
      itarg=1                                                                   !/*re         25*/
      do 10 i=1,3                                                               !/*re         26*/
      pt(i+1)=ws(i+5)                                                           !/*re         27*/
   10 xx(i)=ws(i+9)                                                             !/*re         28*/
      pt(1)=sqrt(ws(5)*ws(5)-ws(4)*ws(4))                                       !/*re         29*/
      et=ws(5)                                                                  !/*re         30*/
      r=ws(14)                                                                  !/*re         31*/
      istepo=istept                                                             !/*re         32*/
      go to 40                                                                  !/*re         33*/
   20 do 30 i=1,3                                                               !/*re         34*/
      p(i+1)=ws(i+5)                                                            !/*re         35*/
   30 xx(i)=ws(i+9)-proj(i+4)                                                   !/*re         36*/
      p(1)=sqrt(ws(5)*ws(5)-ws(4)*ws(4))                                        !/*re         37*/
      xx(3)=xx(3)/gama                                                          !/*re         38*/
      r=sqrt(xx(1)*xx(1)+xx(2)*xx(2)+xx(3)*xx(3))                               !/*re         39*/
      bet=-voc                                                                  !/*re         40*/
      call plt (p,ws(5),pt,et,bet,gama)                                         !/*re         41*/
      istepo=istepp                                                             !/*re         42*/
   40 pr=pt(2)*xx(1)+pt(3)*xx(2)+pt(4)*xx(3)                                    !/*re         43*/
      prold=pt(1)*pr/r                                                          !/*re         44*/
      prnew=dele*dele+dp2*dele*et+prold*prold                                   !/*re         45*/
      if (ipisw.ne.0) go to 50                                                  !/*re         46*/
      if (prnew.lt.dp0) go to 50                                                !/*re         47*/
      if (ind(4).eq.0) go to 110                                                !/*re         48*/
      if (ws(3).eq.dp1) dele=dele+coul                                          !/*re         49*/
      if (ws(3).eq.3.d0) dele=dele+coul                                         !/*re         50*/
      if (ws(3).eq.7.d0) dele=dele+coul                                         !/*re         51*/
      if (ws(3).eq.13.d0) dele=dele+dp3*coul                                    !/*re         52*/
      if (ws(3).eq.14.d0) dele=dele+coul                                        !/*re         53*/
      prnew=dele*dele+dp2*dele*et+prold*prold                                   !/*re         54*/
      go to 110                                                                 !/*re         55*/
!  calculate reflection                                                         !/*re         56*/
   50 iref=1                                                                    !/*re         57*/
      pro=dp2*pr/r/r                                                            !/*re         58*/
      do 60 i=1,3                                                               !/*re         59*/
   60 pt(i+1)=pt(i+1)-xx(i)*pro                                                 !/*re         60*/
!-----in reflection correct coordinates to make sure                            !/*re         61*/
!-----that the particle stays in old region.                                    !/*re         62*/
      arg=dp0                                                                   !/*re         63*/
   70 do 80 i=1,3                                                               !/*re         64*/
      xx(i)=xx(i)+dist*pt(i+1)                                                  !/*re         65*/
   80 arg=arg+xx(i)*xx(i)                                                       !/*re         66*/
      arg=sqrt(arg)                                                             !/*re         67*/
      if (ws(31).eq.dp0) call look (arg,xa,istep)                               !/*re         68*/
      if (ws(31).ne.dp0) call look (arg,ya,istep)                               !/*re         69*/
      idstp=abs(istep-istepo)-1                                                 !/*re         70*/
      if (idstp) 130,100,90                                                     !/*re         71*/
   90 if (mprin.eq.0) mprin=-1                                                  !/*re         72*/
      write (io,220) istepo,istep,ws(31)                                        !/*re         73*/
      go to 190                                                                 !/*re         74*/
  100 dist=dist+1.d-5                                                           !/*re         75*/
      icheck=icheck+1                                                           !/*re         76*/
      if (icheck.lt.10) go to 70                                                !/*re         77*/
      write (io,210) istep,istepp,istpp,istept,istpt,ws,gws,proj                !/*re         78*/
      go to 190                                                                 !/*re         79*/
!  calculate refraction                                                         !/*re         80*/
  110 prnew=sqrt(prnew)                                                         !/*re         81*/
      prnew=sign(prnew,prold)                                                   !/*re         82*/
      delpr=prnew-prold                                                         !/*re         83*/
      et=et+dele                                                                !/*re         84*/
      pnew=sqrt(et*et-ws(4)*ws(4))                                              !/*re         85*/
      ptof1=pt(1)                                                               !/*re         86*/
      delp=delpr/r                                                              !/*re         87*/
      do 120 i=1,3                                                              !/*re         88*/
  120 pt(i+1)=(ptof1*pt(i+1)+delp*xx(i))/pnew                                   !/*re         89*/
      pt(1)=pnew                                                                !/*re         90*/
  130 if (itarg.eq.0) go to 160                                                 !/*re         91*/
      ws(5)=et                                                                  !/*re         92*/
      do 140 i=1,3                                                              !/*re         93*/
  140 ws(i+5)=pt(i+1)                                                           !/*re         94*/
      if (iref.eq.0) return                                                     !/*re         95*/
      do 150 i=1,3                                                              !/*re         96*/
  150 ws(i+9)=xx(i)                                                             !/*re         97*/
      go to 180                                                                 !/*re         98*/
  160 call plt (pt,et,p,e,voc,gama)                                             !/*re         99*/
      ws(5)=e                                                                   !/*re        100*/
      do 170 i=1,3                                                              !/*re        101*/
  170 ws(i+5)=p(i+1)                                                            !/*re        102*/
      if (iref.eq.0) return                                                     !/*re        103*/
      ws(10)=xx(1)+proj(5)                                                      !/*re        104*/
      ws(11)=xx(2)+proj(6)                                                      !/*re        105*/
      ws(12)=xx(3)*gama+proj(7)                                                 !/*re        106*/
  180 ws(25)=ws(10)**2+ws(11)**2+ws(12)**2                                      !/*re        107*/
      ws(14)=sqrt(ws(25))                                                       !/*re        108*/
      go to 200                                                                 !/*re        109*/
  190 kcan=1                                                                    !/*re        110*/
      lkcan='refl'                                                              !/*re        111*/
  200 return                                                                    !/*re        112*/
!                                                                               !/*re        113*/
  210 format (' ichk>10 refl: istep,istepp,istpp,istept,istpt=',5i5/            !/*re        114*/
     & '  ws:'/4(2x,8f15.4/),'  gws:'/2x,8f15.4/2x,7f15.4/'  proj:'/            !/*re        115*/
     & 2(2x,7f15.4/))                                                           !/*re        116*/
  220 format (' big jump in refl, istepo,istep,ws(31)=',2i5,f5.1)               !/*re        117*/
      end                                                                       !/*re        118*/
