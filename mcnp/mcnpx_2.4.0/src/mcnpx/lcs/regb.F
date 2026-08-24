!     $Id: regb.F,v 1.4 2002/03/27 18:15:20 gwm Exp $                         !/*re          1*/
      subroutine regb (dist)                                                    !/*re          2*/
#include <lcs/double.h>                                                         /*!re          3*/
!     ==================================================================        !/*re          4*/
!     regb moves the particles to the region boudary.                           !/*re          5*/
!     it is called when a particle is trying to cross a boundary                !/*re          6*/
!     either in the target or in the projectile (in psel),                      !/*re          7*/
!     or when the particle leaves the target or the projectile                  !/*re          8*/
!     (in rlar). on exit the particle is in the new region - itep1.             !/*re          9*/
!                                                                               !/*re         10*/
! REGB   calls   SQRT  PLT  LOOK                                                !/*re         11*/
! called by RLAR  POE  PSEL                                                     !/*re         12*/
!     ==================================================================        !/*re         13*/
!                                                                               !/*re         14*/
#include <lcs/inout.h>                                                          /*!re         15*/
#include <lcs/vegas.h>                                                          /*!re         16*/
      dimension p(4), pt(4)                                                     !/*re         17*/
!                                                                               !/*re         18*/
      bet=-voc                                                                  !/*re         19*/
      icheck=0                                                                  !/*re         20*/
      step3=dp0                                                                 !/*re         21*/
      if (ind(34).eq.1.or.ind(34).eq.3) istpp=0                                 !/*re         22*/
      if (ind(34).ge.2) istpt=0                                                 !/*re         23*/
      if (ind(8).eq.0.and.istept.eq.istpt) go to 30                             !/*re         24*/
!                                                                               !/*re         25*/
!     the following seems to preclude correctly refracting                      !/*re         26*/
!     projectile nucleons entering the target                                   !/*re         27*/
!                                                                               !/*re         28*/
      if (ind(8).gt.0.and.ws(31).ne.dp0) go to 30                               !/*re         29*/
!                                                                               !/*re         30*/
!     the case of crossing target boundary                                      !/*re         31*/
!                                                                               !/*re         32*/
      istep=min(istept,istpt)+1                                                 !/*re         33*/
      rr=xa(istep)                                                              !/*re         34*/
      rr2=rr*rr                                                                 !/*re         35*/
      step3=gws(6)*gws(10)+gws(7)*gws(11)+gws(8)*gws(12)                        !/*re         36*/
      rro2=gws(3)                                                               !/*re         37*/
      arg=step3**2-rro2+rr2                                                     !/*re         38*/
      if (arg.ge.dp0) go to 10                                                  !/*re         39*/
      write (io,60) arg,step3,rro2,rr2                                          !/*re         40*/
      write (io,70) ws(1),istept,istpt,istep                                    !/*re         41*/
      arg=dp0                                                                   !/*re         42*/
      if (mprin.eq.0) mprin=-1                                                  !/*re         43*/
   10 gf=sqrt(arg)                                                              !/*re         44*/
!                                                                               !/*re         45*/
!     dist is the distance to the new region.                                   !/*re         46*/
!                                                                               !/*re         47*/
      astp3=abs(step3)                                                          !/*re         48*/
      dist=(step3/astp3)*(gf-astp3)+1.d-5                                       !/*re         49*/
      do 20 i=6,8                                                               !/*re         50*/
   20 ws(i+4)=gws(i+4)+dist*gws(i)                                              !/*re         51*/
      ws(25)=ws(10)**2+ws(11)**2+ws(12)**2                                      !/*re         52*/
      ws(14)=sqrt(ws(25))                                                       !/*re         53*/
      go to 50                                                                  !/*re         54*/
!                                                                               !/*re         55*/
! make sure that the particle is in the new region                              !/*re         56*/
!     call look(ws(14),x,istep)                                                 !/*re         57*/
!     if(istep.eq.istpt) return                                                 !/*re         58*/
!     dist=dist+1.d-5                                                           !/*re         59*/
!     icheck=icheck+1                                                           !/*re         60*/
!     if(icheck.lt.100) go to 12                                                !/*re         61*/
!     write(6,100)istep,istpt,istept,gf,step3,rr2,rro2,ws,gws,proj              !/*re         62*/
! 100 format(//" icheck>100 in regb"/"  istep/n.step/o.step",3i5/               !/*re         63*/
!    &"  gf/step3/rr2/rro2   ",4f15.4,                                          !/*re         64*/
!    &"  ws:"/4(2x,8f15.4/),"  gws:"/3(2x,5f15.4/),                             !/*re         65*/
!    &"  proj:"/2(2x,7f15.4/))                                                  !/*re         66*/
!     stop                                                                      !/*re         67*/
!                                                                               !/*re         68*/
!     now the same for projectile boundary                                      !/*re         69*/
!                                                                               !/*re         70*/
   30 istep=min(istepp,istpp)+1                                                 !/*re         71*/
      rr=ya(istep)                                                              !/*re         72*/
      rr2=rr*rr                                                                 !/*re         73*/
      xco=gws(10)-proj(5)                                                       !/*re         74*/
      yco=gws(11)-proj(6)                                                       !/*re         75*/
      zco=(gws(12)-proj(7)+dp10*ws(16)*voc)/gama                                !/*re         76*/
      p(1)=sqrt(gws(5)*gws(5)-ws(4)*ws(4))                                      !/*re         77*/
      do 40 i=1,3                                                               !/*re         78*/
   40 p(i+1)=gws(i+5)                                                           !/*re         79*/
      call plt (p,gws(5),pt,et,bet,gama)                                        !/*re         80*/
      rro2=xco*xco+yco*yco+zco*zco                                              !/*re         81*/
      step3=pt(2)*xco+pt(3)*yco+pt(4)*zco                                       !/*re         82*/
      gf=sqrt(step3*step3-rro2+rr2)                                             !/*re         83*/
      astp3=abs(step3)                                                          !/*re         84*/
      dist=(step3/astp3)*(gf-astp3)+1.d-5                                       !/*re         85*/
      xc=xco+dist*pt(2)                                                         !/*re         86*/
      yc=yco+dist*pt(3)                                                         !/*re         87*/
      zc=zco+dist*pt(4)                                                         !/*re         88*/
      arg=sqrt(xc*xc+yc*yc+zc*zc)                                               !/*re         89*/
      call look (arg,ya,istep)                                                  !/*re         90*/
!     if(istep.eq.istpp) go to 25                                               !/*re         91*/
!     dist=dist+1.d-5                                                           !/*re         92*/
!     icheck=icheck+1                                                           !/*re         93*/
!     if(icheck.lt.100) go to 23                                                !/*re         94*/
!     write(6,100)istep,istpp,istepp,gf,step3,rr2,rro2,ws,gws,proj              !/*re         95*/
!     stop                                                                      !/*re         96*/
      ws(10)=xc+proj(5)                                                         !/*re         97*/
      ws(11)=yc+proj(6)                                                         !/*re         98*/
      ws(12)=zc*gama+proj(7)                                                    !/*re         99*/
      ws(25)=ws(10)**2+ws(11)**2+ws(12)**2                                      !/*re        100*/
      ws(14)=sqrt(ws(25))                                                       !/*re        101*/
!                                                                               !/*re        102*/
!-----------------------------------------------------------------------        !/*re        103*/
! Advancing in the projectile may push the particle out of the target.          !/*re        104*/
      if (istept.gt.0.and.ws(14).ge.csa(9)) ind(34)=2                           !/*re        105*/
!-----------------------------------------------------------------------        !/*re        106*/
!                                                                               !/*re        107*/
   50 return                                                                    !/*re        108*/
!                                                                               !/*re        109*/
   60 format (' neg sqrt in regb, arg,step3,ro2,r2=',4e15.5)                    !/*re        110*/
   70 format ('  cascade=',f7.1,'istept,pt,ep=',3(i5,2x))                       !/*re        111*/
      end                                                                       !/*re        112*/
