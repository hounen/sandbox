!     $Id: stnew.F,v 1.5 2002/03/27 18:15:24 gwm Exp $                        !/*st          1*/
      subroutine stnew                                                          !/*st          2*/
#include <lcs/double.h>                                                         /*!st          3*/
!     ==================================================================        !/*st          4*/
!     stnew is used to advance the nucleons of a                                !/*st          5*/
!     not-yet-reacted projectile forward through the last (past)                !/*st          6*/
!     time-step.  if this causes any of them to intersect the                   !/*st          7*/
!     target, they are stored in "st" for consideration later                   !/*st          8*/
!                                                                               !/*st          9*/
! STNEW  calls   SQRT                                                           !/*st         10*/
! called by AJTEST                                                              !/*st         11*/
!     ==================================================================        !/*st         12*/
!                                                                               !/*st         13*/
#include <lcs/inout.h>                                                          /*!st         14*/
#include <lcs/vegas.h>                                                          /*!st         15*/
      dz=ws(17)*voc*dp10                                                        !/*st         16*/
!                                                                               !/*st         17*/
! ** nuc = # of projectile particles which will                                 !/*st         18*/
!          overlap with the target (see poenew)                                 !/*st         19*/
! ** npout = # of projectile particles which will, but                          !/*st         20*/
!            still did not enter the target                                     !/*st         21*/
!                                                                               !/*st         22*/
      kk=1                                                                      !/*st         23*/
   10 proj(7)=proj(7)+dz                                                        !/*st         24*/
!           increment count of elaspsed time intervals in the cascade           !/*st         25*/
      nti=nti+1                                                                 !/*st         26*/
!-----after the nuclei passed through each other                                !/*st         27*/
!-----switch off the refraction - reflection option                             !/*st         28*/
      if (proj(7).gt.(csa(9)+proj(9))) ind(21)=3                                !/*st         29*/
      if (nnp.le.0) go to 30                                                    !/*st         30*/
!             move projectile holes forward in time (w/nucleus)                 !/*st         31*/
      do 20 i=1,nnp                                                             !/*st         32*/
   20 pnpar(3,i)=pnpar(3,i)+dz                                                  !/*st         33*/
   30 if (npout.eq.0) return                                                    !/*st         34*/
!                                                                               !/*st         35*/
!              handle nucleons of the projectile that will                      !/*st         36*/
!             ultimately hit the target                                         !/*st         37*/
!                                                                               !/*st         38*/
      do 40 k=kk,nuc                                                            !/*st         39*/
      if (jpro(k).lt.0) go to 40                                                !/*st         40*/
      zpro(k)=zpro(k)+dz                                                        !/*st         41*/
      if (zpro(k).gt.dp0) zpro(k)=dp0                                           !/*st         42*/
      xyz=xysq(k)+zpro(k)*zpro(k)                                               !/*st         43*/
      if (xyz.lt.csa(10)) go to 50                                              !/*st         44*/
   40 continue                                                                  !/*st         45*/
      if (kk.eq.1.and.icsb(1).eq.0) go to 10                                    !/*st         46*/
      return                                                                    !/*st         47*/
!                                                                               !/*st         48*/
!             one of these projectile-nucleons has just                         !/*st         49*/
!             hit the target, move it into "st"                                 !/*st         50*/
!                                                                               !/*st         51*/
   50 icsb(2)=icsb(2)+1                                                         !/*st         52*/
      icsb(1)=icsb(1)+1                                                         !/*st         53*/
      icsb(15)=icsb(15)+1                                                       !/*st         54*/
      if (icsb(15).ge.i391) go to 60                                            !/*st         55*/
      l=jpro(k)                                                                 !/*st         56*/
      jpro(k)=-jpro(k)                                                          !/*st         57*/
      npout=npout-1                                                             !/*st         58*/
      npmon(l)=npmon(l)+1                                                       !/*st         59*/
      npmon(3)=npmon(3)+1                                                       !/*st         60*/
#ifdef NPM                                                                      /*!st         61*/
      write (io,70) npmon,(ind(iii),iii=1,35)                                   !/*st         62*/
#endif                                                                          /*!st         63*/
      csa(14)=proj(12+l)                                                        !/*st         64*/
      i8=icsb(15)*i81                                                           !/*st         65*/
      st(i8+1)=poe1                                                             !/*st         66*/
      st(i8+2)=dp5                                                              !/*st         67*/
      st(i8+3)=l                                                                !/*st         68*/
      st(i8+4)=amass(l)                                                         !/*st         69*/
      st(i8+5)=epro(k)                                                          !/*st         70*/
      st(i8+6)=pxpro(k)                                                         !/*st         71*/
      st(i8+7)=pypro(k)                                                         !/*st         72*/
      st(i8+8)=pzpro(k)                                                         !/*st         73*/
      st(i8+9)=ppro(k)                                                          !/*st         74*/
      st(i8+10)=xpro(k)                                                         !/*st         75*/
      st(i8+11)=ypro(k)                                                         !/*st         76*/
      st(i8+12)=zpro(k)                                                         !/*st         77*/
      st(i8+13)=proj(12+l)                                                      !/*st         78*/
      st(i8+14)=sqrt(xyz)                                                       !/*st         79*/
      st(i8+15)=xyz                                                             !/*st         80*/
      ist=ipro(k)                                                               !/*st         81*/
      st(i8+16)=pef(l,ist)                                                      !/*st         82*/
      st(i8+17)=ws(19)                                                          !/*st         83*/
      st(i8+18)=xpro(k)                                                         !/*st         84*/
      st(i8+19)=ypro(k)                                                         !/*st         85*/
      st(i8+20)=zpro(k)                                                         !/*st         86*/
      st(i8+21)=dp0                                                             !/*st         87*/
      st(i8+i81)=k                                                              !/*st         88*/
!             if everything has been stored, return                             !/*st         89*/
!             otherwise, continue sorting out the projectile                    !/*st         90*/
      if (k.eq.nuc) return                                                      !/*st         91*/
      kk=k+1                                                                    !/*st         92*/
      go to 30                                                                  !/*st         93*/
!               cancel cascade when trying to activate                          !/*st         94*/
!               more than "i391" particles in "st"                              !/*st         95*/
   60 kcan=2                                                                    !/*st         96*/
      lkcan='stnew'                                                             !/*st         97*/
      return                                                                    !/*st         98*/
!                                                                               !/*st         99*/
   70 format ('stnew +',3i5/35i3)                                               !/*st        100*/
      end                                                                       !/*st        101*/
