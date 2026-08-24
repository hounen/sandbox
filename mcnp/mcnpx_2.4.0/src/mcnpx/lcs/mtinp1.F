!     $Id: mtinp1.F,v 1.4 2002/03/27 18:15:11 gwm Exp $                       !/*mt          1*/
      subroutine mtinp1 (mxa,nel,mel,a9,z9,fmf,iza,fme,jmd)                     !/*mt          2*/
!     ==================================================================        !/*mt          3*/
!        LAHET ROUTINE REWRITTEN FOR USE IN MCMP                                !/*mt          4*/
!        Defines materials in LAHET form                                        !/*mt          5*/
!     ==================================================================        !/*mt          6*/
#include <lcs/double.h>                                                         /*!mt          7*/
#include <lcs/lptS.h>                                                           /*!mt          8*/
#include <lcs/inout.h>                                                          /*!mt          9*/
#include <lcs/concom.h>                                                         /*!mt         10*/
      dimension nel(1), mel(1), a9(1), z9(1), fmf(1), fme(1), iza(1),           !/*mt         11*/
     & jmd(1)                                                                   !/*mt         12*/
      mel(1)=1                                                                  !/*mt         13*/
      nerr=0                                                                    !/*mt         14*/
      do 20 m=1,mxmat                                                           !/*mt         15*/
      nel1=0                                                                    !/*mt         16*/
      nel(m)=0                                                                  !/*mt         17*/
      do 10 l=jmd(m),jmd(m+1)-1                                                 !/*mt         18*/
      iz=iza(l)/1000                                                            !/*mt         19*/
      zzx=iz                                                                    !/*mt         20*/
      ia=mod(iza(l),1000)                                                       !/*mt         21*/
      ax=ia                                                                     !/*mt         22*/
!        TEMPORARY expedient: special treatment for 1004.id and 1005.id         !/*mt         23*/
!        for mixed ortho and para S(alpha,beta) sets for H-1 and H-2.           !/*mt         24*/
      if (iz.eq.1) then                                                         !/*mt         25*/
         if(ia.eq.4)ax=dp1                                                      !/*mt         26*/
         if(ia.eq.5)ax=dp2                                                      !/*mt         27*/
      endif                                                                     !/*mt         28*/
!                                                                               !/*mt         29*/
      denx=fme(l)                                                               !/*mt         30*/
      call afill (nel(m),zzx,ax,denx,z9(mel(m)+nel(m)),a9(mel(m)+nel(m))        !/*mt         31*/
     & ,fmf(mel(m)+nel(m)),nerr)                                                !/*mt         32*/
   10 continue                                                                  !/*mt         33*/
      mel(m+1)=mel(m)+nel(m)                                                    !/*mt         34*/
   20 continue                                                                  !/*mt         35*/
      if(nerr.gt.0)call erprnt(1,1,0,0,0,0,0,0,'18hMTINP1.  nerr.gt.0')         !/*mt         36*/
      return                                                                    !/*mt         37*/
      end                                                                       !/*mt         38*/
