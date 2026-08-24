!     $Id: poenew.F,v 1.4 2002/03/27 18:15:16 gwm Exp $                       !/*po          1*/
      subroutine poenew                                                         !/*po          2*/
#include <lcs/double.h>                                                         /*!po          3*/
!     ==================================================================        !/*po          4*/
!                                                                               !/*po          5*/
! POENEW calls   FLTRNF  COS  SQRT  PLT  GETWIG                                 !/*po          6*/
! called by POE                                                                 !/*po          7*/
!     ==================================================================        !/*po          8*/
!                                                                               !/*po          9*/
#include <lcs/inout.h>                                                          /*!po         10*/
      dimension vecc(8), vecr(8), vecc0(8)                                      !/*po         11*/
#include <lcs/vegas.h>                                                          /*!po         12*/
      dimension p(4), pt(4)                                                     !/*po         13*/
!                                                                               !/*po         14*/
      data vecc /dp1,.975d0,.9d0,.7d0,.5d0,.3d0,0.1d0,.025d0/                   !/*po         15*/
      data vecc0 /dp1,.975d0,.90839d0,.78598d0,.5d0,.21402d0,.09161d0,.0        !/*po         16*/
     & 25d0/                                                                    !/*po         17*/
      data vecr /-2.5d0,-1.5d0,-.9d0,-.3d0,.3d0,.9d0,1.5d0,2.5d0/               !/*po         18*/
!                                                                               !/*po         19*/
!        get r(tar)+r(proj) = limiting impact parameter                         !/*po         20*/
!                                                                               !/*po         21*/
      cspr9=csa(9)+proj(9)                                                      !/*po         22*/
!                                                                               !/*po         23*/
      twopi=8.d0*atan(dp1)                                                      !/*po         24*/
!                                                                               !/*po         25*/
      if (yp.ge.0) go to 10                                                     !/*po         26*/
!                         replace old rejection method for x,y with             !/*po         27*/
!                         one-pass.  take "b" from "b*db" = d(b**2)             !/*po         28*/
!                         distribution, x,y=b*cos,sin(two*pi*ran1)              !/*po         29*/
!               max(r1,r2) == ri ** (1/2) , but faster, inline                  !/*po         30*/
      r1=rang()                                                                 !/*po         31*/
      r2=rang()                                                                 !/*po         32*/
      rand=max(r1,r2)                                                           !/*po         33*/
      ran1=rang()                                                               !/*po         34*/
!            x(proj), y(proj).  -yp*rand = impact parameter                     !/*po         35*/
!       (yp is restricted to be <= cspr9 in den1 [y.y.10/8/84])                 !/*po         36*/
      rinc=-yp*rand                                                             !/*po         37*/
      proj(5)=cos(twopi*ran1)*rinc                                              !/*po         38*/
      proj(6)=sin(twopi*ran1)*rinc                                              !/*po         39*/
!           if (yp.gt.0), proj(5)=0,proj(6)=yp, always                          !/*po         40*/
   10 proj(7)=-sqrt(cspr9*cspr9-proj(5)*proj(5)-proj(6)*proj(6))                !/*po         41*/
      npmon(1)=0                                                                !/*po         42*/
      npmon(2)=0                                                                !/*po         43*/
      spr(1)=dp0                                                                !/*po         44*/
      spr(2)=dp0                                                                !/*po         45*/
      nuc=proj(3)                                                               !/*po         46*/
      dr=-proj(7)/1.d+02                                                        !/*po         47*/
      th=dpth                                                                   !/*po         48*/
      r=proj(9)-2.5d0                                                           !/*po         49*/
      xx=proj(5)                                                                !/*po         50*/
      yy=proj(6)                                                                !/*po         51*/
      zz=proj(7)                                                                !/*po         52*/
      if (csa(1).eq.dp0) go to 20                                               !/*po         53*/
!                     only one nucleon(meson) incident                          !/*po         54*/
      nnuc=1                                                                    !/*po         55*/
      epro(1)=csa(2)                                                            !/*po         56*/
      jpro(1)=csa(1)                                                            !/*po         57*/
      ijpr=abs(jpro(1))                                                         !/*po         58*/
      ppro(1)=sqrt(epro(1)**2-amass(ijpr)**2)                                   !/*po         59*/
      pxpro(1)=dp0                                                              !/*po         60*/
      pypro(1)=dp0                                                              !/*po         61*/
      pzpro(1)=dp1                                                              !/*po         62*/
      xpro(1)=xx                                                                !/*po         63*/
      ypro(1)=yy                                                                !/*po         64*/
      zpro(1)=zz                                                                !/*po         65*/
      if (jpro(1).eq.1) spr(1)=dp1                                              !/*po         66*/
!          spr(2)=1, not (1)=2 [24jul84]                                        !/*po         67*/
      if (jpro(1).eq.2) spr(2)=dp1                                              !/*po         68*/
      xysq(1)=xx*xx+yy*yy                                                       !/*po         69*/
      go to 150                                                                 !/*po         70*/
!                   whole nucleus incident                                      !/*po         71*/
   20 zp=proj(4)                                                                !/*po         72*/
      apr=proj(3)                                                               !/*po         73*/
      inine=ieght+1                                                             !/*po         74*/
!                                                                               !/*po         75*/
!             loop over "nuc" nucleons of projectile, assign                    !/*po         76*/
!             x,y,z,px,py,pz,e to each before beginning cascade                 !/*po         77*/
!                                                                               !/*po         78*/
      if (iden-2) 30,90,90                                                      !/*po         79*/
!                        following is original code, for                        !/*po         80*/
!                        original (pre los alamos) values of iden               !/*po         81*/
   30 nnuc=0                                                                    !/*po         82*/
      do 80 k=1,nuc                                                             !/*po         83*/
   40 continue                                                                  !/*po         84*/
      rand=rang()                                                               !/*po         85*/
      ri=proj(9)*rand**th                                                       !/*po         86*/
      rr=ri-r                                                                   !/*po         87*/
!                     iden=1 (original isobar) density distributions            !/*po         88*/
      do 50 ii=1,8                                                              !/*po         89*/
      if (rr.gt.vecr(ii)) go to 50                                              !/*po         90*/
      if (iden.eq.1) c=vecc(ii)                                                 !/*po         91*/
      if (iden.eq.0) c=vecc0(ii)                                                !/*po         92*/
      i=9-ii                                                                    !/*po         93*/
      go to 60                                                                  !/*po         94*/
   50 continue                                                                  !/*po         95*/
      go to 40                                                                  !/*po         96*/
   60 continue                                                                  !/*po         97*/
      rand=rang()                                                               !/*po         98*/
      if (rand.gt.c) go to 40                                                   !/*po         99*/
      rand=rang()                                                               !/*po        100*/
      ran1=rang()                                                               !/*po        101*/
      cost=-dp1+dp2*rand                                                        !/*po        102*/
      fi=ran1*6.28318d0                                                         !/*po        103*/
      sint=sqrt(dp1-cost**2)                                                    !/*po        104*/
      cosfi=cos(fi)                                                             !/*po        105*/
      sinfi=sin(fi)                                                             !/*po        106*/
      xpr=xx+ri*sint*cosfi                                                      !/*po        107*/
      ypr=yy+ri*sint*sinfi                                                      !/*po        108*/
      zpr=zz+ri*cost*gama                                                       !/*po        109*/
!                    now x,y,z(i) determined for this nucleon                   !/*po        110*/
!                   determine if proton or neutron                              !/*po        111*/
      xys=xpr*xpr+ypr*ypr                                                       !/*po        112*/
      rand=rang()                                                               !/*po        113*/
      za=zp/apr                                                                 !/*po        114*/
      ll=2                                                                      !/*po        115*/
      apr=apr-1                                                                 !/*po        116*/
      if (rand.gt.za) go to 70                                                  !/*po        117*/
      ll=1                                                                      !/*po        118*/
      zp=zp-1                                                                   !/*po        119*/
   70 jpr0=ll                                                                   !/*po        120*/
      fm=ppfr(ll,i)                                                             !/*po        121*/
      amas=amass(ll)*amass(ll)                                                  !/*po        122*/
      fepm=sqrt(fm*fm+amas)                                                     !/*po        123*/
      et=csa(2)                                                                 !/*po        124*/
      eh0=dp0                                                                   !/*po        125*/
      pt(1)=dp0                                                                 !/*po        126*/
      pt(2)=dp0                                                                 !/*po        127*/
      pt(3)=dp0                                                                 !/*po        128*/
      pt(4)=dp1                                                                 !/*po        129*/
!                 now assign a fermi-momentum to the nucleon                    !/*po        130*/
      rand=rang()                                                               !/*po        131*/
      p(1)=fm*rand**th                                                          !/*po        132*/
      rand=rang()                                                               !/*po        133*/
      p(4)=-dp1+dp2*rand                                                        !/*po        134*/
      rand=rang()                                                               !/*po        135*/
      fi=rand*6.2831853                                                         !/*po        136*/
      sint=sqrt(dp1-p(4)*p(4))                                                  !/*po        137*/
      p(2)=sint*cos(fi)                                                         !/*po        138*/
      p(3)=sint*sin(fi)                                                         !/*po        139*/
      e=sqrt(p(1)*p(1)+amas)                                                    !/*po        140*/
      eh0=fepm-e                                                                !/*po        141*/
      call plt (p,e,pt,et,voc,gama)                                             !/*po        142*/
      if (xys.ge.csa(10)) go to 80                                              !/*po        143*/
!-----------------------------------------------------------------------        !/*po        144*/
! Treat projectile virgins below Fermi sea of target as noninteracting          !/*po        145*/
! available to be scattered up later.                                           !/*po        146*/
      if (et.le.(amass(jpr0)+ef(jpr0,1))) go to 80                              !/*po        147*/
!-----------------------------------------------------------------------        !/*po        148*/
      nnuc=nnuc+1                                                               !/*po        149*/
      xpro(nnuc)=xpr                                                            !/*po        150*/
      ypro(nnuc)=ypr                                                            !/*po        151*/
      zpro(nnuc)=zpr                                                            !/*po        152*/
      xysq(nnuc)=xys                                                            !/*po        153*/
      ppro(nnuc)=pt(1)                                                          !/*po        154*/
      pxpro(nnuc)=pt(2)                                                         !/*po        155*/
      pypro(nnuc)=pt(3)                                                         !/*po        156*/
      pzpro(nnuc)=pt(4)                                                         !/*po        157*/
      epro(nnuc)=et                                                             !/*po        158*/
      jpro(nnuc)=jpr0                                                           !/*po        159*/
      spr(jpr0)=spr(jpr0)+1                                                     !/*po        160*/
      eh(nnuc)=eh0                                                              !/*po        161*/
      ph(1,nnuc)=-p(2)*p(1)                                                     !/*po        162*/
      ph(2,nnuc)=-p(3)*p(1)                                                     !/*po        163*/
      ph(3,nnuc)=-p(4)*p(1)                                                     !/*po        164*/
      ipro(nnuc)=i                                                              !/*po        165*/
   80 continue                                                                  !/*po        166*/
      if (nnuc.gt.0) go to 150                                                  !/*po        167*/
      go to 140                                                                 !/*po        168*/
!                                                                               !/*po        169*/
!          following is for iden .ge. 2 options                                 !/*po        170*/
!          added 4 dec 83, mrc                                                  !/*po        171*/
!                                                                               !/*po        172*/
   90 nnuc=0                                                                    !/*po        173*/
!      next 2 lines handle iden=5,6 cases of square-well densities              !/*po        174*/
      rp=r0p                                                                    !/*po        175*/
      if (ap.ne.dp0) rp=r0p*ap                                                  !/*po        176*/
!                                                                               !/*po        177*/
      do 130 k=1,nuc                                                            !/*po        178*/
      rand=rang()                                                               !/*po        179*/
!                      getwig returns ri with the appropriate                   !/*po        180*/
!                      distribution, so no need for a rejection-method          !/*po        181*/
!                      to see if the square-well distribution result            !/*po        182*/
!                      satisfies the diffuse c.d.f.                             !/*po        183*/
      call getwig (rp,ap,ri,rand)                                               !/*po        184*/
!                   occassional diffuse nucleon may be                          !/*po        185*/
!                   beyond y(1), so put it in the 1st bin                       !/*po        186*/
      if (ri.gt.ya(1)) ri=ya(1)                                                 !/*po        187*/
!                                                                               !/*po        188*/
      rand=rang()                                                               !/*po        189*/
      ran1=rang()                                                               !/*po        190*/
      cost=-dp1+dp2*rand                                                        !/*po        191*/
      fi=ran1*twopi                                                             !/*po        192*/
      sint=sqrt(dp1-cost**2)                                                    !/*po        193*/
      cosfi=cos(fi)                                                             !/*po        194*/
      sinfi=sin(fi)                                                             !/*po        195*/
      xpr=xx+ri*sint*cosfi                                                      !/*po        196*/
      ypr=yy+ri*sint*sinfi                                                      !/*po        197*/
      zpr=zz+ri*cost*gama                                                       !/*po        198*/
!                    now x,y,z(i) determined for this nucleon                   !/*po        199*/
!                   determine if proton or neutron                              !/*po        200*/
      xys=xpr*xpr+ypr*ypr                                                       !/*po        201*/
      rand=rang()                                                               !/*po        202*/
      za=zp/apr                                                                 !/*po        203*/
      ll=2                                                                      !/*po        204*/
      apr=apr-1                                                                 !/*po        205*/
      if (rand.gt.za) go to 100                                                 !/*po        206*/
      ll=1                                                                      !/*po        207*/
      zp=zp-1                                                                   !/*po        208*/
  100 jpr0=ll                                                                   !/*po        209*/
!                                                                               !/*po        210*/
!             now we have to determine which region the                         !/*po        211*/
!             nucleon is in, in order to assign the correct                     !/*po        212*/
!             fermi-momentum                                                    !/*po        213*/
      do 110 ii=1,inine                                                         !/*po        214*/
      if (ri.le.ya(ii)) go to 110                                               !/*po        215*/
      i=ii-1                                                                    !/*po        216*/
      go to 120                                                                 !/*po        217*/
  110 continue                                                                  !/*po        218*/
!                                                                               !/*po        219*/
  120 fm=ppfr(ll,i)                                                             !/*po        220*/
      afm=ppfw(ll,i)                                                            !/*po        221*/
      amas=amass(ll)*amass(ll)                                                  !/*po        222*/
      fepm=sqrt(fm*fm+amas)                                                     !/*po        223*/
      et=csa(2)                                                                 !/*po        224*/
      eh0=dp0                                                                   !/*po        225*/
      pt(1)=dp0                                                                 !/*po        226*/
      pt(2)=dp0                                                                 !/*po        227*/
      pt(3)=dp0                                                                 !/*po        228*/
      pt(4)=dp1                                                                 !/*po        229*/
!                 now assign a fermi-momentum to the nucleon                    !/*po        230*/
      rand=rang()                                                               !/*po        231*/
      call getwig (fm,afm,ri,rand)                                              !/*po        232*/
      p(1)=ri                                                                   !/*po        233*/
      rand=rang()                                                               !/*po        234*/
      ran1=rang()                                                               !/*po        235*/
      p(4)=-dp1+dp2*rand                                                        !/*po        236*/
      fi=ran1*twopi                                                             !/*po        237*/
      sint=sqrt(dp1-p(4)*p(4))                                                  !/*po        238*/
      p(2)=sint*cos(fi)                                                         !/*po        239*/
      p(3)=sint*sin(fi)                                                         !/*po        240*/
      e=sqrt(p(1)*p(1)+amas)                                                    !/*po        241*/
      eh0=fepm-e                                                                !/*po        242*/
      call plt (p,e,pt,et,voc,gama)                                             !/*po        243*/
      if (xys.ge.csa(10)) go to 130                                             !/*po        244*/
!-----------------------------------------------------------------------        !/*po        245*/
! Treat projectile virgins below Fermi sea of target as noninteracting          !/*po        246*/
! available to be scattered up later.                                           !/*po        247*/
      if (et.le.(amass(jpr0)+ef(jpr0,1))) go to 130                             !/*po        248*/
!-----------------------------------------------------------------------        !/*po        249*/
      nnuc=nnuc+1                                                               !/*po        250*/
      xpro(nnuc)=xpr                                                            !/*po        251*/
      ypro(nnuc)=ypr                                                            !/*po        252*/
      zpro(nnuc)=zpr                                                            !/*po        253*/
      xysq(nnuc)=xys                                                            !/*po        254*/
      ppro(nnuc)=pt(1)                                                          !/*po        255*/
      pxpro(nnuc)=pt(2)                                                         !/*po        256*/
      pypro(nnuc)=pt(3)                                                         !/*po        257*/
      pzpro(nnuc)=pt(4)                                                         !/*po        258*/
      epro(nnuc)=et                                                             !/*po        259*/
      jpro(nnuc)=jpr0                                                           !/*po        260*/
      spr(jpr0)=spr(jpr0)+1                                                     !/*po        261*/
      eh(nnuc)=eh0                                                              !/*po        262*/
      ph(1,nnuc)=-p(2)*p(1)                                                     !/*po        263*/
      ph(2,nnuc)=-p(3)*p(1)                                                     !/*po        264*/
      ph(3,nnuc)=-p(4)*p(1)                                                     !/*po        265*/
      ipro(nnuc)=i                                                              !/*po        266*/
  130 continue                                                                  !/*po        267*/
      if (nnuc.gt.0) go to 150                                                  !/*po        268*/
!                                                                               !/*po        269*/
!     if every projectile nucleon missed the outer-target                       !/*po        270*/
!     radius, increment the count of transparencies and total                   !/*po        271*/
!     points of entry (poe1)                                                    !/*po        272*/
!                                                                               !/*po        273*/
  140 time=dp0                                                                  !/*po        274*/
      poe1=poe1+1                                                               !/*po        275*/
      iws1=poe1                                                                 !/*po        276*/
      transp=transp+1                                                           !/*po        277*/
      if (mprin.gt.2) write (io,230) iws1,proj(5),proj(6),time                  !/*po        278*/
      ind(7)=1                                                                  !/*po        279*/
      return                                                                    !/*po        280*/
!                                                                               !/*po        281*/
!       continue set-up for 1 or many nucleons incident                         !/*po        282*/
!                                                                               !/*po        283*/
  150 nuc=nnuc                                                                  !/*po        284*/
!prt  write(6,200) (jpro(k),epro(k),xpro(k),ypro(k),zpro(k),                    !/*po        285*/
!prt 1ppro(k),pxpro(k),pypro(k),pzpro(k) ,k=1,nuc)                              !/*po        286*/
      npout=nuc-1                                                               !/*po        287*/
      spr(3)=proj(4)-spr(1)                                                     !/*po        288*/
      spr(4)=proj(3)-proj(4)-spr(2)                                             !/*po        289*/
  160 do 170 k=1,nuc                                                            !/*po        290*/
      zpro(k)=zpro(k)+dr                                                        !/*po        291*/
  170 if (zpro(k).gt.dp0) zpro(k)=dp0                                           !/*po        292*/
      proj(7)=proj(7)+dr                                                        !/*po        293*/
      do 180 kk=1,nuc                                                           !/*po        294*/
      k=kk                                                                      !/*po        295*/
      xyz=xysq(k)+zpro(k)*zpro(k)                                               !/*po        296*/
      if (xyz.lt.csa(10)) go to 190                                             !/*po        297*/
  180 continue                                                                  !/*po        298*/
      go to 160                                                                 !/*po        299*/
  190 l=jpro(k)                                                                 !/*po        300*/
      jpro(k)=-jpro(k)                                                          !/*po        301*/
      if (jpro(k).gt.0) jpro(k)=jpro(k)-100                                     !/*po        302*/
      npmon(3)=csa(3)                                                           !/*po        303*/
      if (l.lt.0.or.l.gt.2) go to 200                                           !/*po        304*/
      npmon(l)=1                                                                !/*po        305*/
      npmon(3)=npmon(3)+1                                                       !/*po        306*/
#ifdef NPM                                                                      /*!po        307*/
      write (io,220) npmon,(ind(iii),iii=1,35)                                  !/*po        308*/
#endif                                                                          /*!po        309*/
  200 ws(31)=k                                                                  !/*po        310*/
      if (csa(1).ne.dp0) ws(31)=dp0                                             !/*po        311*/
      ws(3)=l                                                                   !/*po        312*/
      ws(5)=epro(k)                                                             !/*po        313*/
      ws(6)=pxpro(k)                                                            !/*po        314*/
      ws(7)=pypro(k)                                                            !/*po        315*/
      ws(8)=pzpro(k)                                                            !/*po        316*/
      ws(9)=ppro(k)                                                             !/*po        317*/
      ws(10)=xpro(k)                                                            !/*po        318*/
      ws(11)=ypro(k)                                                            !/*po        319*/
      ws(12)=zpro(k)                                                            !/*po        320*/
      ws25=ws(10)*ws(10)+ws(11)*ws(11)                                          !/*po        321*/
      do 210 i=1,8                                                              !/*po        322*/
      gws(i+4)=ws(i+4)                                                          !/*po        323*/
  210 continue                                                                  !/*po        324*/
      gws(12)=gws(12)-dr                                                        !/*po        325*/
      ws(25)=ws25+ws(12)*ws(12)                                                 !/*po        326*/
      ws(14)=sqrt(ws(25))                                                       !/*po        327*/
      gws(3)=ws25+gws(12)*gws(12)                                               !/*po        328*/
      gws(1)=sqrt(gws(3))                                                       !/*po        329*/
      gws(2)=dp0                                                                !/*po        330*/
      gws(15)=ws(31)                                                            !/*po        331*/
      if (csa(1).ne.dp0) return                                                 !/*po        332*/
      iws3=ws(3)                                                                !/*po        333*/
      ist=ipro(k)                                                               !/*po        334*/
      ws(24)=pef(iws3,ist)                                                      !/*po        335*/
      csa(14)=proj(12+l)                                                        !/*po        336*/
      ws(13)=csa(14)                                                            !/*po        337*/
      return                                                                    !/*po        338*/
!                                                                               !/*po        339*/
  220 format ('poenew ',3i5/35i3)                                               !/*po        340*/
  230 format (1x,i5,80x,2f6.1,1x,f7.2)                                          !/*po        341*/
      end                                                                       !/*po        342*/
