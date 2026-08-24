!     $Id: gd3tal.F,v 1.12 2002/06/14 17:05:55 jxh Exp $                        !/*gd          1*/
      subroutine gd3tal(mm,da,ig,zd,jz)                                         !/*gd          2*/
#ifdef MESHTAL                                                                  /*!gd          3*/
!                                                                               !/*gd          4*/
!       This subroutine is designed to score a volume tally like                !/*gd          5*/
!       quantity in a three dimensional spherical mesh for later                !/*gd          6*/
!       post processing with any number of graphics packages.                   !/*gd          7*/
!                                                                               !/*gd          9*/
!       mm=1   Locate source or banked particle relative to mesh                !/*gd         10*/
!              and score if this is a point mesh.                               !/*gd         11*/
!       mm=2   Track particle a distance 'da' and score tally info.             !/*gd         12*/
!       mm=5   Score the track of a point detector contribution.                !/*gd         13*/
!       mm=6   Score the track of a dxtran contribution.                        !/*gd         14*/
!       mm=8   Score energy deposite locally.                                   !/*gd         15*/
!                                                                               !/*gd         16*/
      use global_data                                                           !/*gd3c        1*/
!                                                                               !/*gd3c        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gd3c        3*/
      implicit integer(kindi) (i-n)                                             !/*gd3c        4*/
!                                                                               !/*gd3c        5*/
!                                                                               !/*gd         18*/
      parameter (two_pi=6.2831853071796d0)                                      !/*gd         19*/
      parameter (deg_90=90.d0,deg_180=180.d0,deg_270=270.d0,                    !/*gd         20*/
     & deg_360=360.d0)                                                          !/*gd         21*/
      parameter (to_degs=deg_360/two_pi,to_rads=two_pi/deg_360)                 !/*gd         22*/
      parameter (ep=1.d-16)                                                     !/*gd         23*/
!                                                                               !/*gd         24*/
      common /meshtal/ ergl(maxg),ergh(maxg),rdos(5,maxg),                      !/*gd         25*/
     & rmult(maxg),mdos(3,5,maxg),mmult(3,maxg),nemsh(maxg),                    !/*gd         26*/
     & mshtr(maxg),mstyp(maxg),mshpt(maxg),ng1(maxg),ng2(maxg),                 !/*gd         27*/
     & ng3(maxg),mxgt(maxg),ngv(maxg,maxgv),mxgc(maxg),mxgv(maxg)               !/*gd         28*/
!                                                                               !/*gd         29*/
      dimension zd(10),jz(5)                                                    !/*gd         30*/
      dimension tr(12)                                                          !/*gd         31*/
!                                                                               !/*gd         32*/
!       jz(1) = material number                                                 !/*gd         33*/
!       jz(2) = mesh tally point source flag.                                   !/*gd         34*/
!               0 = Not a new particle                                          !/*gd         35*/
!               1 = Samples from the source distribution.                       !/*gd         36*/
!               2 = Secondary particle from collision                           !/*gd         37*/
!               3 = Particle from surface source file.                          !/*gd         38*/
!       jz(3 - 4) currently not used.                                           !/*gd         39*/
!       jz(5) = energy deposition type.                                         !/*gd         40*/
!       zd(1 - 6) = x,y,z,u,v,w                                                 !/*gd         41*/
!       zd(7) = track weight                                                    !/*gd         42*/
!       zd(8) = energy at start of track.                                       !/*gd         43*/
!       zd(9) = energy lost along track of at point.                            !/*gd         44*/
!       zd(10) currently not used.                                              !/*gd         45*/
!                                                                               !/*gd         46*/
      d=zero                                                                    !/*gd         47*/
!                                                                               !/*gd         48*/
!       ip = particle type.                                                     !/*gd         49*/
!       jp = mesh type.                                                         !/*gd         50*/
!                                                                               !/*gd         51*/
      ip=mshpt(ig)                                                              !/*gd         52*/
      jp=mod(mstyp(ig),10)                                                      !/*gd         53*/
!                                                                               !/*gd         54*/
      f_wt=zd(7)                                                                !/*gd         55*/
      f_eg=zd(8)                                                                !/*gd         56*/
      f_ed=zd(9)                                                                !/*gd         57*/
      mk=jz(1)                                                                  !/*gd         58*/
      mt=jz(5)                                                                  !/*gd         59*/
!                                                                               !/*gd         60*/
      m1=mmult(1,ig)                                                            !/*gd         61*/
      if(m1.ne.0) then                                                          !/*gd         62*/
         m2=mmult(2,ig)                                                         !/*gd         63*/
         if(m2.eq.0)m2=1                                                        !/*gd         64*/
         m3=mmult(3,ig)                                                         !/*gd         65*/
         rx=rmult(ig)                                                           !/*gd         66*/
         if(rx.eq.zero)rx=one                                                   !/*gd         67*/
      endif                                                                     !/*gd         68*/
!                                                                               !/*gd         69*/
!       See if something needs to be done for this track.                       !/*gd         70*/
!                                                                               !/*gd         71*/
      if(f_eg.gt.ergh(ig).or.f_eg.le.ergl(ig))return                            !/*gd         72*/
      if((jp.eq.0.or.jp.ge.3).and.ip.ne.ipt)return                              !/*gd         73*/
      if(mm.eq.1.and.jp.eq.1)go to 10                                           !/*gd         74*/
      if((mm.eq.2.or.mm.eq.8).and.(jp.eq.0.or.jp.eq.2))go to 10                 !/*gd         75*/
      if((mm.eq.5.and.jp.eq.3).or.(mm.eq.6.and.jp.eq.4))go to 10                !/*gd         76*/
      return                                                                    !/*gd         77*/
!                                                                               !/*gd         78*/
!       Transform to coordinate system of mesh.                                 !/*gd         79*/
   10 m=mshtr(ig)                                                               !/*gd         80*/
!                                                                               !/*gd         81*/
      if(m.eq.0)then                                                            !/*gd         82*/
         xx=zd(1)                                                               !/*gd         83*/
         yy=zd(2)                                                               !/*gd         84*/
         zz=zd(3)                                                               !/*gd         85*/
         uu=zd(4)                                                               !/*gd         86*/
         vv=zd(5)                                                               !/*gd         87*/
         ww=zd(6)                                                               !/*gd         88*/
      else                                                                      !/*gd         89*/
         call trfdat(m,tr)                                                      !/*gd         90*/
         xx=zd(1)*tr(4)+zd(2)*tr(7)+zd(3)*tr(10)+tr(1)                          !/*gd         91*/
         yy=zd(1)*tr(5)+zd(2)*tr(8)+zd(3)*tr(11)+tr(2)                          !/*gd         92*/
         zz=zd(1)*tr(6)+zd(2)*tr(9)+zd(3)*tr(12)+tr(3)                          !/*gd         93*/
         uu=zd(4)*tr(4)+zd(5)*tr(7)+zd(6)*tr(10)                                !/*gd         94*/
         vv=zd(4)*tr(5)+zd(5)*tr(8)+zd(6)*tr(11)                                !/*gd         95*/
         ww=zd(4)*tr(6)+zd(5)*tr(9)+zd(6)*tr(12)                                !/*gd         96*/
      endif                                                                     !/*gd         97*/
!                                                                               !/*gd         98*/
!       Determine the location of starting point of this track                  !/*gd         99*/
!       in each mesh for the proper particle type.                              !/*gd        100*/
!       Also score source weight if required.                                   !/*gd        101*/
!                                                                               !/*gd        102*/
      i1=0                                                                      !/*gd        103*/
      i2=0                                                                      !/*gd        104*/
      i3=0                                                                      !/*gd        105*/
      j1=0                                                                      !/*gd        106*/
      j2=0                                                                      !/*gd        107*/
      j3=0                                                                      !/*gd        108*/
!                                                                               !/*gd        109*/
      rt=sqrt(xx*xx+yy*yy)                                                      !/*gd        110*/
      if(rt.eq.0.and.abs(ww).eq.1.)go to 420                                    !/*gd        111*/
!                                                                               !/*gd        112*/
      r=sqrt(xx*xx+yy*yy+zz*zz)                                                 !/*gd        113*/
      if(r-ep.gt.gd1(lgd1(ig)+ng1(ig)))go to 100                                !/*gd        114*/
      if(gd1(lgd1(ig)+1).gt.0.) then                                            !/*gd        115*/
         if(r+ep.lt.gd1(lgd1(ig)+1)) then                                       !/*gd        116*/
            i1=1                                                                !/*gd        117*/
            i2=1                                                                !/*gd        118*/
            i3=1                                                                !/*gd        119*/
            go to 100                                                           !/*gd        120*/
         endif                                                                  !/*gd        121*/
      else                                                                      !/*gd        122*/
         if(r.eq.0.) then                                                       !/*gd        123*/
            i1=2                                                                !/*gd        124*/
            go to 40                                                            !/*gd        125*/
         endif                                                                  !/*gd        126*/
      endif                                                                     !/*gd        127*/
!                                                                               !/*gd        128*/
!       Determine indicies of point rr,pa,th,where                              !/*gd        129*/
!       rr is the sphere radii, pa is the polar                                 !/*gd        130*/
!       angle, and th is the azmuthital angle.                                  !/*gd        131*/
!       Then determine if this point is on a mesh boundary.                     !/*gd        132*/
!                                                                               !/*gd        133*/
      an=(xx/r)*uu+(yy/r)*vv+(zz/r)*ww                                          !/*gd        134*/
      if(gd1(lgd1(ig)+1).gt.0.) then                                            !/*gd        135*/
         if(abs(r-gd1(lgd1(ig)+1)).le.ep) then                                  !/*gd        136*/
            if(an.lt.0.) then                                                   !/*gd        137*/
               i1=1                                                             !/*gd        138*/
               i2=1                                                             !/*gd        139*/
               i3=1                                                             !/*gd        140*/
               go to 100                                                        !/*gd        141*/
            else                                                                !/*gd        142*/
               j1=1                                                             !/*gd        143*/
               i1=2                                                             !/*gd        144*/
               go to 40                                                         !/*gd        145*/
            endif                                                               !/*gd        146*/
         endif                                                                  !/*gd        147*/
      endif                                                                     !/*gd        148*/
!                                                                               !/*gd        149*/
      if(abs(r-gd1(lgd1(ig)+ng1(ig))).le.ep) then                               !/*gd        150*/
         if(an.lt.0.) then                                                      !/*gd        151*/
            i1=ng1(ig)                                                          !/*gd        152*/
            j1=2                                                                !/*gd        153*/
            go to 40                                                            !/*gd        154*/
         else                                                                   !/*gd        155*/
            go to 420                                                           !/*gd        156*/
         endif                                                                  !/*gd        157*/
      endif                                                                     !/*gd        158*/
!                                                                               !/*gd        159*/
      ic=1                                                                      !/*gd        160*/
      ib=ng1(ig)                                                                !/*gd        161*/
   20 if(ib-ic.eq.1) go to 30                                                   !/*gd        162*/
      ih=(ic+ib)/2                                                              !/*gd        163*/
      if(r.lt.gd1(lgd1(ig)+ih)) go to 25                                        !/*gd        164*/
      ic=ih                                                                     !/*gd        165*/
      go to 20                                                                  !/*gd        166*/
   25 ib=ih                                                                     !/*gd        167*/
      go to 20                                                                  !/*gd        168*/
!                                                                               !/*gd        169*/
   30 i1=ic+1                                                                   !/*gd        170*/
      if(r.eq.gd1(lgd1(ig)+i1-1)) then                                          !/*gd        171*/
          if(an.gt.0.) then                                                     !/*gd        172*/
             j1=1                                                               !/*gd        173*/
          else                                                                  !/*gd        174*/
             i1=i1-1                                                            !/*gd        175*/
             j1=2                                                               !/*gd        176*/
          endif                                                                 !/*gd        177*/
       endif                                                                    !/*gd        178*/
!                                                                               !/*gd        179*/
   40 if(ng2(ig).gt.1) then                                                     !/*gd        180*/
         if(r.eq.0.) then                                                       !/*gd        181*/
            xt=xx+1.*uu                                                         !/*gd        182*/
            yt=yy+1.*vv                                                         !/*gd        183*/
            zt=zz+1.*ww                                                         !/*gd        184*/
         else                                                                   !/*gd        185*/
            xt=xx                                                               !/*gd        186*/
            yt=yy                                                               !/*gd        187*/
            zt=zz                                                               !/*gd        188*/
         endif                                                                  !/*gd        189*/
         rj=sqrt(xt*xt+yt*yt+zt*zt)                                             !/*gd        190*/
         cc=min(one,max(-one,zt/rj))                                            !/*gd        191*/
         an=to_degs*acos(cc)                                                    !/*gd        192*/
         if(xt*yt.eq.zero.and.abs(ww).eq.1.) go to 420                          !/*gd        193*/
      else                                                                      !/*gd        194*/
         an=zero                                                                !/*gd        195*/
         i2=1                                                                   !/*gd        196*/
         go to 70                                                               !/*gd        197*/
      endif                                                                     !/*gd        198*/
!                                                                               !/*gd        199*/
      j2=0                                                                      !/*gd        200*/
      if(abs(an-gd2(lgd2(ig)+ng2(ig))).lt.ep) then                              !/*gd        201*/
         i2=ng2(ig)                                                             !/*gd        202*/
         j2=2                                                                   !/*gd        203*/
         go to 70                                                               !/*gd        204*/
      endif                                                                     !/*gd        205*/
      if(abs(an).lt.ep)then                                                     !/*gd        206*/
         i2=1                                                                   !/*gd        207*/
         j2=1                                                                   !/*gd        208*/
         go to 70                                                               !/*gd        209*/
      endif                                                                     !/*gd        210*/
!                                                                               !/*gd        211*/
      if(abs(an-gd2(lgd2(ig)+1)).lt.ep) then                                    !/*gd        212*/
         if(ww.ge.an) then                                                      !/*gd        213*/
            i3=1                                                                !/*gd        214*/
            j3=2                                                                !/*gd        215*/
         else                                                                   !/*gd        216*/
            i3=2                                                                !/*gd        217*/
            j3=1                                                                !/*gd        218*/
         endif                                                                  !/*gd        219*/
         go to 70                                                               !/*gd        220*/
      endif                                                                     !/*gd        221*/
!                                                                               !/*gd        222*/
      if(abs(an-gd2(lgd2(ig)+ng2(ig)-1)).lt.ep) then                            !/*gd        223*/
         if(ww.ge.an) then                                                      !/*gd        224*/
            i3=ng2(ig)-1                                                        !/*gd        225*/
            j3=2                                                                !/*gd        226*/
         else                                                                   !/*gd        227*/
            i3=ng2(ig)                                                          !/*gd        228*/
            j3=1                                                                !/*gd        229*/
         endif                                                                  !/*gd        230*/
         go to 70                                                               !/*gd        231*/
      endif                                                                     !/*gd        232*/
!                                                                               !/*gd        233*/
      if(an.lt.gd2(lgd2(ig)+1)-ep) then                                         !/*gd        234*/
         i2=1                                                                   !/*gd        235*/
         go to 70                                                               !/*gd        236*/
      endif                                                                     !/*gd        237*/
      if(an.gt.gd2(lgd2(ig)+ng2(ig)-1)+ep) then                                 !/*gd        238*/
         i2=ng2(ig)                                                             !/*gd        239*/
         go to 70                                                               !/*gd        240*/
      endif                                                                     !/*gd        241*/
!                                                                               !/*gd        242*/
      ic=1                                                                      !/*gd        243*/
      ib=ng2(ig)-1                                                              !/*gd        244*/
   50 if(ib-ic.eq.1) go to 60                                                   !/*gd        245*/
      ih=(ic+ib)/2                                                              !/*gd        246*/
      if(an.lt.gd2(lgd2(ig)+ih)) go to 55                                       !/*gd        247*/
      ic=ih                                                                     !/*gd        248*/
      go to 50                                                                  !/*gd        249*/
   55 ib=ih                                                                     !/*gd        250*/
      go to 50                                                                  !/*gd        251*/
!                                                                               !/*gd        252*/
   60 i2=ic                                                                     !/*gd        253*/
      gj=0.                                                                     !/*gd3c        6*/
      if(i2.gt.1)gj=gd2(lgd2(ig)+i2-1)                                          !/*gd3c        7*/
      if(abs(an-gj).lt.ep) go to 65                                             !/*gd3c        8*/
      i2=i2+1                                                                   !/*gd        255*/
      go to 70                                                                  !/*gd        256*/
!                                                                               !/*gd        257*/
   65 if(ww.ge.an) then                                                         !/*gd        258*/
         i2=i2+1                                                                !/*gd        259*/
         j2=1                                                                   !/*gd        260*/
      else                                                                      !/*gd        261*/
         j2=2                                                                   !/*gd        262*/
      endif                                                                     !/*gd        263*/
!                                                                               !/*gd        264*/
   70 if(ng3(ig).gt.1) then                                                     !/*gd        265*/
         if(r.eq.0.) then                                                       !/*gd        266*/
            xt=xx+1.*uu                                                         !/*gd        267*/
            yt=yy+1.*vv                                                         !/*gd        268*/
         else                                                                   !/*gd        269*/
            xt=xx                                                               !/*gd        270*/
            yt=yy                                                               !/*gd        271*/
         endif                                                                  !/*gd        272*/
         rj=sqrt(xt*xt+yt*yt)                                                   !/*gd        273*/
         cc=min(one,max(-one,xt/rj))                                            !/*gd        274*/
         an=to_degs*acos(cc)                                                    !/*gd        275*/
         if(yt.lt.zero)an=deg_360-an                                            !/*gd        276*/
         if(yt.eq.zero.and.xt.ge.zero)an=zero                                   !/*gd        277*/
         if(yt.eq.zero.and.xt.lt.zero)an=deg_180                                !/*gd        278*/
      else                                                                      !/*gd        279*/
         an=zero                                                                !/*gd        280*/
         i3=1                                                                   !/*gd        281*/
         go to 93                                                               !/*gd        282*/
      endif                                                                     !/*gd        283*/
!                                                                               !/*gd        284*/
      if((abs(an-gd3(lgd3(ig)+ng3(ig))).lt.ep).or.                              !/*gd        285*/
     & (abs(an).lt.ep)) then                                                    !/*gd        286*/
         i3=ng3(ig)                                                             !/*gd        287*/
         go to 92                                                               !/*gd        288*/
      endif                                                                     !/*gd        289*/
      if(abs(an-gd3(lgd3(ig)+1)).lt.ep) then                                    !/*gd        290*/
         i3=1                                                                   !/*gd        291*/
         go to 92                                                               !/*gd        292*/
      endif                                                                     !/*gd        293*/
      if(an.lt.gd3(lgd3(ig)+1)-ep) then                                         !/*gd        294*/
         i3=1                                                                   !/*gd        295*/
         go to 93                                                               !/*gd        296*/
      endif                                                                     !/*gd        297*/
!                                                                               !/*gd        298*/
      ic=1                                                                      !/*gd        299*/
      ib=ng3(ig)                                                                !/*gd        300*/
   80 if(ib-ic.eq.1) go to 90                                                   !/*gd        301*/
      ih=(ic+ib)/2                                                              !/*gd        302*/
      if(an.lt.gd3(lgd3(ig)+ih)) go to 85                                       !/*gd        303*/
      ic=ih                                                                     !/*gd        304*/
      go to 80                                                                  !/*gd        305*/
   85 ib=ih                                                                     !/*gd        306*/
      go to 80                                                                  !/*gd        307*/
!                                                                               !/*gd        308*/
   90 i3=ic                                                                     !/*gd        309*/
      if(abs(an-gd3(lgd3(ig)+i3)).lt.ep) go to 92                               !/*gd        310*/
      i3=i3+1                                                                   !/*gd        311*/
      go to 93                                                                  !/*gd        312*/
!                                                                               !/*gd        313*/
   92 a=cos(to_rads*(deg_90+an))                                                !/*gd        314*/
      b=cos(to_rads*an)                                                         !/*gd        315*/
      t1=uu*a+vv*b                                                              !/*gd        316*/
      if(t1.gt.zero) then                                                       !/*gd        317*/
         i3=i3+1                                                                !/*gd        318*/
         if(i3.gt.ng3(ig))i3=1                                                  !/*gd        319*/
         j3=1                                                                   !/*gd        320*/
      else                                                                      !/*gd        321*/
         j3=2                                                                   !/*gd        322*/
      endif                                                                     !/*gd        323*/
!                                                                               !/*gd        324*/
!       Score source particles of the correct type or banked particles          !/*gd        325*/
!       that have not had a collision.                                          !/*gd        326*/
   93 if(mm.ne.1.and.mm.ne.8)go to 110                                          !/*gd        327*/
      do 95 i=1,mxgv(ig)                                                        !/*gd        328*/
      if(jp.eq.1.and.ngv(ig,i).ne.ipt) go to 95                                 !/*gd        329*/
      h=one                                                                     !/*gd        330*/
      ta=f_wt                                                                   !/*gd        331*/
      if(jp.eq.0.or.jp.eq.2) then                                               !/*gd        332*/
         if(jp.eq.0.and.ngv(ig,i).ne.4) go to 95                                !/*gd        333*/
         if(jp.eq.2) then                                                       !/*gd        334*/
            if(ngv(ig,i).ne.1.and.ngv(ig,i).ne.mt) go to 95                     !/*gd        335*/
            if(ipt.eq.2.and.kpt(3).ne.0) go to 95                               !/*gd        336*/
         endif                                                                  !/*gd        337*/
         lr=lgd1(ig)+i1                                                         !/*gd        338*/
         lp=lgd2(ig)+i2                                                         !/*gd        339*/
         lh=lgd3(ig)+i3                                                         !/*gd        340*/
         gj=0.                                                                  !/*gd3c        9*/
         if(i1.gt.1)gj=gd1(lr-1)                                                !/*gd3c       10*/
         va=(2.0/3.0)*pie*(gd1(lr)**3-gj**3)                                    !/*gd3c       11*/
         if(ng2(ig).eq.1) then                                                  !/*gd        342*/
            ac=2.0                                                              !/*gd        343*/
         else                                                                   !/*gd        344*/
            if(i2.eq.1) then                                                    !/*gd        345*/
               ac=one-cos(to_rads*gd2(lp))                                      !/*gd        346*/
            else                                                                !/*gd        347*/
               ac=cos(to_rads*gd2(lp-1))-cos(to_rads*gd2(lp))                   !/*gd        348*/
            endif                                                               !/*gd        349*/
         endif                                                                  !/*gd        350*/
!                                                                               !/*gd        351*/
         if(ng3(ig).eq.1) then                                                  !/*gd        352*/
            av=one                                                              !/*gd        353*/
         else                                                                   !/*gd        354*/
            if(i3.eq.1) then                                                    !/*gd        355*/
               av=gd3(lh)/deg_360                                               !/*gd        356*/
            else                                                                !/*gd        357*/
               av=(gd3(lh)-gd3(lh-1))/deg_360                                   !/*gd        358*/
            endif                                                               !/*gd        359*/
          endif                                                                 !/*gd        360*/
         vo=va*ac*av                                                            !/*gd        361*/
         if(vo.le.0.) then                                                      !/*gd        362*/
            call expirx(1,'gd3tal',                                             !/*gd3c       12*/
     &       'Detected a zero or negative mesh volume!')                        !/*gd        364*/
            return                                                              !/*gd        365*/
         endif                                                                  !/*gd        366*/
!                                                                               !/*gd        367*/
         ta=ta/vo                                                               !/*gd        368*/
         h=f_ed                                                                 !/*gd        369*/
         if (m1.ne.0 .and. m1.ne.-1) then                                       !/*gd        370*/
            eg=f_eg                                                             !/*gd        371*/
            if(m3.eq.1)eg=h*ta                                                  !/*gd        372*/
            if(m3.eq.2)eg=h*ta*f_eg                                             !/*gd        373*/
            ta=ta*rfact(m1,m2,m3,rx,eg,nemsh(m1))                               !/*gd        374*/
         elseif(m1.eq.-1) then                                                  !/*gd        375*/
            ta=ta*rx                                                            !/*gd        376*/
         endif                                                                  !/*gd        377*/
      endif                                                                     !/*gd        378*/
!                                                                               !/*gd        379*/
      mx=(i-1)*mxgc(ig)+(i3-1)*(ng1(ig)-1)*ng2(ig)+                             !/*gd        380*/
     & (i2-1)*(ng1(ig)-1)+i1-1                                                  !/*gd        381*/
      gdata(ktgd(ig)+mx)=gdata(ktgd(ig)+mx)+ta*h                                !/*gdxk        1*/
   95 continue                                                                  !/*gd        383*/
!                                                                               !/*gd        384*/
  100 continue                                                                  !/*gd        385*/
      if(mm.eq.1.or.mm.eq.8)return                                              !/*gd        386*/
!                                                                               !/*gd        387*/
!       Track the particle a distance 'da' in all meshs and score               !/*gd        388*/
!       tally data as required.                                                 !/*gd        389*/
!                                                                               !/*gd        390*/
  110 ds=da                                                                     !/*gd        391*/
      x=xx                                                                      !/*gd        392*/
      y=yy                                                                      !/*gd        393*/
      z=zz                                                                      !/*gd        394*/
      if(i1*i2*i3.gt.0)go to 300                                                !/*gd        395*/
!                                                                               !/*gd        396*/
!       The point is not in this mesh.                                          !/*gd        397*/
!       Can it enter this mesh within                                           !/*gd        398*/
!       a distance 'da', and if so where?                                       !/*gd        399*/
      dd=r-da                                                                   !/*gd        400*/
      rm=gd1(lgd1(ig)+ng1(ig))                                                  !/*gd        401*/
      if(dd.gt.rm+ep)go to 420                                                  !/*gd        402*/
!                                                                               !/*gd        403*/
!        Find the distance to the sphere.                                       !/*gd        404*/
      a1=x*uu+y*vv+z*ww                                                         !/*gd        405*/
      b1=x**2+y**2+z**2-rm**2                                                   !/*gd        406*/
      t1=a1**2-b1                                                               !/*gd        407*/
      if(t1.lt.0.) go to 420                                                    !/*gd        408*/
      t2=sqrt(t1)                                                               !/*gd        409*/
      d1=-a1+t2                                                                 !/*gd        410*/
      if(d1.le.0.) go to 420                                                    !/*gd        411*/
      d2=-a1-t2                                                                 !/*gd        412*/
      if(d2.le.0.)d2=huge                                                       !/*gd        413*/
      dr=min(d1,d2)                                                             !/*gd        414*/
      if(dr.ge.ds) go to 420                                                    !/*gd        415*/
!                                                                               !/*gd        416*/
!        Advance the particle by dr.                                            !/*gd        417*/
      x=x+dr*uu                                                                 !/*gd        418*/
      y=y+dr*vv                                                                 !/*gd        419*/
      z=z+dr*ww                                                                 !/*gd        420*/
      ds=ds-dr                                                                  !/*gd        421*/
      if(ds.le.ep) go to 420                                                    !/*gd        422*/
      i1=ng1(ig)                                                                !/*gd        423*/
      j1=2                                                                      !/*gd        424*/
!                                                                               !/*gd        425*/
!        Determine the indices for pa and th where the particle                 !/*gd        426*/
!        will inter the mesh.                                                   !/*gd        427*/
  115 if(ng2(ig).gt.1) then                                                     !/*gd        428*/
         rt=gd1(lgd1(ig)+ng1(ig))                                               !/*gd        429*/
         cc=min(one,max(-one,z/rt))                                             !/*gd        430*/
         an=to_degs*acos(cc)                                                    !/*gd        431*/
      else                                                                      !/*gd        432*/
         i2=1                                                                   !/*gd        433*/
         go to 140                                                              !/*gd        434*/
      endif                                                                     !/*gd        435*/
      if(an.gt.gd2(lgd2(ig)+ng2(ig)-1)) then                                    !/*gd        436*/
         i2=ng2(ig)                                                             !/*gd        437*/
         go to 140                                                              !/*gd        438*/
      endif                                                                     !/*gd        439*/
      if(an.lt.gd2(lgd2(ig)+1)) then                                            !/*gd        440*/
         i2=1                                                                   !/*gd        441*/
         go to 140                                                              !/*gd        442*/
      endif                                                                     !/*gd        443*/
!                                                                               !/*gd        444*/
      ic=1                                                                      !/*gd        445*/
      ib=ng2(ig)-1                                                              !/*gd        446*/
  120 if(ib-ic.eq.1) go to 130                                                  !/*gd        447*/
      ih=(ic+ib)/2                                                              !/*gd        448*/
      if(an.lt.gd2(lgd2(ig)+ih)) go to 125                                      !/*gd        449*/
      ic=ih                                                                     !/*gd        450*/
      go to 120                                                                 !/*gd        451*/
  125 ib=ih                                                                     !/*gd        452*/
      go to 120                                                                 !/*gd        453*/
!                                                                               !/*gd        454*/
  130 i2=ic+1                                                                   !/*gd        455*/
!                                                                               !/*gd        456*/
  140 if(ng3(ig).gt.1) then                                                     !/*gd        457*/
         rj=sqrt(x*x+y*y)                                                       !/*gd        458*/
         if(rj.gt.0.) then                                                      !/*gd        459*/
            cc=min(one,max(-one,x/rj))                                          !/*gd        460*/
         else                                                                   !/*gd        461*/
            cc=one                                                              !/*gd        462*/
         endif                                                                  !/*gd        463*/
         an=to_degs*acos(cc)                                                    !/*gd        464*/
         if(y.lt.zero)an=deg_360-an                                             !/*gd        465*/
         if(y.eq.zero.and.x.ge.zero)an=zero                                     !/*gd        466*/
         if(y.eq.zero.and.x.lt.zero)an=deg_180                                  !/*gd        467*/
      else                                                                      !/*gd        468*/
         an=zero                                                                !/*gd        469*/
         i3=1                                                                   !/*gd        470*/
         go to 300                                                              !/*gd        471*/
      endif                                                                     !/*gd        472*/
      if(an.eq.gd3(lgd3(ig)+ng3(ig))) then                                      !/*gd        473*/
         i3=ng3(ig)                                                             !/*gd        474*/
         go to 170                                                              !/*gd        475*/
      endif                                                                     !/*gd        476*/
      if(an.eq.gd3(lgd3(ig)+1)) then                                            !/*gd        477*/
         i3=1                                                                   !/*gd        478*/
         go to 170                                                              !/*gd        479*/
      endif                                                                     !/*gd        480*/
      if(an.lt.gd3(lgd3(ig)+1)) then                                            !/*gd        481*/
         i3=1                                                                   !/*gd        482*/
         go to 300                                                              !/*gd        483*/
      endif                                                                     !/*gd        484*/
!                                                                               !/*gd        485*/
      ic=1                                                                      !/*gd        486*/
      ib=ng3(ig)                                                                !/*gd        487*/
  150 if(ib-ic.eq.1) go to 160                                                  !/*gd        488*/
      ih=(ic+ib)/2                                                              !/*gd        489*/
      if(an.lt.gd3(lgd3(ig)+ih)) go to 155                                      !/*gd        490*/
      ic=ih                                                                     !/*gd        491*/
      go to 150                                                                 !/*gd        492*/
  155 ib=ih                                                                     !/*gd        493*/
      go to 150                                                                 !/*gd        494*/
!                                                                               !/*gd        495*/
  160 i3=ic                                                                     !/*gd        496*/
      if(an.lt.gd3(lgd3(ig)+1)) go to 300                                       !/*gd        497*/
      if(gd3(lgd3(ig)+i3).eq.an) go to 170                                      !/*gd        498*/
      i3=i3+1                                                                   !/*gd        499*/
      if(i3.gt.ng3(ig))i3=1                                                     !/*gd        500*/
      go to 300                                                                 !/*gd        501*/
!                                                                               !/*gd        502*/
  170 a=cos(to_rads*(deg_90+an))                                                !/*gd        503*/
      b=cos(to_rads*an)                                                         !/*gd        504*/
      t1=uu*a+vv*b                                                              !/*gd        505*/
      if(t1.gt.zero) then                                                       !/*gd        506*/
         j3=1                                                                   !/*gd        507*/
         i3=i3+1                                                                !/*gd        508*/
         if(i3.gt.ng3(ig))i3=1                                                  !/*gd        509*/
      else                                                                      !/*gd        510*/
         j3=2                                                                   !/*gd        511*/
      endif                                                                     !/*gd        512*/
!                                                                               !/*gd        513*/
!       Track the particle in this mesh until the distance 'ds'                 !/*gd        514*/
!       is exhausted or the particle leaves this mesh.                          !/*gd        515*/
  300 lr=lgd1(ig)+i1                                                            !/*gd        516*/
      lp=lgd2(ig)+i2                                                            !/*gd        517*/
      lh=lgd3(ig)+i3                                                            !/*gd        518*/
!                                                                               !/*gd        519*/
!        Find the distances to the next sphereical surface.                     !/*gd        520*/
      r=sqrt(x*x+y*y+z*z)                                                       !/*gd        521*/
      s1=huge                                                                   !/*gd        522*/
      s2=huge                                                                   !/*gd        523*/
      s3=huge                                                                   !/*gd        524*/
      c1=huge                                                                   !/*gd        525*/
      c2=huge                                                                   !/*gd        526*/
      p1=huge                                                                   !/*gd        527*/
      p2=huge                                                                   !/*gd        528*/
      a1=x*uu+y*vv+z*ww                                                         !/*gd        529*/
      if((gd1(lgd1(ig)+1).gt.0..and.i1.gt.1).or.                                !/*gd        530*/
     &   (gd1(lgd1(ig)+1).eq.0..and.i1.gt.2)) then                              !/*gd        531*/
         an=(x/r)*uu+(y/r)*vv+(z/r)*ww                                          !/*gd        532*/
         if(an.ge.0)go to 302                                                   !/*gd        533*/
         b1=x**2+y**2+z**2-gd1(lr-1)**2                                         !/*gd        534*/
         t1=a1**2-b1                                                            !/*gd        535*/
         if(t1.lt.0.)go to 302                                                  !/*gd        536*/
         t2=sqrt(t1)                                                            !/*gd        537*/
         s1=-a1+t2                                                              !/*gd        538*/
         if(s1.le.0.) then                                                      !/*gd        539*/
            s1=huge                                                             !/*gd        540*/
            go to 302                                                           !/*gd        541*/
         endif                                                                  !/*gd        542*/
         s2=-a1-t2                                                              !/*gd        543*/
         if(s2.le.0.)s2=huge                                                    !/*gd        544*/
      endif                                                                     !/*gd        545*/
!                                                                               !/*gd        546*/
  302 b1=x**2+y**2+z**2-gd1(lr)**2                                              !/*gd        547*/
      t1=a1**2-b1                                                               !/*gd        548*/
      if(t1.lt.0.)go to 303                                                     !/*gd        549*/
      t2=sqrt(t1)                                                               !/*gd        550*/
      s3=-a1+t2                                                                 !/*gd        551*/
      if(s3.le.0.)s3=huge                                                       !/*gd        552*/
!                                                                               !/*gd        553*/
!       Find the distance to the next conical surface.                          !/*gd        554*/
  303 if(i1.eq.1)go to 340                                                      !/*gd        555*/
      if(ng2(ig).eq.1)go to 309                                                 !/*gd        556*/
      do 308 i=1,2                                                              !/*gd        557*/
      lc=lp+i-2                                                                 !/*gd        558*/
      if((i.eq.1.and.i2.eq.1).or.(i.eq.2.and.i2.eq.ng2(ig)))                    !/*gd        559*/
     & go to 308                                                                !/*gd        560*/
      jf=0                                                                      !/*gd        561*/
      if(i.eq.j2) jf=1                                                          !/*gd        562*/
      d1=-1.                                                                    !/*gd        563*/
      d2=-1.                                                                    !/*gd        564*/
!                                                                               !/*gd        565*/
      if(gd2(lc).eq.90.) then                                                   !/*gd        566*/
         if(jf.eq.0.and.ww.ne.0.) then                                          !/*gd        567*/
            d1=-z/ww                                                            !/*gd        568*/
            go to 304                                                           !/*gd        569*/
         else                                                                   !/*gd        570*/
            go to 308                                                           !/*gd        571*/
         endif                                                                  !/*gd        572*/
      endif                                                                     !/*gd        573*/
!                                                                               !/*gd        574*/
      t9=1.e10                                                                  !/*gd        575*/
      an=(tan(gd2(lc)*to_rads))**2                                              !/*gd        576*/
      t4=uu**2+vv**2-an*ww**2                                                   !/*gd        577*/
      if(abs(t4).gt.1.e-10)t9=1./t4                                             !/*gd        578*/
      ux=ww                                                                     !/*gd        579*/
      tk=1                                                                      !/*gd        580*/
      if(gd2(lc).gt.90.)tk=-1                                                   !/*gd        581*/
      a1=(x*uu+y*vv-z*ww*an)*t9                                                 !/*gd        582*/
      b1=(x**2+y**2-an*z**2)*t9                                                 !/*gd        583*/
      if(t9.ne.1.e10)go to 305                                                  !/*gd        584*/
!                                                                               !/*gd        585*/
!         Calculate a single distance. (rare case)                              !/*gd        586*/
      if(abs(a1).le.1.e-10.or.jf.eq.1)go to 308                                 !/*gd        587*/
      d1=-.5*b1/a1                                                              !/*gd        588*/
      if(d1.le.0.) go to 308                                                    !/*gd        589*/
      go to 306                                                                 !/*gd        590*/
!                                                                               !/*gd        591*/
!          calculate a pair of distances.                                       !/*gd        592*/
  305 t1=a1**2-b1                                                               !/*gd        593*/
      if(t1.lt.0.)go to 308                                                     !/*gd        594*/
      if(jf.eq.1) then                                                          !/*gd        595*/
         d1=-2.*a1                                                              !/*gd        596*/
         if(d1.le.0.)go to 308                                                  !/*gd        597*/
         go to 306                                                              !/*gd        598*/
      else                                                                      !/*gd        599*/
         t2=sqrt(t1)                                                            !/*gd        600*/
         d1=-a1+t2                                                              !/*gd        601*/
         if(d1.le.0.)go to 308                                                  !/*gd        602*/
         d2=-a1-t2                                                              !/*gd        603*/
      endif                                                                     !/*gd        604*/
!                                                                               !/*gd        605*/
!        reject the distance to the wrong sheet of a cone.                      !/*gd        606*/
  306 if(tk*(z+d1*ux).lt.0.)d1=-1                                               !/*gd        607*/
      if(tk*(z+d2*ux).lt.0.)d2=-1                                               !/*gd        608*/
      d1=max(d1,d2)                                                             !/*gd        609*/
      if(d1.le.0.)go to 308                                                     !/*gd        610*/
      if(d2.gt.0.)d1=d2                                                         !/*gd        611*/
!                                                                               !/*gd        612*/
  304 if(i.eq.1) then                                                           !/*gd        613*/
         if(d1.gt.0.) then                                                      !/*gd        614*/
            c1=d1                                                               !/*gd        615*/
         endif                                                                  !/*gd        616*/
      else                                                                      !/*gd        617*/
         if(d1.gt.0.) then                                                      !/*gd        618*/
            c2=d1                                                               !/*gd        619*/
         endif                                                                  !/*gd        620*/
      endif                                                                     !/*gd        621*/
  308 continue                                                                  !/*gd        622*/
!                                                                               !/*gd        623*/
!       Calculate the distance to the next plane.                               !/*gd        624*/
  309 p1=huge                                                                   !/*gd        625*/
      p2=huge                                                                   !/*gd        626*/
      if(ng3(ig).eq.1)go to 340                                                 !/*gd        627*/
      if(j3.eq.2)go to 320                                                      !/*gd        628*/
      an=gd3(lh)                                                                !/*gd        629*/
      kk=0                                                                      !/*gd        630*/
      if(an.eq.90.) then                                                        !/*gd        631*/
         kk=1                                                                   !/*gd        632*/
         a=1.                                                                   !/*gd        633*/
         b=0.                                                                   !/*gd        634*/
      elseif(an.eq.180.) then                                                   !/*gd        635*/
         kk=1                                                                   !/*gd        636*/
         a=0.                                                                   !/*gd        637*/
         b=-1.                                                                  !/*gd        638*/
      elseif(an.eq.270.) then                                                   !/*gd        639*/
         kk=1                                                                   !/*gd        640*/
         a=-1.                                                                  !/*gd        641*/
         b=0.                                                                   !/*gd        642*/
      elseif(an.eq.360.) then                                                   !/*gd        643*/
         kk=1                                                                   !/*gd        644*/
         a=0.                                                                   !/*gd        645*/
         b=1.                                                                   !/*gd        646*/
      endif                                                                     !/*gd        647*/
      if(kk.eq.0) then                                                          !/*gd        648*/
         a=cos(to_rads*(deg_90+an))                                             !/*gd        649*/
         b=cos(to_rads*an)                                                      !/*gd        650*/
      endif                                                                     !/*gd        651*/
      t1=uu*a+vv*b                                                              !/*gd        652*/
      if(t1.eq.zero)go to 320                                                   !/*gd        653*/
      d=(-x*a-y*b)/t1                                                           !/*gd        654*/
      if(d.le.zero)go to 320                                                    !/*gd        655*/
      xt=x+d*uu                                                                 !/*gd        656*/
      yt=y+d*vv                                                                 !/*gd        657*/
      zt=z+d*ww                                                                 !/*gd        658*/
      rt=sqrt(xt*xt+yt*yt+zt*zt)                                                !/*gd        659*/
      if(rt.lt.gd1(lgd1(ig)+1))go to 320                                        !/*gd        660*/
      if(xt.lt.0..and.(an.eq.0..or.an.eq.360.))go to 320                        !/*gd        661*/
      if(xt.ge.0..and.an.eq.180.)go to 320                                      !/*gd        662*/
      if(yt.lt.0..and.an.eq.90.)go to 320                                       !/*gd        663*/
      if(yt.ge.0..and.an.eq.270.)go to 320                                      !/*gd        664*/
      if(yt.gt.0..and.an.gt.0..and.an.le.180.)then                              !/*gd        665*/
         if(abs(xt).lt.ep.and.an.eq.90.)go to 310                               !/*gd        666*/
         if(xt.gt.0..and.an.le.90.)go to 310                                    !/*gd        667*/
         if(xt.lt.0..and.an.gt.90.)go to 310                                    !/*gd        668*/
         go to 320                                                              !/*gd        669*/
      endif                                                                     !/*gd        670*/
      if(yt.lt.0..and.an.gt.180..and.an.le.360.)then                            !/*gd        671*/
         if(abs(xt).lt.ep.and.an.eq.270.)go to 310                              !/*gd        672*/
         if(xt.gt.0..and.an.gt.270.)go to 310                                   !/*gd        673*/
         if(xt.lt.0..and.an.le.270.)go to 310                                   !/*gd        674*/
         go to 320                                                              !/*gd        675*/
      endif                                                                     !/*gd        676*/
  310 p2=d                                                                      !/*gd        677*/
!                                                                               !/*gd        678*/
  320 if(j3.eq.1)go to 340                                                      !/*gd        679*/
      an=0.                                                                     !/*gd3c       13*/
      if(i3.gt.1)an=gd3(lh-1)                                                   !/*gd3c       14*/
      if(i3.eq.1)an=zero                                                        !/*gd        681*/
      kk=0                                                                      !/*gd        682*/
      if(an.eq.0.) then                                                         !/*gd        683*/
         kk=1                                                                   !/*gd        684*/
         a=0.                                                                   !/*gd        685*/
         b=1                                                                    !/*gd        686*/
      elseif(an.eq.90.) then                                                    !/*gd        687*/
         kk=1                                                                   !/*gd        688*/
         a=1.                                                                   !/*gd        689*/
         b=0.                                                                   !/*gd        690*/
      elseif(an.eq.180.) then                                                   !/*gd        691*/
         kk=1                                                                   !/*gd        692*/
         a=0.                                                                   !/*gd        693*/
         b=-1.                                                                  !/*gd        694*/
      elseif(an.eq.270.) then                                                   !/*gd        695*/
         kk=1                                                                   !/*gd        696*/
         a=-1.                                                                  !/*gd        697*/
         b=0.                                                                   !/*gd        698*/
      endif                                                                     !/*gd        699*/
      if(kk.eq.0) then                                                          !/*gd        700*/
         a=cos(to_rads*(deg_90+an))                                             !/*gd        701*/
         b=cos(to_rads*an)                                                      !/*gd        702*/
      endif                                                                     !/*gd        703*/
      t1=uu*a+vv*b                                                              !/*gd        704*/
      if(t1.eq.zero)go to 340                                                   !/*gd        705*/
      d=(-x*a-y*b)/t1                                                           !/*gd        706*/
      if(d.le.zero)go to 340                                                    !/*gd        707*/
      xt=x+d*uu                                                                 !/*gd        708*/
      yt=y+d*vv                                                                 !/*gd        709*/
      zt=z+d*ww                                                                 !/*gd        710*/
      rt=sqrt(xt*xt+yt*yt+zt*zt)                                                !/*gd        711*/
      if(rt.lt.gd1(lgd1(ig)+1))go to 340                                        !/*gd        712*/
      if(i3.eq.1)ax=zero                                                        !/*gd        713*/
      if(xt.lt.0..and.(an.eq.0..or.an.eq.360.))go to 340                        !/*gd        714*/
      if(xt.ge.0..and.an.eq.180.)go to 340                                      !/*gd        715*/
      if(yt.lt.0..and.an.eq.90.)go to 340                                       !/*gd        716*/
      if(yt.ge.0..and.an.eq.270.)go to 340                                      !/*gd        717*/
      if(yt.gt.0..and.an.gt.0..and.an.le.180.)then                              !/*gd        718*/
         if(abs(xt).lt.ep.and.an.eq.90.)go to 330                               !/*gd        719*/
         if(xt.gt.0..and.an.le.90.)go to 330                                    !/*gd        720*/
         if(xt.lt.0..and.an.gt.90.)go to 330                                    !/*gd        721*/
         go to 340                                                              !/*gd        722*/
      endif                                                                     !/*gd        723*/
      if(yt.lt.0..and.an.gt.180..and.an.le.360.)then                            !/*gd        724*/
         if(abs(xt).lt.ep.and.an.eq.270.)go to 330                              !/*gd        725*/
         if(xt.gt.0..and.an.gt.270.)go to 330                                   !/*gd        726*/
         if(xt.lt.0..and.an.le.270.)go to 330                                   !/*gd        727*/
         go to 340                                                              !/*gd        728*/
      endif                                                                     !/*gd        729*/
  330 p1=d                                                                      !/*gd        730*/
!                                                                               !/*gd        731*/
  340 d=min(s1,s2,s3,c1,c2,p1,p2,ds)                                            !/*gd        732*/
      dm=d                                                                      !/*gd        733*/
      if(d.lt.p_1m12)dm=p_1m12                                                  !/*gd        734*/
      if(d.le.p_1m12.or.i1.le.1)go to 401                                       !/*gd        735*/
!                                                                               !/*gd        736*/
      va=(2.0/3.0)*pie*(gd1(lr)**3-gd1(lr-1)**3)                                !/*gd        737*/
      if(ng2(ig).eq.1) then                                                     !/*gd        738*/
         ac=2.0                                                                 !/*gd        739*/
      else                                                                      !/*gd        740*/
         if(i2.eq.1) then                                                       !/*gd        741*/
            ac=one-cos(to_rads*gd2(lp))                                         !/*gd        742*/
         else                                                                   !/*gd        743*/
            ac=cos(to_rads*gd2(lp-1))-cos(to_rads*gd2(lp))                      !/*gd        744*/
         endif                                                                  !/*gd        745*/
      endif                                                                     !/*gd        746*/
!                                                                               !/*gd        747*/
      if(ng3(ig).eq.1) then                                                     !/*gd        748*/
         av=one                                                                 !/*gd        749*/
      else                                                                      !/*gd        750*/
         if(i3.eq.1) then                                                       !/*gd        751*/
            av=gd3(lh)/deg_360                                                  !/*gd        752*/
         else                                                                   !/*gd        753*/
            av=(gd3(lh)-gd3(lh-1))/deg_360                                      !/*gd        754*/
         endif                                                                  !/*gd        755*/
      endif                                                                     !/*gd        756*/
      vo=va*ac*av                                                               !/*gd        757*/
      if(vo.le.zero)call expirx(1,'gd3tal',                                     !/*gd3c       15*/
     & 'Detected a zero or negative mesh volume!')                              !/*gd        759*/
!                                                                               !/*gd        760*/
  343 jj=0                                                                      !/*gd        761*/
      fc=zero                                                                   !/*gd        762*/
      if(da.gt.zero)fc=d/da                                                     !/*gd        763*/
      do 400 i=1,mxgv(ig)                                                       !/*gd        764*/
      h=one                                                                     !/*gd        765*/
      if(jp.eq.1) then                                                          !/*gd        766*/
         if(ipt.ne.ngv(ig,i))go to 400                                          !/*gd        767*/
         ta=f_wt                                                                !/*gd        768*/
         go to 395                                                              !/*gd        769*/
      endif                                                                     !/*gd        770*/
      if(jp.eq.2) then                                                          !/*gd        771*/
         if(ngv(ig,i).ne.1.and.ngv(ig,i).ne.mt)go to 400                        !/*gd        772*/
         go to 355                                                              !/*gd        773*/
      endif                                                                     !/*gd        774*/
      if(jp.ge.3)go to 350                                                      !/*gd        775*/
      go to (350,345,354,355,352)ngv(ig,i)                                      !/*gd        776*/
!                                                                               !/*gd        777*/
  345 ta=one                                                                    !/*gd        778*/
      go to 393                                                                 !/*gd        779*/
!                                                                               !/*gd        780*/
  350 ta=d*f_wt/vo                                                              !/*gd        781*/
      go to 393                                                                 !/*gd        782*/
!                                                                               !/*gd        783*/
  352 jj=jj+1                                                                   !/*gd        784*/
      n1=mdos(1,jj,ig)                                                          !/*gd        785*/
      if(n1.eq.0)n1=10                                                          !/*gd        786*/
      n2=mdos(2,jj,ig)                                                          !/*gd        787*/
      if(n2.eq.0)n2=1                                                           !/*gd        788*/
      n3=mdos(3,jj,ig)                                                          !/*gd        789*/
      if(n3.eq.0)n3=1                                                           !/*gd        790*/
      rm=rdos(jj,ig)                                                            !/*gd        791*/
      if(rm.eq.zero)rm=one                                                      !/*gd        792*/
      if(n1.ge.10) then                                                         !/*gd        793*/
         ft=dfact(ipt,n1,f_eg,n2,n3,rm)                                         !/*gd        794*/
      else                                                                      !/*gd        795*/
         ft=rfact(n1,n2,n3,rm,f_eg,nemsh(n1))                                   !/*gd        796*/
      endif                                                                     !/*gd        797*/
      ta=ft*d*f_wt/vo                                                           !/*gd        798*/
      go to 395                                                                 !/*gd        799*/
!                                                                               !/*gd        800*/
  354 ta=d*f_wt                                                                 !/*gd        801*/
      go to 393                                                                 !/*gd        802*/
!                                                                               !/*gd        803*/
  355 ta=f_wt/vo                                                                !/*gd        804*/
      if(ipt.eq.1.and.erg.gt.cut_n)go to 400                                    !/*gd        805*/
      if(mk.eq.0.or.(jgp.lt.0.and.ipt.eq.2))go to 400                           !/*gd        806*/
      if(jp.eq.2.and.ipt.eq.2.and.                                              !/*gd        807*/
     & (jgp.lt.0.or.kpt(3).ne.0))go to 400                                      !/*gd        808*/
!                                                                               !/*gd        809*/
!       Particle energy deposition.                                             !/*gd        810*/
  390 h=fc*f_ed                                                                 !/*gd        811*/
      f_eg=f_eg-h                                                               !/*gd        812*/
!                                                                               !/*gd        813*/
!       Energy-dependent multiplier function.                                   !/*gd        814*/
  393 if (m1.ne.0 .and. m1.ne.-1) then                                          !/*gd        815*/
         eg=f_eg                                                                !/*gd        816*/
         if(m3.eq.1)eg=h*ta                                                     !/*gd        817*/
         if(m3.eq.2)eg=h*ta*f_eg                                                !/*gd        818*/
         ta=ta*rfact(m1,m2,m3,rx,eg,nemsh(m1))                                  !/*gd        819*/
      elseif(m1.eq.-1) then                                                     !/*gd        820*/
         ta=ta*rx                                                               !/*gd        821*/
      endif                                                                     !/*gd        822*/
!                                                                               !/*gd        823*/
  395 if(jp.eq.0) then                                                          !/*gd        824*/
         il=-1                                                                  !/*gd        825*/
         do 396 k=1,ntal+mshfm                                                  !/*gd        826*/
         if(nugd(ig).eq.jptal(ljpt+1,k)) then                                   !/*gd        827*/
            il=k                                                                !/*gd        828*/
         endif                                                                  !/*gd        829*/
  396    continue                                                               !/*gd        830*/
         if(il.lt.0)go to 397                                                   !/*gd        831*/
!                                                                               !/*gd        832*/
         if(iptal(lipt+5,2,il).ne.0) then                                       !/*gd        833*/
            ital=il                                                             !/*gd        834*/
            if(iptal(lipt+5,1,il).ne.0)call wtmult(ta,1)                        !/*gd        835*/
            ta=ta*tds(iptal(lipt+5,2,il)+1)                                     !/*gd        836*/
            if(tds(iptal(lipt+5,2,il)+1).lt.0.)ta=-ta*rho(lrho+icl)             !/*gd        837*/
         endif                                                                  !/*gd        838*/
!                                                                               !/*gd        839*/
         if(iptal(lipt+3,2,il).ne.0) then                                       !/*gd        840*/
            ital=il                                                             !/*gd        841*/
            ta=dosef(ta)                                                        !/*gd        842*/
         endif                                                                  !/*gd        843*/
      endif                                                                     !/*gd        844*/
!                                                                               !/*gd        845*/
  397 mx=(i-1)*mxgc(ig)+(i3-1)*(ng1(ig)-1)*ng2(ig)+                             !/*gd        846*/
     & (i2-1)*(ng1(ig)-1)+i1-1                                                  !/*gd        847*/
      gdata(ktgd(ig)+mx)=gdata(ktgd(ig)+mx)+ta*h                                !/*gdxk        2*/
!                                                                               !/*gd        849*/
  400 continue                                                                  !/*gd        850*/
!                                                                               !/*gd        851*/
!       Move a distance 'd', advance the indicies if required,                  !/*gd        852*/
!       and continue tracking until 'ds'=0 or particle exits mesh.              !/*gd        853*/
  401 ds=ds-dm                                                                  !/*gd        854*/
      if(ds.le.ep)go to 420                                                     !/*gd        855*/
      if(d.le.p_1m12) then                                                      !/*gd        856*/
         j1=0                                                                   !/*gd        857*/
         j2=0                                                                   !/*gd        858*/
         j3=0                                                                   !/*gd        859*/
         go to 402                                                              !/*gd        860*/
      endif                                                                     !/*gd        861*/
      j1=0                                                                      !/*gd        862*/
      j2=0                                                                      !/*gd        863*/
      j3=0                                                                      !/*gd        864*/
      if(d.eq.s1.or.d.eq.s2) then                                               !/*gd        865*/
         j1=2                                                                   !/*gd        866*/
         i1=i1-1                                                                !/*gd        867*/
      elseif(d.eq.s3) then                                                      !/*gd        868*/
         j1=1                                                                   !/*gd        869*/
         i1=i1+1                                                                !/*gd        870*/
         if(i1.gt.ng1(ig))go to 420                                             !/*gd        871*/
      elseif(d.eq.c1) then                                                      !/*gd        872*/
         j2=2                                                                   !/*gd        873*/
         i2=i2-1                                                                !/*gd        874*/
      elseif(d.eq.c2) then                                                      !/*gd        875*/
         j2=1                                                                   !/*gd        876*/
         i2=i2+1                                                                !/*gd        877*/
       elseif(d.eq.p1) then                                                     !/*gd        878*/
         j3=2                                                                   !/*gd        879*/
         i3=i3-1                                                                !/*gd        880*/
         if(i3.lt.1)i3=ng3(ig)                                                  !/*gd        881*/
      elseif(d.eq.p2) then                                                      !/*gd        882*/
         j3=1                                                                   !/*gd        883*/
         i3=i3+1                                                                !/*gd        884*/
         if(i3.gt.ng3(ig))i3=1                                                  !/*gd        885*/
      endif                                                                     !/*gd        886*/
!                                                                               !/*gd        887*/
  402 x=x+dm*uu                                                                 !/*gd        888*/
      y=y+dm*vv                                                                 !/*gd        889*/
      z=z+dm*ww                                                                 !/*gd        890*/
      if(i1.eq.2.and.j1.eq.1.and.gd1(lgd1(ig)+1).gt.0.) then                    !/*gd        891*/
         r=sqrt(x*x+y*y+z*z)                                                    !/*gd        892*/
         if(ng2(ig).gt.1) then                                                  !/*gd        893*/
            cc=min(one,max(-one,z/r))                                           !/*gd        894*/
            an=to_degs*acos(cc)                                                 !/*gd        895*/
         else                                                                   !/*gd        896*/
            i2=1                                                                !/*gd        897*/
            go to 406                                                           !/*gd        898*/
         endif                                                                  !/*gd        899*/
         if(an.eq.gd2(lgd2(ig)+ng2(ig))) then                                   !/*gd        900*/
            i2=ng2(ig)                                                          !/*gd        901*/
            j2=2                                                                !/*gd        902*/
            go to 406                                                           !/*gd        903*/
         endif                                                                  !/*gd        904*/
         if(an.eq.0.) then                                                      !/*gd        905*/
            i2=1                                                                !/*gd        906*/
            j2=1                                                                !/*gd        907*/
            go to 406                                                           !/*gd        908*/
         endif                                                                  !/*gd        909*/
         if(an.lt.gd2(lgd2(ig)+1)) then                                         !/*gd        910*/
            i2=1                                                                !/*gd        911*/
            go to 406                                                           !/*gd        912*/
         endif                                                                  !/*gd        913*/
!                                                                               !/*gd        914*/
         ic=1                                                                   !/*gd        915*/
         ib=ng2(ig)                                                             !/*gd        916*/
  403    if(ib-ic.eq.1) go to 405                                               !/*gd        917*/
         ih=(ic+ib)/2                                                           !/*gd        918*/
         if(an.lt.gd2(lgd2(ig)+ih)) go to 404                                   !/*gd        919*/
         ic=ih                                                                  !/*gd        920*/
         go to 403                                                              !/*gd        921*/
  404    ib=ih                                                                  !/*gd        922*/
         go to 403                                                              !/*gd        923*/
!                                                                               !/*gd        924*/
  405    i2=ic+1                                                                !/*gd        925*/
         if(gd2(lgd2(ig)+i2-1).eq.zz/r) then                                    !/*gd        926*/
            if(ww.le.an) then                                                   !/*gd        927*/
               i2=i2-1                                                          !/*gd        928*/
            endif                                                               !/*gd        929*/
         endif                                                                  !/*gd        930*/
!                                                                               !/*gd        931*/
  406    if(ng3(ig).gt.1) then                                                  !/*gd        932*/
            rj=sqrt(x*x+y*y)                                                    !/*gd        933*/
            if(rj.gt.0.) then                                                   !/*gd        934*/
               cc=min(one,max(-one,x/rj))                                       !/*gd        935*/
            else                                                                !/*gd        936*/
               cc=one                                                           !/*gd        937*/
            endif                                                               !/*gd        938*/
            an=to_degs*acos(cc)                                                 !/*gd        939*/
            if(y.lt.zero)an=deg_360-an                                          !/*gd        940*/
            if(y.eq.zero.and.x.ge.zero)an=zero                                  !/*gd        941*/
            if(y.eq.zero.and.x.lt.zero)an=deg_180                               !/*gd        942*/
         else                                                                   !/*gd        943*/
            an=zero                                                             !/*gd        944*/
            i3=1                                                                !/*gd        945*/
            go to 300                                                           !/*gd        946*/
         endif                                                                  !/*gd        947*/
!                                                                               !/*gd        948*/
         if(abs(an-gd3(lgd3(ig)+ng3(ig))).lt.ep) then                           !/*gd        949*/
            i3=ng3(ig)                                                          !/*gd        950*/
            go to 410                                                           !/*gd        951*/
         endif                                                                  !/*gd        952*/
         if(abs(an-gd3(lgd3(ig)+1)).lt.ep) then                                 !/*gd        953*/
            i3=1                                                                !/*gd        954*/
            go to 410                                                           !/*gd        955*/
         endif                                                                  !/*gd        956*/
         if(an+ep.lt.gd3(lgd3(ig)+1)) then                                      !/*gd        957*/
            i3=1                                                                !/*gd        958*/
            go to 300                                                           !/*gd        959*/
         endif                                                                  !/*gd        960*/
!                                                                               !/*gd        961*/
         ic=1                                                                   !/*gd        962*/
         ib=ng3(ig)                                                             !/*gd        963*/
  407    if(ib-ic.eq.1) go to 409                                               !/*gd        964*/
         ih=(ic+ib)/2                                                           !/*gd        965*/
         if(an.lt.gd3(lgd3(ig)+ih)) go to 408                                   !/*gd        966*/
         ic=ih                                                                  !/*gd        967*/
         go to 407                                                              !/*gd        968*/
  408    ib=ih                                                                  !/*gd        969*/
         go to 407                                                              !/*gd        970*/
!                                                                               !/*gd        971*/
  409    i3=ic                                                                  !/*gd        972*/
         if(abs(an-gd3(lgd3(ig)+i3)).lt.ep) go to 410                           !/*gd        973*/
         i3=i3+1                                                                !/*gd        974*/
         go to 300                                                              !/*gd        975*/
!                                                                               !/*gd        976*/
  410    a=cos(to_rads*(deg_90+an))                                             !/*gd        977*/
         b=cos(to_rads*an)                                                      !/*gd        978*/
         t1=uu*a+vv*b                                                           !/*gd        979*/
         if(t1.gt.zero) then                                                    !/*gd        980*/
            j3=1                                                                !/*gd        981*/
            i3=i3+1                                                             !/*gd        982*/
            if(i3.gt.ng3(ig))i3=1                                               !/*gd        983*/
         else                                                                   !/*gd        984*/
            j3=2                                                                !/*gd        985*/
         endif                                                                  !/*gd        986*/
      endif                                                                     !/*gd        987*/
      go to 300                                                                 !/*gd        988*/
!                                                                               !/*gd        989*/
  420 continue                                                                  !/*gd        990*/
!                                                                               !/*gd        991*/
#endif                                                                          /*!gd        992*/
      return                                                                    !/*gd        993*/
      end                                                                       !/*gd        994*/
