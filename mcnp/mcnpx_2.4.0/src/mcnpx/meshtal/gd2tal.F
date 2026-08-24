!     $Id: gd2tal.F,v 1.11 2002/06/14 17:05:54 jxh Exp $                        !/*gd          1*/
      subroutine gd2tal(mm,da,ig,zd,jz)                                         !/*gd          2*/
#ifdef MESHTAL                                                                  /*!gd          3*/
!                                                                               !/*gd          4*/
!       This subroutine is designed to score a volume tally like                !/*gd          5*/
!       quantity in a three dimensional R, Z, Theta mesh for later              !/*gd          6*/
!       post processing with any number of graphics packages.                   !/*gd          7*/
!                                                                               !/*gd          9*/
!       mm=1   Locate source or banked particle relative to mesh                !/*gd         10*/
!              and score if this is a point mesh.                               !/*gd         11*/
!       mm=2   Track particle a distance 'da' and score tally info.             !/*gd         12*/
!       mm=5   Score the track of a point detector contribution.                !/*gd         13*/
!       mm=6   Score the track of a dxtran contribution.                        !/*gd         14*/
!       mm=8   Score energy deposite locally.                                   !/*gd         15*/
!                                                                               !/*gd         16*/
      use global_data                                                           !/*gd2c        1*/
!                                                                               !/*gd2c        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gd2c        3*/
      implicit integer(kindi) (i-n)                                             !/*gd2c        4*/
!                                                                               !/*gd2c        5*/
!                                                                               !/*gd         18*/
      parameter (two_pi=6.2831853071796d0)                                      !/*gd         19*/
      parameter (deg_90=90.d0,deg_180=180.d0,deg_360=360.d0)                    !/*gd         20*/
      parameter (to_degs=deg_360/two_pi,to_rads=two_pi/deg_360)                 !/*gd         21*/
      parameter (ep=1.d-15)                                                     !/*gd         22*/
!                                                                               !/*gd         23*/
      common /meshtal/ ergl(maxg),ergh(maxg),rdos(5,maxg),                      !/*gd         24*/
     & rmult(maxg),mdos(3,5,maxg),mmult(3,maxg),nemsh(maxg),                    !/*gd         25*/
     & mshtr(maxg),mstyp(maxg),mshpt(maxg),ng1(maxg),ng2(maxg),                 !/*gd         26*/
     & ng3(maxg),mxgt(maxg),ngv(maxg,maxgv),mxgc(maxg),mxgv(maxg)               !/*gd         27*/
!                                                                               !/*gd         28*/
      dimension zd(10),jz(5)                                                    !/*gd         29*/
      dimension tr(12)                                                          !/*gd         30*/
!                                                                               !/*gd         31*/
!       jz(1) = material number                                                 !/*gd         32*/
!       jz(2) = mesh tally point source flag.                                   !/*gd         33*/
!               0 = Not a new particle                                          !/*gd         34*/
!               1 = Samples from the source distribution.                       !/*gd         35*/
!               2 = Secondary particle from collision                           !/*gd         36*/
!               3 = Particle from surface source file.                          !/*gd         37*/
!       jz(3 - 4) currently not used.                                           !/*gd         38*/
!       jz(5) = energy deposition type.                                         !/*gd         39*/
!       zd(1 - 6) = x,y,z,u,v,w                                                 !/*gd         40*/
!       zd(7) = track weight                                                    !/*gd         41*/
!       zd(8) = energy at start of track.                                       !/*gd         42*/
!       zd(9) = energy lost along track of at point.                            !/*gd         43*/
!       zd(10) currently not used.                                              !/*gd         44*/
!                                                                               !/*gd         45*/
      d=zero                                                                    !/*gd         46*/
!                                                                               !/*gd         47*/
!       ip = particle type.                                                     !/*gd         48*/
!       jp = mesh type.                                                         !/*gd         49*/
!                                                                               !/*gd         50*/
      ip=mshpt(ig)                                                              !/*gd         51*/
      jp=mod(mstyp(ig),10)                                                      !/*gd         52*/
!                                                                               !/*gd         53*/
      f_wt=zd(7)                                                                !/*gd         54*/
      f_eg=zd(8)                                                                !/*gd         55*/
      f_ed=zd(9)                                                                !/*gd         56*/
      mk=jz(1)                                                                  !/*gd         57*/
      mt=jz(5)                                                                  !/*gd         58*/
!                                                                               !/*gd         59*/
      m1=mmult(1,ig)                                                            !/*gd         60*/
      if(m1.ne.0) then                                                          !/*gd         61*/
         m2=mmult(2,ig)                                                         !/*gd         62*/
         if(m2.eq.0)m2=1                                                        !/*gd         63*/
         m3=mmult(3,ig)                                                         !/*gd         64*/
         rx=rmult(ig)                                                           !/*gd         65*/
         if(rx.eq.zero)rx=one                                                   !/*gd         66*/
      endif                                                                     !/*gd         67*/
!                                                                               !/*gd         68*/
!       See if something needs to be done for this track.                       !/*gd         69*/
!                                                                               !/*gd         70*/
      if(f_eg.gt.ergh(ig).or.f_eg.le.ergl(ig))return                            !/*gd         71*/
      if((jp.eq.0.or.jp.ge.3).and.ip.ne.ipt)return                              !/*gd         72*/
      if(mm.eq.1.and.jp.eq.1)go to 10                                           !/*gd         73*/
      if((mm.eq.2.or.mm.eq.8).and.(jp.eq.0.or.jp.eq.2))go to 10                 !/*gd         74*/
      if((mm.eq.5.and.jp.eq.3).or.(mm.eq.6.and.jp.eq.4))go to 10                !/*gd         75*/
      return                                                                    !/*gd         76*/
!                                                                               !/*gd         77*/
!       Transform to coordinate system of mesh.                                 !/*gd         78*/
   10 m=mshtr(ig)                                                               !/*gd         79*/
!                                                                               !/*gd         80*/
      if(m.eq.0)then                                                            !/*gd         81*/
         xx=zd(1)                                                               !/*gd         82*/
         yy=zd(2)                                                               !/*gd         83*/
         zz=zd(3)                                                               !/*gd         84*/
         uu=zd(4)                                                               !/*gd         85*/
         vv=zd(5)                                                               !/*gd         86*/
         ww=zd(6)                                                               !/*gd         87*/
      else                                                                      !/*gd         88*/
         call trfdat(m,tr)                                                      !/*gd         89*/
         xx=zd(1)*tr(4)+zd(2)*tr(7)+zd(3)*tr(10)+tr(1)                          !/*gd         90*/
         yy=zd(1)*tr(5)+zd(2)*tr(8)+zd(3)*tr(11)+tr(2)                          !/*gd         91*/
         zz=zd(1)*tr(6)+zd(2)*tr(9)+zd(3)*tr(12)+tr(3)                          !/*gd         92*/
         uu=zd(4)*tr(4)+zd(5)*tr(7)+zd(6)*tr(10)                                !/*gd         93*/
         vv=zd(4)*tr(5)+zd(5)*tr(8)+zd(6)*tr(11)                                !/*gd         94*/
         ww=zd(4)*tr(6)+zd(5)*tr(9)+zd(6)*tr(12)                                !/*gd         95*/
      endif                                                                     !/*gd         96*/
!                                                                               !/*gd         97*/
!       Determine the location of starting point of this track                  !/*gd         98*/
!       in each mesh for the proper particle type.                              !/*gd         99*/
!       Also score source weight if required.                                   !/*gd        100*/
!                                                                               !/*gd        101*/
      i1=0                                                                      !/*gd        102*/
      i2=0                                                                      !/*gd        103*/
      i3=0                                                                      !/*gd        104*/
      j1=0                                                                      !/*gd        105*/
      j2=0                                                                      !/*gd        106*/
      j3=0                                                                      !/*gd        107*/
!                                                                               !/*gd        108*/
      r=sqrt(xx*xx+yy*yy)                                                       !/*gd        109*/
      if(r.eq.zero.and.abs(ww).eq.one)go to 420                                 !/*gd        110*/
!                                                                               !/*gd        111*/
      if(r-ep.gt.gd1(lgd1(ig)+ng1(ig)).or.zz.lt.gd2(lgd2(ig)+1).                !/*gd        112*/
     & or.zz.gt.gd2(lgd2(ig)+ng2(ig)))go to 100                                 !/*gd        113*/
      if(gd1(lgd1(ig)+1).gt.0.) then                                            !/*gd        114*/
         if(r+ep.lt.gd1(lgd1(ig)+1)) then                                       !/*gd        115*/
            i1=1                                                                !/*gd        116*/
            i2=2                                                                !/*gd        117*/
            i3=1                                                                !/*gd        118*/
            go to 100                                                           !/*gd        119*/
         endif                                                                  !/*gd        120*/
      else                                                                      !/*gd        121*/
         if(r.eq.0.) then                                                       !/*gd        122*/
            i1=2                                                                !/*gd        123*/
            go to 40                                                            !/*gd        124*/
         endif                                                                  !/*gd        125*/
      endif                                                                     !/*gd        126*/
!                                                                               !/*gd        127*/
!       Determine indicies of point rr,zz,th.                                   !/*gd        128*/
!       and determine if this point is on a mesh boundary.                      !/*gd        129*/
!                                                                               !/*gd        130*/
      an=(xx/r)*uu+(yy/r)*vv                                                    !/*gd        131*/
      if(gd1(lgd1(ig)+1).gt.0.) then                                            !/*gd        132*/
         if(abs(r-gd1(lgd1(ig)+1)).le.ep) then                                  !/*gd        133*/
            if(an.lt.0.) then                                                   !/*gd        134*/
               i1=1                                                             !/*gd        135*/
               i2=2                                                             !/*gd        136*/
               i3=1                                                             !/*gd        137*/
               go to 100                                                        !/*gd        138*/
            else                                                                !/*gd        139*/
               j1=1                                                             !/*gd        140*/
               i1=2                                                             !/*gd        141*/
               go to 40                                                         !/*gd        142*/
            endif                                                               !/*gd        143*/
         endif                                                                  !/*gd        144*/
      endif                                                                     !/*gd        145*/
!                                                                               !/*gd        146*/
      if(abs(r-gd1(lgd1(ig)+ng1(ig))).le.ep) then                               !/*gd        147*/
         if(an.lt.0.) then                                                      !/*gd        148*/
            i1=ng1(ig)                                                          !/*gd        149*/
            j1=2                                                                !/*gd        150*/
            go to 40                                                            !/*gd        151*/
         else                                                                   !/*gd        152*/
            go to 420                                                           !/*gd        153*/
         endif                                                                  !/*gd        154*/
      endif                                                                     !/*gd        155*/
!                                                                               !/*gd        156*/
      ic=1                                                                      !/*gd        157*/
      ib=ng1(ig)                                                                !/*gd        158*/
   20 if(ib-ic.eq.1) go to 30                                                   !/*gd        159*/
      ih=(ic+ib)/2                                                              !/*gd        160*/
      if(abs(r-gd1(lgd1(ig)+ih)).lt.ep) go to 28                                !/*gd        161*/
      if(r.lt.gd1(lgd1(ig)+ih)) go to 25                                        !/*gd        162*/
      ic=ih                                                                     !/*gd        163*/
      go to 20                                                                  !/*gd        164*/
   25 ib=ih                                                                     !/*gd        165*/
      go to 20                                                                  !/*gd        166*/
!                                                                               !/*gd        167*/
   28 i1=ih                                                                     !/*gd        168*/
      if(an.gt.0) then                                                          !/*gd        169*/
         i1=i1+1                                                                !/*gd        170*/
         j1=1                                                                   !/*gd        171*/
      else                                                                      !/*gd        172*/
         j1=2                                                                   !/*gd        173*/
      endif                                                                     !/*gd        174*/
      go to 40                                                                  !/*gd        175*/
!                                                                               !/*gd        176*/
   30 i1=ic+1                                                                   !/*gd        177*/
!                                                                               !/*gd        178*/
   40 i2=2                                                                      !/*gd        179*/
      if(abs(gd2(lgd2(ig)+ng2(ig))-zz).lt.ep) then                              !/*gd        180*/
         if(ww.lt.0.) then                                                      !/*gd        181*/
            j2=2                                                                !/*gd        182*/
            go to 70                                                            !/*gd        183*/
         else                                                                   !/*gd        184*/
            go to 420                                                           !/*gd        185*/
         endif                                                                  !/*gd        186*/
      elseif(abs(gd2(lgd2(ig)+1)-zz).lt.ep) then                                !/*gd        187*/
         if(ww.gt.0.) then                                                      !/*gd        188*/
            j2=1                                                                !/*gd        189*/
            go to 70                                                            !/*gd        190*/
         else                                                                   !/*gd        191*/
            go to 420                                                           !/*gd        192*/
         endif                                                                  !/*gd        193*/
      endif                                                                     !/*gd        194*/
!                                                                               !/*gd        195*/
      ic=1                                                                      !/*gd        196*/
      ib=ng2(ig)                                                                !/*gd        197*/
   50 if(ib-ic.eq.1) go to 60                                                   !/*gd        198*/
      ih=(ic+ib)/2                                                              !/*gd        199*/
      if(abs(zz-gd2(lgd2(ig)+ih)).lt.ep) go to 65                               !/*gd        200*/
      if(zz.lt.gd2(lgd2(ig)+ih)) go to 55                                       !/*gd        201*/
      ic=ih                                                                     !/*gd        202*/
      go to 50                                                                  !/*gd        203*/
   55 ib=ih                                                                     !/*gd        204*/
      go to 50                                                                  !/*gd        205*/
!                                                                               !/*gd        206*/
   60 i2=ic+1                                                                   !/*gd        207*/
      go to 70                                                                  !/*gd        208*/
!                                                                               !/*gd        209*/
   65 i2=ih                                                                     !/*gd        210*/
      if(ww.ge.0.) then                                                         !/*gd        211*/
         i2=i2+1                                                                !/*gd        212*/
         j2=1                                                                   !/*gd        213*/
      else                                                                      !/*gd        214*/
         j2=2                                                                   !/*gd        215*/
      endif                                                                     !/*gd        216*/
!                                                                               !/*gd        217*/
   70 if(ng3(ig).gt.1) then                                                     !/*gd        218*/
         if(r.eq.0.) then                                                       !/*gd        219*/
            xt=xx+uu                                                            !/*gd        220*/
            yt=yy+vv                                                            !/*gd        221*/
         else                                                                   !/*gd        222*/
            xt=xx                                                               !/*gd        223*/
            yt=yy                                                               !/*gd        224*/
         endif                                                                  !/*gd        225*/
         rj=sqrt(xt*xt+yt*yt)                                                   !/*gd        226*/
         cc=min(one,max(-one,xt/rj))                                            !/*gd        227*/
         an=to_degs*acos(cc)                                                    !/*gd        228*/
         if(yt.lt.zero)an=deg_360-an                                            !/*gd        229*/
         if(yt.eq.zero.and.xt.ge.zero)an=zero                                   !/*gd        230*/
         if(yt.eq.zero.and.xt.lt.zero)an=deg_180                                !/*gd        231*/
      else                                                                      !/*gd        232*/
         an=zero                                                                !/*gd        233*/
         i3=1                                                                   !/*gd        234*/
         go to 93                                                               !/*gd        235*/
      endif                                                                     !/*gd        236*/
!                                                                               !/*gd        237*/
      if((abs(an-gd3(lgd3(ig)+ng3(ig))).lt.ep).or.                              !/*gd        238*/
     & (abs(an).lt.ep)) then                                                    !/*gd        239*/
         i3=ng3(ig)                                                             !/*gd        240*/
         go to 92                                                               !/*gd        241*/
      endif                                                                     !/*gd        242*/
      if(abs(an-gd3(lgd3(ig)+1)).lt.ep) then                                    !/*gd        243*/
         i3=1                                                                   !/*gd        244*/
         go to 92                                                               !/*gd        245*/
      endif                                                                     !/*gd        246*/
      if(an.lt.gd3(lgd3(ig)+1)-ep) then                                         !/*gd        247*/
         i3=1                                                                   !/*gd        248*/
         go to 93                                                               !/*gd        249*/
      endif                                                                     !/*gd        250*/
!                                                                               !/*gd        251*/
      ic=1                                                                      !/*gd        252*/
      ib=ng3(ig)                                                                !/*gd        253*/
   80 if(ib-ic.eq.1) go to 90                                                   !/*gd        254*/
      ih=(ic+ib)/2                                                              !/*gd        255*/
      if(an.lt.gd3(lgd3(ig)+ih)) go to 85                                       !/*gd        256*/
      ic=ih                                                                     !/*gd        257*/
      go to 80                                                                  !/*gd        258*/
   85 ib=ih                                                                     !/*gd        259*/
      go to 80                                                                  !/*gd        260*/
!                                                                               !/*gd        261*/
   90 i3=ic                                                                     !/*gd        262*/
      if(abs(an-gd3(lgd3(ig)+i3)).lt.ep) go to 92                               !/*gd        263*/
      i3=i3+1                                                                   !/*gd        264*/
      go to 93                                                                  !/*gd        265*/
!                                                                               !/*gd        266*/
   92 a=cos(to_rads*(deg_90+an))                                                !/*gd        267*/
      b=cos(to_rads*an)                                                         !/*gd        268*/
      t1=uu*a+vv*b                                                              !/*gd        269*/
      if(t1.gt.zero) then                                                       !/*gd        270*/
         i3=i3+1                                                                !/*gd        271*/
         if(i3.gt.ng3(ig))i3=1                                                  !/*gd        272*/
         j3=1                                                                   !/*gd        273*/
      else                                                                      !/*gd        274*/
         j3=2                                                                   !/*gd        275*/
      endif                                                                     !/*gd        276*/
!                                                                               !/*gd        277*/
!       Score source particles of the correct type or banked particles          !/*gd        278*/
!       that have not had a collision.                                          !/*gd        279*/
   93 if(mm.ne.1.and.mm.ne.8)go to 110                                          !/*gd        280*/
      do 95 i=1,mxgv(ig)                                                        !/*gd        281*/
      if(jp.eq.1.and.ngv(ig,i).ne.ipt) go to 95                                 !/*gd        282*/
      h=one                                                                     !/*gd        283*/
      ta=f_wt                                                                   !/*gd        284*/
      if(jp.eq.0.or.jp.eq.2) then                                               !/*gd        285*/
         if(jp.eq.0.and.ngv(ig,i).ne.4) go to 95                                !/*gd        286*/
         if(jp.eq.2) then                                                       !/*gd        287*/
            if(ngv(ig,i).ne.1.and.ngv(ig,i).ne.mt) go to 95                     !/*gd        288*/
            if(ipt.eq.2.and.kpt(3).ne.0) go to 95                               !/*gd        289*/
         endif                                                                  !/*gd        290*/
         lz=lgd2(ig)+i2                                                         !/*gd        291*/
         lr=lgd1(ig)+i1                                                         !/*gd        292*/
         lh=lgd3(ig)+i3                                                         !/*gd        293*/
      gj=0.                                                                     !/*gd2c        6*/
      if(i1.gt.1)gj=gd1(lr-1)                                                   !/*gd2c        7*/
      if(i2.le.1)call expirx(1,'gd2tal','index out of bounds.')                 !/*gd2c        8*/
      va=pie*(gd1(lr)**2-gj**2)*(gd2(lz)-gd2(lz-1))                             !/*gd2c        9*/
         if(ng3(ig).eq.1) then                                                  !/*gd        295*/
            ag=1.0                                                              !/*gd        296*/
         else                                                                   !/*gd        297*/
            if(i3.eq.1) then                                                    !/*gd        298*/
               ag=gd3(lh)/360                                                   !/*gd        299*/
            else                                                                !/*gd        300*/
               ag=(gd3(lh)-gd3(lh-1))/360.                                      !/*gd        301*/
            endif                                                               !/*gd        302*/
         endif                                                                  !/*gd        303*/
         vo=va*ag                                                               !/*gd        304*/
         if(vo.le.0.) then                                                      !/*gd        305*/
            call expirx(1,'gd2tal',                                             !/*gd2c       10*/
     &       'Detected a zero or negative mesh volume!')                        !/*gd        307*/
            return                                                              !/*gd        308*/
         endif                                                                  !/*gd        309*/
         ta=ta/vo                                                               !/*gd        310*/
         h=f_ed                                                                 !/*gd        311*/
         if (m1.ne.0 .and. m1.ne.-1) then                                       !/*gd        312*/
            eg=f_eg                                                             !/*gd        313*/
            if(m3.eq.1)eg=h*ta                                                  !/*gd        314*/
            if(m3.eq.2)eg=h*ta*f_eg                                             !/*gd        315*/
            ta=ta*rfact(m1,m2,m3,rx,eg,nemsh(m1))                               !/*gd        316*/
         elseif(m1.eq.-1) then                                                  !/*gd        317*/
            ta=ta*rx                                                            !/*gd        318*/
         endif                                                                  !/*gd        319*/
      endif                                                                     !/*gd        320*/
!                                                                               !/*gd        321*/
      mx=(i-1)*mxgc(ig)+(i3-1)*(ng1(ig)-1)*(ng2(ig)-1)+                         !/*gd        322*/
     & (i2-2)*(ng1(ig)-1)+i1-1                                                  !/*gd        323*/
      gdata(ktgd(ig)+mx)=gdata(ktgd(ig)+mx)+ta*h                                !/*gdxk        1*/
   95 continue                                                                  !/*gd        325*/
!                                                                               !/*gd        326*/
  100 continue                                                                  !/*gd        327*/
      if(mm.eq.1.or.mm.eq.8)return                                              !/*gd        328*/
!                                                                               !/*gd        329*/
!       Track the particle a distance 'da' in all meshs and score               !/*gd        330*/
!       tally data as required.                                                 !/*gd        331*/
!                                                                               !/*gd        332*/
  110 ds=da                                                                     !/*gd        333*/
      x=xx                                                                      !/*gd        334*/
      y=yy                                                                      !/*gd        335*/
      z=zz                                                                      !/*gd        336*/
      if(i1*i2*i3.gt.0)go to 300                                                !/*gd        337*/
!                                                                               !/*gd        338*/
!       The point is not in this mesh.  Can it enter this mesh within           !/*gd        339*/
!       a distance 'da', and if so where?                                       !/*gd        340*/
!                                                                               !/*gd        341*/
      r=sqrt(x*x+y*y)                                                           !/*gd        342*/
      if(r.gt.0.) then                                                          !/*gd        343*/
         t1=uu*x/r+vv*y/r                                                       !/*gd        344*/
      else                                                                      !/*gd        345*/
         t1=one                                                                 !/*gd        346*/
      endif                                                                     !/*gd        347*/
      rm=gd1(lgd1(ig)+ng1(ig))                                                  !/*gd        348*/
      zl=gd2(lgd2(ig)+1)                                                        !/*gd        349*/
      zh=gd2(lgd2(ig)+ng2(ig))                                                  !/*gd        350*/
!                                                                               !/*gd        351*/
      if(r.ge.rm.and.t1.gt.zero)go to 420                                       !/*gd        352*/
      if((ww.ge.zero.and.z.ge.zh).or.(ww.le.zero.and.z.le.zl))go to 420         !/*gd        353*/
!                                                                               !/*gd        354*/
!        Find the distance to the cylinder.                                     !/*gd        355*/
      if(r.lt.rm)go to 160                                                      !/*gd        356*/
      t1=uu**2+vv**2                                                            !/*gd        357*/
      if(t1.eq.zero)go to 420                                                   !/*gd        358*/
      t1=one/t1                                                                 !/*gd        359*/
      a1=(x*uu+y*vv)*t1                                                         !/*gd        360*/
      b1=(x*x+y*y-rm**2)*t1                                                     !/*gd        361*/
!                                                                               !/*gd        362*/
      t1=a1**2-b1                                                               !/*gd        363*/
      if(t1.lt.zero)go to 420                                                   !/*gd        364*/
      t2=sqrt(t1)                                                               !/*gd        365*/
      d1=-a1+t2                                                                 !/*gd        366*/
      if(d1.lt.zero)go to 420                                                   !/*gd        367*/
      dr=d1                                                                     !/*gd        368*/
      d2=-a1-t2                                                                 !/*gd        369*/
      if(d2.gt.zero)dr=d2                                                       !/*gd        370*/
      if(ds-dr.le.ep)go to 420                                                  !/*gd        371*/
!                                                                               !/*gd        372*/
      z1=z+ww*dr                                                                !/*gd        373*/
      if(z1.le.zl.or.z1.ge.zh)go to 160                                         !/*gd        374*/
      j1=2                                                                      !/*gd        375*/
      i1=ng1(ig)                                                                !/*gd        376*/
      if(ng2(ig).eq.2) then                                                     !/*gd        377*/
         i2=2                                                                   !/*gd        378*/
         go to 138                                                              !/*gd        379*/
      endif                                                                     !/*gd        380*/
!                                                                               !/*gd        381*/
      ic=1                                                                      !/*gd        382*/
      ib=ng2(ig)                                                                !/*gd        383*/
  120 if(ib-ic.eq.1) go to 130                                                  !/*gd        384*/
      ih=(ic+ib)/2                                                              !/*gd        385*/
      if(z1.lt.gd2(lgd2(ig)+ih)) go to 125                                      !/*gd        386*/
      ic=ih                                                                     !/*gd        387*/
      go to 120                                                                 !/*gd        388*/
  125 ib=ih                                                                     !/*gd        389*/
      go to 120                                                                 !/*gd        390*/
!                                                                               !/*gd        391*/
  130 i2=ic                                                                     !/*gd        392*/
      if(gd2(lgd2(ig)+i2).eq.z1) go to 135                                      !/*gd        393*/
      i2=i2+1                                                                   !/*gd        394*/
      go to 138                                                                 !/*gd        395*/
  135 if(ww.ge.0)i2=i2+1                                                        !/*gd        396*/
  138 x=x+uu*dr                                                                 !/*gd        397*/
      y=y+vv*dr                                                                 !/*gd        398*/
      z=z1                                                                      !/*gd        399*/
      ds=ds-dr                                                                  !/*gd        400*/
      if(ds.le.ep) go to 420                                                    !/*gd        401*/
!                                                                               !/*gd        402*/
      if(ng3(ig).gt.1) then                                                     !/*gd        403*/
         rj=sqrt(x*x+y*y)                                                       !/*gd        404*/
         if(rj.gt.0.) then                                                      !/*gd        405*/
            cc=min(one,max(-one,x/rj))                                          !/*gd        406*/
         else                                                                   !/*gd        407*/
            cc=one                                                              !/*gd        408*/
         endif                                                                  !/*gd        409*/
         an=to_degs*acos(cc)                                                    !/*gd        410*/
         if(y.lt.zero)an=deg_360-an                                             !/*gd        411*/
         if(y.eq.zero.and.x.ge.zero)an=zero                                     !/*gd        412*/
         if(y.eq.zero.and.x.lt.zero)an=deg_180                                  !/*gd        413*/
      else                                                                      !/*gd        414*/
         an=zero                                                                !/*gd        415*/
         i3=1                                                                   !/*gd        416*/
         go to 300                                                              !/*gd        417*/
      endif                                                                     !/*gd        418*/
      if(an.eq.gd3(lgd3(ig)+ng3(ig))) then                                      !/*gd        419*/
         i3=ng3(ig)                                                             !/*gd        420*/
         go to 155                                                              !/*gd        421*/
      endif                                                                     !/*gd        422*/
      if(an.eq.gd3(lgd3(ig)+1)) then                                            !/*gd        423*/
         i3=1                                                                   !/*gd        424*/
         go to 155                                                              !/*gd        425*/
      endif                                                                     !/*gd        426*/
      if(an.lt.gd3(lgd3(ig)+1)) then                                            !/*gd        427*/
         i3=1                                                                   !/*gd        428*/
         go to 300                                                              !/*gd        429*/
      endif                                                                     !/*gd        430*/
!                                                                               !/*gd        431*/
      ic=1                                                                      !/*gd        432*/
      ib=ng3(ig)                                                                !/*gd        433*/
  140 if(ib-ic.eq.1) go to 150                                                  !/*gd        434*/
      ih=(ic+ib)/2                                                              !/*gd        435*/
      if(an.lt.gd3(lgd3(ig)+ih)) go to 145                                      !/*gd        436*/
      ic=ih                                                                     !/*gd        437*/
      go to 140                                                                 !/*gd        438*/
  145 ib=ih                                                                     !/*gd        439*/
      go to 140                                                                 !/*gd        440*/
!                                                                               !/*gd        441*/
  150 i3=ic                                                                     !/*gd        442*/
      if(an.lt.gd3(lgd3(ig)+1)) go to 300                                       !/*gd        443*/
      if(gd3(lgd3(ig)+i3).eq.an) go to 155                                      !/*gd        444*/
      i3=i3+1                                                                   !/*gd        445*/
      if(i3.gt.ng3(ig))i3=1                                                     !/*gd        446*/
      go to 300                                                                 !/*gd        447*/
!                                                                               !/*gd        448*/
  155 a=cos(to_rads*(deg_90+an))                                                !/*gd        449*/
      b=cos(to_rads*an)                                                         !/*gd        450*/
      t1=uu*a+vv*b                                                              !/*gd        451*/
      if(t1.gt.zero) then                                                       !/*gd        452*/
         j3=1                                                                   !/*gd        453*/
         i3=i3+1                                                                !/*gd        454*/
         if(i3.gt.ng3(ig))i3=1                                                  !/*gd        455*/
      else                                                                      !/*gd        456*/
         j3=2                                                                   !/*gd        457*/
      endif                                                                     !/*gd        458*/
      go to 300                                                                 !/*gd        459*/
!                                                                               !/*gd        460*/
  160 if(z.lt.zh.and.z.gt.zl)go to 420                                          !/*gd        461*/
      if(ww.ne.zero) then                                                       !/*gd        462*/
         dz=(zl-z)/ww                                                           !/*gd        463*/
         if(z.ge.zh)dz=(zh-z)/ww                                                !/*gd        464*/
      else                                                                      !/*gd        465*/
         go to 420                                                              !/*gd        466*/
      endif                                                                     !/*gd        467*/
      if(dz.ge.ds)go to 420                                                     !/*gd        468*/
      x=x+uu*dz                                                                 !/*gd        469*/
      y=y+vv*dz                                                                 !/*gd        470*/
      r1=sqrt(x*x+y*y)                                                          !/*gd        471*/
      if(r1.gt.rm)go to 420                                                     !/*gd        472*/
      if(z.ge.zh) then                                                          !/*gd        473*/
         i2=ng2(ig)                                                             !/*gd        474*/
         z=zh                                                                   !/*gd        475*/
      elseif(z.le.zl) then                                                      !/*gd        476*/
         i2=2                                                                   !/*gd        477*/
         z=zl                                                                   !/*gd        478*/
      endif                                                                     !/*gd        479*/
!                                                                               !/*gd        480*/
      if(r1.eq.0.) then                                                         !/*gd        481*/
         x1=x+uu                                                                !/*gd        482*/
         y1=y+vv                                                                !/*gd        483*/
         i1=2                                                                   !/*gd        484*/
         if(gd1(lgd1(ig)+1).gt.0.)i1=1                                          !/*gd        485*/
         r1=sqrt(x1*x1+y1*y1)                                                   !/*gd        486*/
         go to 185                                                              !/*gd        487*/
      else                                                                      !/*gd        488*/
         x1=x                                                                   !/*gd        489*/
         y1=y                                                                   !/*gd        490*/
      endif                                                                     !/*gd        491*/
!                                                                               !/*gd        492*/
      if(gd1(lgd1(ig)+1).gt.0.) then                                            !/*gd        493*/
         if(r1.lt.gd1(lgd1(ig)+1)) then                                         !/*gd        494*/
            i1=1                                                                !/*gd        495*/
            i3=1                                                                !/*gd        496*/
            go to 210                                                           !/*gd        497*/
         endif                                                                  !/*gd        498*/
      endif                                                                     !/*gd        499*/
!                                                                               !/*gd        500*/
      ic=1                                                                      !/*gd        501*/
      ib=ng1(ig)                                                                !/*gd        502*/
  170 if(ib-ic.eq.1) go to 180                                                  !/*gd        503*/
      ih=(ic+ib)/2                                                              !/*gd        504*/
      if(r1.lt.gd1(lgd1(ig)+ih)) go to 175                                      !/*gd        505*/
      ic=ih                                                                     !/*gd        506*/
      go to 170                                                                 !/*gd        507*/
  175 ib=ih                                                                     !/*gd        508*/
      go to 170                                                                 !/*gd        509*/
!                                                                               !/*gd        510*/
  180 i1=ic                                                                     !/*gd        511*/
      if(abs(r1-gd1(lgd1(ig)+i1)).lt.ep) then                                   !/*gd        512*/
         an=uu*x1/r1+vv*y1/r1                                                   !/*gd        513*/
         if(an.ge.zero) then                                                    !/*gd        514*/
            i1=i1+1                                                             !/*gd        515*/
            j1=2                                                                !/*gd        516*/
         else                                                                   !/*gd        517*/
            j1=1                                                                !/*gd        518*/
         endif                                                                  !/*gd        519*/
         go to 182                                                              !/*gd        520*/
      endif                                                                     !/*gd        521*/
      i1=i1+1                                                                   !/*gd        522*/
  182 if(i1.eq.1) then                                                          !/*gd        523*/
         j1=2                                                                   !/*gd        524*/
         i2=2                                                                   !/*gd        525*/
         i3=1                                                                   !/*gd        526*/
         go to 210                                                              !/*gd        527*/
      endif                                                                     !/*gd        528*/
!                                                                               !/*gd        529*/
  185 if(ng3(ig).gt.1) then                                                     !/*gd        530*/
         cc=min(one,max(-one,x1/r1))                                            !/*gd        531*/
         an=to_degs*acos(cc)                                                    !/*gd        532*/
         if(y1.lt.zero)an=deg_360-an                                            !/*gd        533*/
         if(y1.eq.zero.and.x1.ge.zero)an=zero                                   !/*gd        534*/
         if(y1.eq.zero.and.x1.lt.zero)an=deg_180                                !/*gd        535*/
      else                                                                      !/*gd        536*/
         an=zero                                                                !/*gd        537*/
         i3=1                                                                   !/*gd        538*/
         go to 210                                                              !/*gd        539*/
      endif                                                                     !/*gd        540*/
!                                                                               !/*gd        541*/
      if(an.eq.gd3(lgd3(ig)+ng3(ig))) then                                      !/*gd        542*/
         i3=ng3(ig)                                                             !/*gd        543*/
         go to 205                                                              !/*gd        544*/
      endif                                                                     !/*gd        545*/
      if(an.eq.gd3(lgd3(ig)+1)) then                                            !/*gd        546*/
         i3=1                                                                   !/*gd        547*/
         go to 205                                                              !/*gd        548*/
      endif                                                                     !/*gd        549*/
      if(an.lt.gd3(lgd3(ig)+1)) then                                            !/*gd        550*/
         i3=1                                                                   !/*gd        551*/
         go to 210                                                              !/*gd        552*/
      endif                                                                     !/*gd        553*/
!                                                                               !/*gd        554*/
      ic=1                                                                      !/*gd        555*/
      ib=ng3(ig)                                                                !/*gd        556*/
  190 if(ib-ic.eq.1) go to 200                                                  !/*gd        557*/
      ih=(ic+ib)/2                                                              !/*gd        558*/
      if(an.lt.gd3(lgd3(ig)+ih)) go to 195                                      !/*gd        559*/
      ic=ih                                                                     !/*gd        560*/
      go to 190                                                                 !/*gd        561*/
  195 ib=ih                                                                     !/*gd        562*/
      go to 190                                                                 !/*gd        563*/
!                                                                               !/*gd        564*/
  200 i3=ic                                                                     !/*gd        565*/
      if(an.lt.gd3(lgd3(ig)+1)) go to 210                                       !/*gd        566*/
      if(gd3(lgd3(ig)+i3).eq.an) go to 205                                      !/*gd        567*/
      i3=i3+1                                                                   !/*gd        568*/
      if(i3.gt.ng3(ig))i3=1                                                     !/*gd        569*/
      go to 210                                                                 !/*gd        570*/
!                                                                               !/*gd        571*/
  205 a=cos(to_rads*(deg_90+an))                                                !/*gd        572*/
      b=cos(to_rads*an)                                                         !/*gd        573*/
      t1=uu*a+vv*b                                                              !/*gd        574*/
      if(t1.gt.zero) then                                                       !/*gd        575*/
         i3=i3+1                                                                !/*gd        576*/
         if(i3.gt.ng3(ig))i3=1                                                  !/*gd        577*/
      endif                                                                     !/*gd        578*/
!                                                                               !/*gd        579*/
  210 ds=ds-dz                                                                  !/*gd        580*/
!                                                                               !/*gd        581*/
!       Track the particle in this mesh until the distance 'ds'                 !/*gd        582*/
!       is exhausted or the particle leaves this mesh.                          !/*gd        583*/
  300 lz=lgd2(ig)+i2                                                            !/*gd        584*/
      lr=lgd1(ig)+i1                                                            !/*gd        585*/
      lh=lgd3(ig)+i3                                                            !/*gd        586*/
      rr=sqrt(x*x+y*y)                                                          !/*gd        587*/
      dz=huge                                                                   !/*gd        588*/
      if(ww.eq.zero)go to 305                                                   !/*gd        589*/
      if(i1.gt.1) then                                                          !/*gd        590*/
         dz=(gd2(lz)-z)/ww                                                      !/*gd        591*/
         if(ww.lt.zero)dz=(gd2(lz-1)-z)/ww                                      !/*gd        592*/
      else                                                                      !/*gd        593*/
         dz=(gd2(lgd2(ig)+ng2(ig))-z)/ww                                        !/*gd        594*/
         if(ww.lt.zero)dz=(gd2(lgd2(ig)+1)-z)/ww                                !/*gd        595*/
      endif                                                                     !/*gd        596*/
!                                                                               !/*gd        597*/
!       Calculate the distance to the next plane.                               !/*gd        598*/
  305 p1=huge                                                                   !/*gd        599*/
      p2=huge                                                                   !/*gd        600*/
      if(ng3(ig).eq.1)go to 320                                                 !/*gd        601*/
      if(j3.eq.2)go to 310                                                      !/*gd        602*/
      an=gd3(lh)                                                                !/*gd        603*/
      kk=0                                                                      !/*gd        604*/
      if(an.eq.90.) then                                                        !/*gd        605*/
         kk=1                                                                   !/*gd        606*/
         a=1.                                                                   !/*gd        607*/
         b=0.                                                                   !/*gd        608*/
      elseif(an.eq.180.) then                                                   !/*gd        609*/
         kk=1                                                                   !/*gd        610*/
         a=0.                                                                   !/*gd        611*/
         b=-1.                                                                  !/*gd        612*/
      elseif(an.eq.270.) then                                                   !/*gd        613*/
         kk=1                                                                   !/*gd        614*/
         a=-1.                                                                  !/*gd        615*/
         b=0.                                                                   !/*gd        616*/
      elseif(an.eq.360.) then                                                   !/*gd        617*/
         kk=1                                                                   !/*gd        618*/
         a=0.                                                                   !/*gd        619*/
         b=1.                                                                   !/*gd        620*/
      endif                                                                     !/*gd        621*/
      if(kk.eq.0) then                                                          !/*gd        622*/
         a=cos(to_rads*(deg_90+an))                                             !/*gd        623*/
         b=cos(to_rads*an)                                                      !/*gd        624*/
      endif                                                                     !/*gd        625*/
      t1=uu*a+vv*b                                                              !/*gd        626*/
      if(t1.eq.zero)go to 310                                                   !/*gd        627*/
      d=(-x*a-y*b)/t1                                                           !/*gd        628*/
      if(d.le.zero)go to 310                                                    !/*gd        629*/
      xt=x+d*uu                                                                 !/*gd        630*/
      yt=y+d*vv                                                                 !/*gd        631*/
      rt=sqrt(xt*xt+yt*yt)                                                      !/*gd        632*/
      if(rt.lt.gd1(lgd1(ig)+1))go to 310                                        !/*gd        633*/
      if(xt.lt.0..and.(an.eq.0..or.an.eq.360.))go to 310                        !/*gd        634*/
      if(xt.ge.0..and.an.eq.180.)go to 310                                      !/*gd        635*/
      if(yt.lt.0..and.an.eq.90.)go to 310                                       !/*gd        636*/
      if(yt.ge.0..and.an.eq.270.)go to 310                                      !/*gd        637*/
      if(yt.gt.0..and.an.gt.0..and.an.lt.180.)then                              !/*gd        638*/
         if(abs(xt).lt.ep.and.an.eq.90.)go to 309                               !/*gd        639*/
         if(xt.gt.0..and.an.lt.90.)go to 309                                    !/*gd        640*/
         if(xt.lt.0..and.an.gt.90.)go to 309                                    !/*gd        641*/
         go to 310                                                              !/*gd        642*/
      endif                                                                     !/*gd        643*/
      if(yt.lt.0..and.an.gt.180..and.an.lt.360.)then                            !/*gd        644*/
         if(abs(xt).lt.ep.and.an.eq.270.)go to 309                              !/*gd        645*/
         if(xt.gt.0..and.an.gt.270.)go to 309                                   !/*gd        646*/
         if(xt.lt.0..and.an.lt.270.)go to 309                                   !/*gd        647*/
         go to 310                                                              !/*gd        648*/
      endif                                                                     !/*gd        649*/
  309 p2=d                                                                      !/*gd        650*/
!                                                                               !/*gd        651*/
  310 if(j3.eq.1)go to 320                                                      !/*gd        652*/
      an=0.                                                                     !/*gd2c       11*/
      if(i3.gt.1)an=gd3(lh-1)                                                   !/*gd2c       12*/
      kk=0                                                                      !/*gd        654*/
      if(an.eq.0.) then                                                         !/*gd        655*/
         kk=1                                                                   !/*gd        656*/
         a=0.                                                                   !/*gd        657*/
         b=1                                                                    !/*gd        658*/
      elseif(an.eq.90.) then                                                    !/*gd        659*/
         kk=1                                                                   !/*gd        660*/
         a=1.                                                                   !/*gd        661*/
         b=0.                                                                   !/*gd        662*/
      elseif(an.eq.180.) then                                                   !/*gd        663*/
         kk=1                                                                   !/*gd        664*/
         a=0.                                                                   !/*gd        665*/
         b=-1.                                                                  !/*gd        666*/
      elseif(an.eq.270.) then                                                   !/*gd        667*/
         kk=1                                                                   !/*gd        668*/
         a=-1.                                                                  !/*gd        669*/
         b=0.                                                                   !/*gd        670*/
      endif                                                                     !/*gd        671*/
      if(kk.eq.0) then                                                          !/*gd        672*/
         a=cos(to_rads*(deg_90+an))                                             !/*gd        673*/
         b=cos(to_rads*an)                                                      !/*gd        674*/
      endif                                                                     !/*gd        675*/
      t1=uu*a+vv*b                                                              !/*gd        676*/
      if(t1.eq.zero)go to 320                                                   !/*gd        677*/
      d=(-x*a-y*b)/t1                                                           !/*gd        678*/
      if(d.le.zero)go to 320                                                    !/*gd        679*/
      xt=x+d*uu                                                                 !/*gd        680*/
      yt=y+d*vv                                                                 !/*gd        681*/
      rt=sqrt(xt*xt+yt*yt)                                                      !/*gd        682*/
      if(rt.lt.gd1(lgd1(ig)+1))go to 320                                        !/*gd        683*/
      if(i3.eq.1)ax=zero                                                        !/*gd        684*/
      if(xt.lt.0..and.(an.eq.0..or.an.eq.360.))go to 320                        !/*gd        685*/
      if(xt.ge.0..and.an.eq.180.)go to 320                                      !/*gd        686*/
      if(yt.lt.0..and.an.eq.90.)go to 320                                       !/*gd        687*/
      if(yt.ge.0..and.an.eq.270.)go to 320                                      !/*gd        688*/
      if(yt.gt.0..and.an.gt.0..and.an.lt.180.)then                              !/*gd        689*/
         if(abs(xt).lt.ep.and.an.eq.90.)go to 315                               !/*gd        690*/
         if(xt.gt.0..and.an.lt.90.)go to 315                                    !/*gd        691*/
         if(xt.lt.0..and.an.gt.90.)go to 315                                    !/*gd        692*/
         go to 320                                                              !/*gd        693*/
      endif                                                                     !/*gd        694*/
      if(yt.lt.0..and.an.gt.180..and.an.lt.360.)then                            !/*gd        695*/
         if(abs(xt).lt.ep.and.an.eq.270.)go to 315                              !/*gd        696*/
         if(xt.gt.0..and.an.gt.270.)go to 315                                   !/*gd        697*/
         if(xt.lt.0..and.an.lt.270.)go to 315                                   !/*gd        698*/
         go to 320                                                              !/*gd        699*/
      endif                                                                     !/*gd        700*/
  315 p1=d                                                                      !/*gd        701*/
!                                                                               !/*gd        702*/
  320 d3=huge                                                                   !/*gd        703*/
      d4=huge                                                                   !/*gd        704*/
      t1=uu**2+vv**2                                                            !/*gd        705*/
      if(t1.eq.zero) go to 340                                                  !/*gd        706*/
      t1=one/t1                                                                 !/*gd        707*/
      a1=(x*uu+y*vv)*t1                                                         !/*gd        708*/
      b1=(rr**2-gd1(lr)**2)*t1                                                  !/*gd        709*/
!                                                                               !/*gd        710*/
      t2=a1**2-b1                                                               !/*gd        711*/
      if(t2.lt.zero)go to 330                                                   !/*gd        712*/
      t3=sqrt(t2)                                                               !/*gd        713*/
      d1=-a1+t3                                                                 !/*gd        714*/
      if(d1.le.zero)go to 330                                                   !/*gd        715*/
      d3=d1                                                                     !/*gd        716*/
!                                                                               !/*gd        717*/
  330 if(j1.eq.1)go to 340                                                      !/*gd        718*/
      gj=0.                                                                     !/*gd2c       13*/
      if(i1.gt.1)gj=gd1(lr-1)                                                   !/*gd2c       14*/
      b1=(x*x+y*y-gj**2)*t1                                                     !/*gd2c       15*/
      t2=a1**2-b1                                                               !/*gd        720*/
      if(t2.le.zero)go to 340                                                   !/*gd        721*/
      t3=sqrt(t2)                                                               !/*gd        722*/
      d1=-a1+t3                                                                 !/*gd        723*/
      if(d1.le.zero)go to 340                                                   !/*gd        724*/
      d4=d1                                                                     !/*gd        725*/
      d2=-a1-t3                                                                 !/*gd        726*/
      if(d2.gt.zero)d4=d2                                                       !/*gd        727*/
!                                                                               !/*gd        728*/
  340 d=min(p1,p2,d3,d4,dz,ds)                                                  !/*gd        729*/
      dm=d                                                                      !/*gd        730*/
      if(d.eq.zero)dm=p_1m12                                                    !/*gd        731*/
      if(d.eq.zero.or.i1.le.1)go to 401                                         !/*gd        732*/
!                                                                               !/*gd        733*/
      if(i2.le.1)call expirx(1,'gd2tal','index out of bounds.')                 !/*gd2c       16*/
      va=pie*(gd1(lr)**2-gd1(lr-1)**2)*(gd2(lz)-gd2(lz-1))                      !/*gd        734*/
      if(ng3(ig).eq.1) then                                                     !/*gd        735*/
         ag=one                                                                 !/*gd        736*/
      else                                                                      !/*gd        737*/
         if(i3.eq.1) then                                                       !/*gd        738*/
            ag=gd3(lh)/deg_360                                                  !/*gd        739*/
         else                                                                   !/*gd        740*/
            ag=(gd3(lh)-gd3(lh-1))/deg_360                                      !/*gd        741*/
         endif                                                                  !/*gd        742*/
      endif                                                                     !/*gd        743*/
      vo=va*ag                                                                  !/*gd        744*/
      if(vo.le.zero)call expirx(1,'gd2tal',                                     !/*gd2c       17*/
     & 'Detected a zero or negative mesh volume!')                              !/*gd        746*/
!                                                                               !/*gd        747*/
  343 jj=0                                                                      !/*gd        748*/
      fc=zero                                                                   !/*gd        749*/
      if(da.gt.zero)fc=d/da                                                     !/*gd        750*/
      do 400 i=1,mxgv(ig)                                                       !/*gd        751*/
      h=one                                                                     !/*gd        752*/
      if(jp.eq.1) then                                                          !/*gd        753*/
         if(ipt.ne.ngv(ig,i))go to 400                                          !/*gd        754*/
         ta=f_wt                                                                !/*gd        755*/
         go to 395                                                              !/*gd        756*/
      endif                                                                     !/*gd        757*/
      if(jp.eq.2) then                                                          !/*gd        758*/
         if(ngv(ig,i).ne.1.and.ngv(ig,i).ne.mt)go to 400                        !/*gd        759*/
         go to 355                                                              !/*gd        760*/
      endif                                                                     !/*gd        761*/
      if(jp.ge.3)go to 350                                                      !/*gd        762*/
      go to (350,345,354,355,352)ngv(ig,i)                                      !/*gd        763*/
!                                                                               !/*gd        764*/
  345 ta=one                                                                    !/*gd        765*/
      go to 393                                                                 !/*gd        766*/
!                                                                               !/*gd        767*/
  350 ta=d*f_wt/vo                                                              !/*gd        768*/
      go to 393                                                                 !/*gd        769*/
!                                                                               !/*gd        770*/
  352 jj=jj+1                                                                   !/*gd        771*/
      n1=mdos(1,jj,ig)                                                          !/*gd        772*/
      if(n1.eq.0)n1=10                                                          !/*gd        773*/
      n2=mdos(2,jj,ig)                                                          !/*gd        774*/
      if(n2.eq.0)n2=1                                                           !/*gd        775*/
      n3=mdos(3,jj,ig)                                                          !/*gd        776*/
      if(n3.eq.0)n3=1                                                           !/*gd        777*/
      rm=rdos(jj,ig)                                                            !/*gd        778*/
      if(rm.eq.zero)rm=one                                                      !/*gd        779*/
      if(n1.ge.10) then                                                         !/*gd        780*/
         ft=dfact(ipt,n1,f_eg,n2,n3,rm)                                         !/*gd        781*/
      else                                                                      !/*gd        782*/
         ft=rfact(n1,n2,n3,rm,f_eg,nemsh(n1))                                   !/*gd        783*/
      endif                                                                     !/*gd        784*/
      ta=ft*d*f_wt/vo                                                           !/*gd        785*/
      go to 395                                                                 !/*gd        786*/
!                                                                               !/*gd        787*/
  354 ta=d*f_wt                                                                 !/*gd        788*/
      go to 393                                                                 !/*gd        789*/
!                                                                               !/*gd        790*/
  355 ta=f_wt/vo                                                                !/*gd        791*/
      if(ipt.eq.1.and.erg.gt.cut_n)go to 400                                    !/*gd        792*/
      if(mk.eq.0.or.(jgp.lt.0.and.ipt.eq.2))go to 400                           !/*gd        793*/
      if(jp.eq.2.and.ipt.eq.2.and.                                              !/*gd        794*/
     & (jgp.lt.0.or.kpt(3).ne.0))go to 400                                      !/*gd        795*/
!                                                                               !/*gd        796*/
!       Particle energy deposition.                                             !/*gd        797*/
  390 h=fc*f_ed                                                                 !/*gd        798*/
      f_eg=f_eg-h                                                               !/*gd        799*/
!                                                                               !/*gd        800*/
!       Energy-dependent multiplier function.                                   !/*gd        801*/
  393 if (m1.ne.0 .and. m1.ne.-1) then                                          !/*gd        802*/
         eg=f_eg                                                                !/*gd        803*/
         if(m3.eq.1)eg=h*ta                                                     !/*gd        804*/
         if(m3.eq.2)eg=h*ta*f_eg                                                !/*gd        805*/
         ta=ta*rfact(m1,m2,m3,rx,eg,nemsh(m1))                                  !/*gd        806*/
      elseif(m1.eq.-1) then                                                     !/*gd        807*/
         ta=ta*rx                                                               !/*gd        808*/
      endif                                                                     !/*gd        809*/
!                                                                               !/*gd        810*/
  395 if(jp.eq.0) then                                                          !/*gd        811*/
         il=-1                                                                  !/*gd        812*/
         do 396 k=1,ntal+mshfm                                                  !/*gd        813*/
         if(nugd(ig).eq.jptal(ljpt+1,k)) then                                   !/*gd        814*/
            il=k                                                                !/*gd        815*/
         endif                                                                  !/*gd        816*/
  396    continue                                                               !/*gd        817*/
         if(il.lt.0)go to 397                                                   !/*gd        818*/
!                                                                               !/*gd        819*/
         if(iptal(lipt+5,2,il).ne.0) then                                       !/*gd        820*/
            ital=il                                                             !/*gd        821*/
            if(iptal(lipt+5,1,il).ne.0)call wtmult(ta,1)                        !/*gd        822*/
            ta=ta*tds(iptal(lipt+5,2,il)+1)                                     !/*gd        823*/
            if(tds(iptal(lipt+5,2,il)+1).lt.0.)ta=-ta*rho(lrho+icl)             !/*gd        824*/
         endif                                                                  !/*gd        825*/
!                                                                               !/*gd        826*/
         if(iptal(lipt+3,2,il).ne.0) then                                       !/*gd        827*/
            ital=il                                                             !/*gd        828*/
            ta=dosef(ta)                                                        !/*gd        829*/
         endif                                                                  !/*gd        830*/
      endif                                                                     !/*gd        831*/
!                                                                               !/*gd        832*/
  397 mx=(i-1)*mxgc(ig)+(i3-1)*(ng1(ig)-1)*(ng2(ig)-1)+                         !/*gd        833*/
     & (i2-2)*(ng1(ig)-1)+i1-1                                                  !/*gd        834*/
      gdata(ktgd(ig)+mx)=gdata(ktgd(ig)+mx)+ta*h                                !/*gdxk        2*/
!                                                                               !/*gd        836*/
  400 continue                                                                  !/*gd        837*/
!                                                                               !/*gd        838*/
!       Move a distance 'd', advance the indicies if required,                  !/*gd        839*/
!       and continue tracking until 'ds'=0 or particle exits mesh.              !/*gd        840*/
  401 ds=ds-dm                                                                  !/*gd        841*/
      if(ds.le.ep)go to 420                                                     !/*gd        842*/
      if(d.eq.zero) then                                                        !/*gd        843*/
         j1=0                                                                   !/*gd        844*/
         j3=0                                                                   !/*gd        845*/
         go to 402                                                              !/*gd        846*/
      endif                                                                     !/*gd        847*/
      j1=0                                                                      !/*gd        848*/
      j3=0                                                                      !/*gd        849*/
      if(d.eq.d3) then                                                          !/*gd        850*/
         j1=1                                                                   !/*gd        851*/
         i1=i1+1                                                                !/*gd        852*/
         if(i1.gt.ng1(ig))go to 420                                             !/*gd        853*/
      elseif(d.eq.d4) then                                                      !/*gd        854*/
         j1=2                                                                   !/*gd        855*/
         i1=i1-1                                                                !/*gd        856*/
      elseif(d.eq.p1) then                                                      !/*gd        857*/
         j3=2                                                                   !/*gd        858*/
         i3=i3-1                                                                !/*gd        859*/
         if(i3.lt.1)i3=ng3(ig)                                                  !/*gd        860*/
      elseif(d.eq.p2) then                                                      !/*gd        861*/
         j3=1                                                                   !/*gd        862*/
         i3=i3+1                                                                !/*gd        863*/
         if(i3.gt.ng3(ig))i3=1                                                  !/*gd        864*/
      elseif(d.eq.dz) then                                                      !/*gd        865*/
         if(ww.gt.zero)i2=i2+1                                                  !/*gd        866*/
         if(ww.lt.zero)i2=i2-1                                                  !/*gd        867*/
         if(i2.eq.1.or.i2.gt.ng2(ig))go to 420                                  !/*gd        868*/
      endif                                                                     !/*gd        869*/
!                                                                               !/*gd        870*/
  402 x=x+dm*uu                                                                 !/*gd        871*/
      y=y+dm*vv                                                                 !/*gd        872*/
      z=z+dm*ww                                                                 !/*gd        873*/
      if(i1.eq.2.and.j1.eq.1.and.gd1(lgd1(ig)+1).gt.0.) then                    !/*gd        874*/
         ic=1                                                                   !/*gd        875*/
         ib=ng2(ig)                                                             !/*gd        876*/
  403    if(ib-ic.eq.1) go to 405                                               !/*gd        877*/
         ih=(ic+ib)/2                                                           !/*gd        878*/
         if(z.lt.gd2(lgd2(ig)+ih)) go to 404                                    !/*gd        879*/
         ic=ih                                                                  !/*gd        880*/
         go to 403                                                              !/*gd        881*/
  404    ib=ih                                                                  !/*gd        882*/
         go to 403                                                              !/*gd        883*/
!                                                                               !/*gd        884*/
  405    i2=ic                                                                  !/*gd        885*/
         if(gd2(lgd2(ig)+i2).eq.z) then                                         !/*gd        886*/
            if(www.lt.0.) go to 406                                             !/*gd        887*/
         endif                                                                  !/*gd        888*/
!                                                                               !/*gd        889*/
         i2=i2+1                                                                !/*gd        890*/
         if(i2.gt.ng2(ig))go to 420                                             !/*gd        891*/
!                                                                               !/*gd        892*/
  406    if(ng3(ig).gt.1) then                                                  !/*gd        893*/
            rj=sqrt(x*x+y*y)                                                    !/*gd        894*/
            if(rj.gt.0.) then                                                   !/*gd        895*/
               cc=min(one,max(-one,x/rj))                                       !/*gd        896*/
            else                                                                !/*gd        897*/
               cc=one                                                           !/*gd        898*/
            endif                                                               !/*gd        899*/
            an=to_degs*acos(cc)                                                 !/*gd        900*/
            if(y.lt.zero)an=deg_360-an                                          !/*gd        901*/
            if(y.eq.zero.and.x.ge.zero)an=zero                                  !/*gd        902*/
            if(y.eq.zero.and.x.lt.zero)an=deg_180                               !/*gd        903*/
         else                                                                   !/*gd        904*/
            an=zero                                                             !/*gd        905*/
            i3=1                                                                !/*gd        906*/
            go to 300                                                           !/*gd        907*/
         endif                                                                  !/*gd        908*/
!                                                                               !/*gd        909*/
         if(abs(an-gd3(lgd3(ig)+ng3(ig))).lt.ep) then                           !/*gd        910*/
            i3=ng3(ig)                                                          !/*gd        911*/
            go to 410                                                           !/*gd        912*/
         endif                                                                  !/*gd        913*/
         if(abs(an-gd3(lgd3(ig)+1)).lt.ep) then                                 !/*gd        914*/
            i3=1                                                                !/*gd        915*/
            go to 410                                                           !/*gd        916*/
         endif                                                                  !/*gd        917*/
         if(an+ep.lt.gd3(lgd3(ig)+1)) then                                      !/*gd        918*/
            i3=1                                                                !/*gd        919*/
            go to 300                                                           !/*gd        920*/
         endif                                                                  !/*gd        921*/
!                                                                               !/*gd        922*/
         ic=1                                                                   !/*gd        923*/
         ib=ng3(ig)                                                             !/*gd        924*/
  407    if(ib-ic.eq.1) go to 409                                               !/*gd        925*/
         ih=(ic+ib)/2                                                           !/*gd        926*/
         if(an.lt.gd3(lgd3(ig)+ih)) go to 408                                   !/*gd        927*/
         ic=ih                                                                  !/*gd        928*/
         go to 407                                                              !/*gd        929*/
  408    ib=ih                                                                  !/*gd        930*/
         go to 407                                                              !/*gd        931*/
!                                                                               !/*gd        932*/
  409    i3=ic                                                                  !/*gd        933*/
         if(abs(an-gd3(lgd3(ig)+i3)).lt.ep) go to 410                           !/*gd        934*/
         i3=i3+1                                                                !/*gd        935*/
         go to 300                                                              !/*gd        936*/
!                                                                               !/*gd        937*/
  410    a=cos(to_rads*(deg_90+an))                                             !/*gd        938*/
         b=cos(to_rads*an)                                                      !/*gd        939*/
         t1=uu*a+vv*b                                                           !/*gd        940*/
         if(t1.gt.zero) then                                                    !/*gd        941*/
            i3=i3+1                                                             !/*gd        942*/
            if(i3.gt.ng3(ig))i3=1                                               !/*gd        943*/
         endif                                                                  !/*gd        944*/
      endif                                                                     !/*gd        945*/
      go to 300                                                                 !/*gd        946*/
!                                                                               !/*gd        947*/
  420 continue                                                                  !/*gd        948*/
!                                                                               !/*gd        949*/
#endif                                                                          /*!gd        950*/
      return                                                                    !/*gd        951*/
      end                                                                       !/*gd        952*/
