!     $Id: gd1tal.F,v 1.9 2002/06/14 17:05:54 jxh Exp $                         !/*gd          1*/
      subroutine gd1tal(mm,da,ig,zd,jz)                                         !/*gd          2*/
#ifdef MESHTAL                                                                  /*!gd          3*/
!                                                                               !/*gd          4*/
!       This subroutine is designed to score a volume tally like                !/*gd          5*/
!       quantity in a three dimensional rectangular mesh for later              !/*gd          6*/
!       post processing with any number of graphics packages.                   !/*gd          7*/
!                                                                               !/*gd          9*/
!       mm=1   Locate source or banked particle relative to mesh                !/*gd         10*/
!              and score if this is a point mesh.                               !/*gd         11*/
!       mm=2   Track particle a distance 'da' and score tally info.             !/*gd         12*/
!       mm=5   Score the track of a point detector contribution.                !/*gd         13*/
!       mm=6   Score the track of a dxtran contribution.                        !/*gd         14*/
!       mm=8   Score energy deposite locally.                                   !/*gd         15*/
!                                                                               !/*gd         16*/
      use global_data                                                           !/*gd1c        1*/
!                                                                               !/*gd1c        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gd1c        3*/
      implicit integer(kindi) (i-n)                                             !/*gd1c        4*/
!                                                                               !/*gd1c        5*/
!                                                                               !/*gd         18*/
      parameter (ep=1.d-15)                                                     !/*gd         19*/
!                                                                               !/*gd         20*/
      common /meshtal/ ergl(maxg),ergh(maxg),rdos(5,maxg),                      !/*gd         21*/
     & rmult(maxg),mdos(3,5,maxg),mmult(3,maxg),nemsh(maxg),                    !/*gd         22*/
     & mshtr(maxg),mstyp(maxg),mshpt(maxg),ng1(maxg),ng2(maxg),                 !/*gd         23*/
     & ng3(maxg),mxgt(maxg),ngv(maxg,maxgv),mxgc(maxg),mxgv(maxg)               !/*gd         24*/
!                                                                               !/*gd         25*/
      dimension zd(10),jz(5)                                                    !/*gd         26*/
      dimension tr(12)                                                          !/*gd         27*/
!                                                                               !/*gd         28*/
!       jz(1) = material number                                                 !/*gd         29*/
!       jz(2) = mesh tally point source flag.                                   !/*gd         30*/
!               0 = Not a new particle                                          !/*gd         31*/
!               1 = Samples from the source distribution.                       !/*gd         32*/
!               2 = Secondary particle from collision                           !/*gd         33*/
!               3 = Particle from surface source file.                          !/*gd         34*/
!       jz(3 - 4) currently not used.                                           !/*gd         35*/
!       jz(5) = energy deposition type.                                         !/*gd         36*/
!       zd(1 - 6) = x,y,z,u,v,w                                                 !/*gd         37*/
!       zd(7) = track weight                                                    !/*gd         38*/
!       zd(8) = energy at start of track.                                       !/*gd         39*/
!       zd(9) = energy lost along track or at point.                            !/*gd         40*/
!       zd(10) currently not used.                                              !/*gd         41*/
!                                                                               !/*gd         42*/
      d=0.                                                                      !/*gd         43*/
      ip=mshpt(ig)                                                              !/*gd         44*/
      jp=mod(mstyp(ig),10)                                                      !/*gd         45*/
!                                                                               !/*gd         46*/
!       ip = particle type.                                                     !/*gd         47*/
!       jp = mesh type.                                                         !/*gd         48*/
!                                                                               !/*gd         49*/
    5 f_wt=zd(7)                                                                !/*gd         50*/
      f_eg=zd(8)                                                                !/*gd         51*/
      f_ed=zd(9)                                                                !/*gd         52*/
      mk=jz(1)                                                                  !/*gd         53*/
      mt=jz(5)                                                                  !/*gd         54*/
!                                                                               !/*gd         55*/
      m1=mmult(1,ig)                                                            !/*gd         56*/
      if(m1.ne.0) then                                                          !/*gd         57*/
         m2=mmult(2,ig)                                                         !/*gd         58*/
         if(m2.eq.0)m2=1                                                        !/*gd         59*/
         m3=mmult(3,ig)                                                         !/*gd         60*/
         rx=rmult(ig)                                                           !/*gd         61*/
         if(rx.eq.0.0)rx=1.0                                                    !/*gd         62*/
      endif                                                                     !/*gd         63*/
!                                                                               !/*gd         64*/
!       See if something needs to be done for this track.                       !/*gd         65*/
!                                                                               !/*gd         66*/
      if(f_eg.gt.ergh(ig).or.f_eg.le.ergl(ig))return                            !/*gd         67*/
      if((jp.eq.0.or.jp.ge.3).and.ip.ne.ipt)return                              !/*gd         68*/
      if(mm.eq.1.and.jp.eq.1)go to 10                                           !/*gd         69*/
      if((mm.eq.2.or.mm.eq.8).and.(jp.eq.0.or.jp.eq.2))go to 10                 !/*gd         70*/
      if((mm.eq.5.and.jp.eq.3).or.(mm.eq.6.and.jp.eq.4))go to 10                !/*gd         71*/
      return                                                                    !/*gd         72*/
!                                                                               !/*gd         73*/
!       Transform to coordinate system of mesh.                                 !/*gd         74*/
   10 m=mshtr(ig)                                                               !/*gd         75*/
!                                                                               !/*gd         76*/
      if(m.eq.0)then                                                            !/*gd         77*/
         xx=zd(1)                                                               !/*gd         78*/
         yy=zd(2)                                                               !/*gd         79*/
         zz=zd(3)                                                               !/*gd         80*/
         uu=zd(4)                                                               !/*gd         81*/
         vv=zd(5)                                                               !/*gd         82*/
         ww=zd(6)                                                               !/*gd         83*/
      else                                                                      !/*gd         84*/
         call trfdat(m,tr)                                                      !/*gd         85*/
         xx=zd(1)*tr(4)+zd(2)*tr(7)+zd(3)*tr(10)+tr(1)                          !/*gd         86*/
         yy=zd(1)*tr(5)+zd(2)*tr(8)+zd(3)*tr(11)+tr(2)                          !/*gd         87*/
         zz=zd(1)*tr(6)+zd(2)*tr(9)+zd(3)*tr(12)+tr(3)                          !/*gd         88*/
         uu=zd(4)*tr(4)+zd(5)*tr(7)+zd(6)*tr(10)                                !/*gd         89*/
         vv=zd(4)*tr(5)+zd(5)*tr(8)+zd(6)*tr(11)                                !/*gd         90*/
         ww=zd(4)*tr(6)+zd(5)*tr(9)+zd(6)*tr(12)                                !/*gd         91*/
      endif                                                                     !/*gd         92*/
!                                                                               !/*gd         93*/
!       Determine the location of starting point of the track                   !/*gd         94*/
!       in each mesh for the proper particle type.                              !/*gd         95*/
!       Also score source weight if required.                                   !/*gd         96*/
!                                                                               !/*gd         97*/
      i1=0                                                                      !/*gd         98*/
      i2=0                                                                      !/*gd         99*/
      i3=0                                                                      !/*gd        100*/
      if(xx.lt.gd1(lgd1(ig)+1).or.xx.gt.gd1(lgd1(ig)+ng1(ig)).or.               !/*gd        101*/
     & yy.lt.gd2(lgd2(ig)+1).or.yy.gt.gd2(lgd2(ig)+ng2(ig)).or.                 !/*gd        102*/
     & zz.lt.gd3(lgd3(ig)+1).or.zz.gt.gd3(lgd3(ig)+ng3(ig)))                    !/*gd        103*/
     & go to 100                                                                !/*gd        104*/
!                                                                               !/*gd        105*/
!       Determine indicies of point xx,yy,zz.                                   !/*gd        106*/
!       and determine if this point is on a mesh boundary.                      !/*gd        107*/
!                                                                               !/*gd        108*/
      if(abs(xx-gd1(lgd1(ig)+1)).lt.ep)then                                     !/*gd        109*/
         if(uu.gt.0)then                                                        !/*gd        110*/
            i1=2                                                                !/*gd        111*/
            go to 40                                                            !/*gd        112*/
         else                                                                   !/*gd        113*/
            go to 400                                                           !/*gd        114*/
         endif                                                                  !/*gd        115*/
      endif                                                                     !/*gd        116*/
!                                                                               !/*gd        117*/
      if(abs(xx-gd1(lgd1(ig)+ng1(ig))).lt.ep)then                               !/*gd        118*/
         if(uu.lt.0)then                                                        !/*gd        119*/
            i1=ng1(ig)                                                          !/*gd        120*/
            go to 40                                                            !/*gd        121*/
         else                                                                   !/*gd        122*/
            go to 400                                                           !/*gd        123*/
         endif                                                                  !/*gd        124*/
      endif                                                                     !/*gd        125*/
!                                                                               !/*gd        126*/
      ic=1                                                                      !/*gd        127*/
      ib=ng1(ig)                                                                !/*gd        128*/
   20 if(ib-ic.eq.1) go to 30                                                   !/*gd        129*/
      ih=(ic+ib)/2                                                              !/*gd        130*/
      if(abs(xx-gd1(lgd1(ig)+ih)).lt.ep) go to 35                               !/*gd        131*/
      if(xx.lt.gd1(lgd1(ig)+ih)) go to 25                                       !/*gd        132*/
      ic=ih                                                                     !/*gd        133*/
      go to 20                                                                  !/*gd        134*/
   25 ib=ih                                                                     !/*gd        135*/
      go to 20                                                                  !/*gd        136*/
!                                                                               !/*gd        137*/
   30 i1=ic+1                                                                   !/*gd        138*/
      go to 40                                                                  !/*gd        139*/
!                                                                               !/*gd        140*/
   35 i1=ih                                                                     !/*gd        141*/
      if(uu.ge.0.)i1=i1+1                                                       !/*gd        142*/
!                                                                               !/*gd        143*/
   40 if(abs(yy-gd2(lgd2(ig)+1)).lt.ep) then                                    !/*gd        144*/
         if(vv.gt.0)then                                                        !/*gd        145*/
            i2=2                                                                !/*gd        146*/
            go to 70                                                            !/*gd        147*/
         else                                                                   !/*gd        148*/
            go to 400                                                           !/*gd        149*/
         endif                                                                  !/*gd        150*/
      endif                                                                     !/*gd        151*/
!                                                                               !/*gd        152*/
      if(abs(yy-gd2(lgd2(ig)+ng2(ig))).lt.ep) then                              !/*gd        153*/
        if(vv.lt.0)then                                                         !/*gd        154*/
            i2=ng2(ig)                                                          !/*gd        155*/
            go to 70                                                            !/*gd        156*/
         else                                                                   !/*gd        157*/
            go to 400                                                           !/*gd        158*/
         endif                                                                  !/*gd        159*/
      endif                                                                     !/*gd        160*/
!                                                                               !/*gd        161*/
      ic=1                                                                      !/*gd        162*/
      ib=ng2(ig)                                                                !/*gd        163*/
   50 if(ib-ic.eq.1) go to 60                                                   !/*gd        164*/
      ih=(ic+ib)/2                                                              !/*gd        165*/
      if(abs(yy-gd2(lgd2(ig)+ih)).lt.ep) go to 65                               !/*gd        166*/
      if(yy.lt.gd2(lgd2(ig)+ih)) go to 55                                       !/*gd        167*/
      ic=ih                                                                     !/*gd        168*/
      go to 50                                                                  !/*gd        169*/
   55 ib=ih                                                                     !/*gd        170*/
      go to 50                                                                  !/*gd        171*/
!                                                                               !/*gd        172*/
   60 i2=ic+1                                                                   !/*gd        173*/
      go to 70                                                                  !/*gd        174*/
!                                                                               !/*gd        175*/
   65 i2=ih                                                                     !/*gd        176*/
      if(vv.ge.0.)i2=i2+1                                                       !/*gd        177*/
!                                                                               !/*gd        178*/
   70 if(abs(zz-gd3(lgd3(ig)+1)).lt.ep) then                                    !/*gd        179*/
         if(ww.gt.0)then                                                        !/*gd        180*/
            i3=2                                                                !/*gd        181*/
            go to 93                                                            !/*gd        182*/
         else                                                                   !/*gd        183*/
            go to 400                                                           !/*gd        184*/
         endif                                                                  !/*gd        185*/
      endif                                                                     !/*gd        186*/
!                                                                               !/*gd        187*/
      if(abs(zz-gd3(lgd3(ig)+ng3(ig))).lt.ep) then                              !/*gd        188*/
         if(ww.lt.0)then                                                        !/*gd        189*/
            i3=ng3(ig)                                                          !/*gd        190*/
            go to 93                                                            !/*gd        191*/
         else                                                                   !/*gd        192*/
            go to 400                                                           !/*gd        193*/
         endif                                                                  !/*gd        194*/
      endif                                                                     !/*gd        195*/
!                                                                               !/*gd        196*/
      ic=1                                                                      !/*gd        197*/
      ib=ng3(ig)                                                                !/*gd        198*/
   80 if(ib-ic.eq.1) go to 90                                                   !/*gd        199*/
      ih=(ic+ib)/2                                                              !/*gd        200*/
      if(abs(zz-gd3(lgd3(ig)+ih)).lt.ep) go to 92                               !/*gd        201*/
      if(zz.lt.gd3(lgd3(ig)+ih)) go to 85                                       !/*gd        202*/
      ic=ih                                                                     !/*gd        203*/
      go to 80                                                                  !/*gd        204*/
   85 ib=ih                                                                     !/*gd        205*/
      go to 80                                                                  !/*gd        206*/
!                                                                               !/*gd        207*/
   90 i3=ic+1                                                                   !/*gd        208*/
      go to 93                                                                  !/*gd        209*/
!                                                                               !/*gd        210*/
   92 i3=ih                                                                     !/*gd        211*/
      if(ww.ge.0.)i3=i3+1                                                       !/*gd        212*/
!                                                                               !/*gd        213*/
!       Score source particles of the correct type or banked particles          !/*gd        214*/
!       that have not had a collision.                                          !/*gd        215*/
   93 if(mm.ne.1.and.mm.ne.8)go to 110                                          !/*gd        216*/
      do 95 i=1,mxgv(ig)                                                        !/*gd        217*/
      if(jp.eq.1.and.ngv(ig,i).ne.ipt) go to 95                                 !/*gd        218*/
      h=1.0                                                                     !/*gd        219*/
      ta=f_wt                                                                   !/*gd        220*/
      if(jp.eq.0.or.jp.eq.2) then                                               !/*gd        221*/
         if(jp.eq.0.and.ngv(ig,i).ne.4) go to 95                                !/*gd        222*/
         if(jp.eq.2) then                                                       !/*gd        223*/
            if(ngv(ig,i).ne.1.and.ngv(ig,i).ne.mt) go to 95                     !/*gd        224*/
            if(ipt.eq.2.and.kpt(3).ne.0) go to 95                               !/*gd        225*/
         endif                                                                  !/*gd        226*/
         lx=lgd1(ig)+i1                                                         !/*gd        227*/
         ly=lgd2(ig)+i2                                                         !/*gd        228*/
         lz=lgd3(ig)+i3                                                         !/*gd        229*/
      if((i1.le.1.or.i2.le.1.or.i3.le.1).and.lfatl.eq.0)call expirx             !/*gd1c        6*/
     & (1,'gd1tal','gd array out of bounds, err=1.')                            !/*gd1c        7*/
         vo=abs((gd1(lx)-gd1(lx-1))*                                            !/*gd        230*/
     &          (gd2(ly)-gd2(ly-1))*                                            !/*gd        231*/
     &          (gd3(lz)-gd3(lz-1)))                                            !/*gd        232*/
         if(vo.le.0.) then                                                      !/*gd        233*/
            call expirx(1,'gd1tal',                                             !/*gd1c        8*/
     &       'Detected a zero or negative mesh volume!')                        !/*gd        235*/
            return                                                              !/*gd        236*/
         endif                                                                  !/*gd        237*/
         ta=ta/vo                                                               !/*gd        238*/
         h=f_ed                                                                 !/*gd        239*/
         if (m1.ne.0 .and. m1.ne.-1) then                                       !/*gd        240*/
            eg=f_eg                                                             !/*gd        241*/
            if(m3.eq.1)eg=h*ta                                                  !/*gd        242*/
            if(m3.eq.2)eg=h*ta*f_eg                                             !/*gd        243*/
            ta=ta*rfact(m1,m2,m3,rx,eg,nemsh(m1))                               !/*gd        244*/
         elseif(m1.eq.-1) then                                                  !/*gd        245*/
            ta=ta*rx                                                            !/*gd        246*/
         endif                                                                  !/*gd        247*/
      endif                                                                     !/*gd        248*/
!                                                                               !/*gd        249*/
      mx=(i-1)*mxgc(ig)+(i3-2)*(ng1(ig)-1)*(ng2(ig)-1)+                         !/*gd        250*/
     & (i2-2)*(ng1(ig)-1)+i1-1                                                  !/*gd        251*/
      gdata(ktgd(ig)+mx)=gdata(ktgd(ig)+mx)+ta*h                                !/*gdxk        1*/
   95 continue                                                                  !/*gd        253*/
!                                                                               !/*gd        254*/
  100 continue                                                                  !/*gd        255*/
      if(mm.eq.1.or.mm.eq.8)return                                              !/*gd        256*/
!                                                                               !/*gd        257*/
!       Track the particle a distance 'da' in all meshs and score               !/*gd        258*/
!       tally data as required.                                                 !/*gd        259*/
!                                                                               !/*gd        260*/
  110 ds=da                                                                     !/*gd        261*/
      x=xx                                                                      !/*gd        262*/
      y=yy                                                                      !/*gd        263*/
      z=zz                                                                      !/*gd        264*/
      if(i1*i2*i3.gt.0)go to 300                                                !/*gd        265*/
!                                                                               !/*gd        266*/
!       The point is not in this mesh.  Can it enter this mesh within           !/*gd        267*/
!       a distance 'da', and if so where?                                       !/*gd        268*/
!                                                                               !/*gd        269*/
      xl=gd1(lgd1(ig)+1)                                                        !/*gd        270*/
      xh=gd1(lgd1(ig)+ng1(ig))                                                  !/*gd        271*/
      yl=gd2(lgd2(ig)+1)                                                        !/*gd        272*/
      yh=gd2(lgd2(ig)+ng2(ig))                                                  !/*gd        273*/
      zl=gd3(lgd3(ig)+1)                                                        !/*gd        274*/
      zh=gd3(lgd3(ig)+ng3(ig))                                                  !/*gd        275*/
!                                                                               !/*gd        276*/
      if((uu.ge.0..and.x.ge.xh).or.(uu.le.0..and.x.le.xl))go to 400             !/*gd        277*/
      if((vv.ge.0..and.y.ge.yh).or.(vv.le.0..and.y.le.yl))go to 400             !/*gd        278*/
      if((ww.ge.0..and.z.ge.zh).or.(ww.le.0..and.z.le.zl))go to 400             !/*gd        279*/
!                                                                               !/*gd        280*/
      if(x.lt.xh.and.x.gt.xl)go to 160                                          !/*gd        281*/
      dx=huge                                                                   !/*gd        282*/
      if(uu.ne.0.) then                                                         !/*gd        283*/
         dx=(xl-x)/uu                                                           !/*gd        284*/
         if(x.ge.xh)dx=(xh-x)/uu                                                !/*gd        285*/
      endif                                                                     !/*gd        286*/
      if(dx.ge.ds)go to 400                                                     !/*gd        287*/
      y1=y+dx*vv                                                                !/*gd        288*/
      z1=z+dx*ww                                                                !/*gd        289*/
      if(y1.ge.yh.or.y1.le.yl.or.z1.ge.zh.or.z1.le.zl)go to 400                 !/*gd        290*/
      if(x.ge.xh) then                                                          !/*gd        291*/
         i1=ng1(ig)                                                             !/*gd        292*/
         x=xh                                                                   !/*gd        293*/
      else                                                                      !/*gd        294*/
         i1=2                                                                   !/*gd        295*/
         x=xl                                                                   !/*gd        296*/
      endif                                                                     !/*gd        297*/
!                                                                               !/*gd        298*/
      ic=1                                                                      !/*gd        299*/
      ib=ng2(ig)                                                                !/*gd        300*/
  120 if(ib-ic.eq.1) go to 130                                                  !/*gd        301*/
      ih=(ic+ib)/2                                                              !/*gd        302*/
      if(y1.lt.gd2(lgd2(ig)+ih)) go to 125                                      !/*gd        303*/
      ic=ih                                                                     !/*gd        304*/
      go to 120                                                                 !/*gd        305*/
  125 ib=ih                                                                     !/*gd        306*/
      go to 120                                                                 !/*gd        307*/
!                                                                               !/*gd        308*/
  130 i2=ic+1                                                                   !/*gd        309*/
      y=y1                                                                      !/*gd        310*/
!                                                                               !/*gd        311*/
      ic=1                                                                      !/*gd        312*/
      ib=ng3(ig)                                                                !/*gd        313*/
  140 if(ib-ic.eq.1) go to 150                                                  !/*gd        314*/
      ih=(ic+ib)/2                                                              !/*gd        315*/
      if(z1.lt.gd3(lgd3(ig)+ih)) go to 145                                      !/*gd        316*/
      ic=ih                                                                     !/*gd        317*/
      go to 140                                                                 !/*gd        318*/
  145 ib=ih                                                                     !/*gd        319*/
      go to 140                                                                 !/*gd        320*/
!                                                                               !/*gd        321*/
  150 i3=ic+1                                                                   !/*gd        322*/
      z=z1                                                                      !/*gd        323*/
!                                                                               !/*gd        324*/
      ds=ds-dx                                                                  !/*gd        325*/
      go to 300                                                                 !/*gd        326*/
!                                                                               !/*gd        327*/
  160 if(y.lt.yh.and.y.gt.yl)go to 210                                          !/*gd        328*/
      dy=huge                                                                   !/*gd        329*/
      if(vv.ne.0.) then                                                         !/*gd        330*/
         dy=(yl-y)/vv                                                           !/*gd        331*/
         if(y.ge.yh)dy=(yh-y)/vv                                                !/*gd        332*/
      endif                                                                     !/*gd        333*/
      if(dy.ge.ds)go to 400                                                     !/*gd        334*/
      x1=x+dy*uu                                                                !/*gd        335*/
      z1=z+dy*ww                                                                !/*gd        336*/
      if(x1.ge.xh.or.x1.le.xl.or.z1.ge.zh.or.z1.le.zl)go to 400                 !/*gd        337*/
      if(y.ge.yh) then                                                          !/*gd        338*/
         i2=ng2(ig)                                                             !/*gd        339*/
         y=yh                                                                   !/*gd        340*/
      else                                                                      !/*gd        341*/
         i2=2                                                                   !/*gd        342*/
         y=yl                                                                   !/*gd        343*/
      endif                                                                     !/*gd        344*/
!                                                                               !/*gd        345*/
      ic=1                                                                      !/*gd        346*/
      ib=ng1(ig)                                                                !/*gd        347*/
  170 if(ib-ic.eq.1) go to 180                                                  !/*gd        348*/
      ih=(ic+ib)/2                                                              !/*gd        349*/
      if(x1.lt.gd1(lgd1(ig)+ih)) go to 175                                      !/*gd        350*/
      ic=ih                                                                     !/*gd        351*/
      go to 170                                                                 !/*gd        352*/
  175 ib=ih                                                                     !/*gd        353*/
      go to 170                                                                 !/*gd        354*/
!                                                                               !/*gd        355*/
  180 i1=ic+1                                                                   !/*gd        356*/
      x=x1                                                                      !/*gd        357*/
!                                                                               !/*gd        358*/
      ic=1                                                                      !/*gd        359*/
      ib=ng3(ig)                                                                !/*gd        360*/
  190 if(ib-ic.eq.1) go to 200                                                  !/*gd        361*/
      ih=(ic+ib)/2                                                              !/*gd        362*/
      if(z1.lt.gd3(lgd3(ig)+ih)) go to 195                                      !/*gd        363*/
      ic=ih                                                                     !/*gd        364*/
      go to 190                                                                 !/*gd        365*/
  195 ib=ih                                                                     !/*gd        366*/
      go to 190                                                                 !/*gd        367*/
!                                                                               !/*gd        368*/
  200 i3=ic+1                                                                   !/*gd        369*/
      z=z1                                                                      !/*gd        370*/
!                                                                               !/*gd        371*/
      ds=ds-dy                                                                  !/*gd        372*/
      go to 300                                                                 !/*gd        373*/
!                                                                               !/*gd        374*/
  210 if(z.lt.zh.and.z.gt.zl)go to 400                                          !/*gd        375*/
      dz=huge                                                                   !/*gd        376*/
      if(ww.ne.0.) then                                                         !/*gd        377*/
         dz=(zl-z)/ww                                                           !/*gd        378*/
         if(z.ge.zh)dz=(zh-z)/ww                                                !/*gd        379*/
      endif                                                                     !/*gd        380*/
      if(dz.ge.ds)go to 400                                                     !/*gd        381*/
      x1=x+dz*uu                                                                !/*gd        382*/
      y1=y+dz*vv                                                                !/*gd        383*/
      if(x1.ge.xh.or.x1.le.xl.or.y1.ge.yh.or.y1.le.yl)go to 400                 !/*gd        384*/
      if(z.ge.zh) then                                                          !/*gd        385*/
         i3=ng3(ig)                                                             !/*gd        386*/
         z=zh                                                                   !/*gd        387*/
      else                                                                      !/*gd        388*/
         i3=2                                                                   !/*gd        389*/
         z=zl                                                                   !/*gd        390*/
      endif                                                                     !/*gd        391*/
!                                                                               !/*gd        392*/
      ic=1                                                                      !/*gd        393*/
      ib=ng1(ig)                                                                !/*gd        394*/
  220 if(ib-ic.eq.1) go to 230                                                  !/*gd        395*/
      ih=(ic+ib)/2                                                              !/*gd        396*/
      if(x1.lt.gd1(lgd1(ig)+ih)) go to 225                                      !/*gd        397*/
      ic=ih                                                                     !/*gd        398*/
      go to 220                                                                 !/*gd        399*/
  225 ib=ih                                                                     !/*gd        400*/
      go to 220                                                                 !/*gd        401*/
!                                                                               !/*gd        402*/
  230 i1=ic+1                                                                   !/*gd        403*/
      x=x1                                                                      !/*gd        404*/
!                                                                               !/*gd        405*/
      ic=1                                                                      !/*gd        406*/
      ib=ng2(ig)                                                                !/*gd        407*/
  240 if(ib-ic.eq.1) go to 250                                                  !/*gd        408*/
      ih=(ic+ib)/2                                                              !/*gd        409*/
      if(y1.lt.gd2(lgd2(ig)+ih)) go to 245                                      !/*gd        410*/
      ic=ih                                                                     !/*gd        411*/
      go to 240                                                                 !/*gd        412*/
  245 ib=ih                                                                     !/*gd        413*/
      go to 240                                                                 !/*gd        414*/
!                                                                               !/*gd        415*/
  250 i2=ic+1                                                                   !/*gd        416*/
      y=y1                                                                      !/*gd        417*/
!                                                                               !/*gd        418*/
      ds=ds-dz                                                                  !/*gd        419*/
!                                                                               !/*gd        420*/
!       Track the particle in this mesh until the distance 'ds'                 !/*gd        421*/
!       is exhausted or the particle leaves this mesh.                          !/*gd        422*/
  300 lx=lgd1(ig)+i1                                                            !/*gd        423*/
      ly=lgd2(ig)+i2                                                            !/*gd        424*/
      lz=lgd3(ig)+i3                                                            !/*gd        425*/
      if((i1.le.1.or.i2.le.1.or.i3.le.1).and.lfatl.eq.0)call expirx             !/*gd1c        9*/
     & (1,'gd1tal','gd array out of bounds, err=2.')                            !/*gd1c       10*/
!                                                                               !/*gd        426*/
      dx=huge                                                                   !/*gd        427*/
      if(uu.ne.0.) then                                                         !/*gd        428*/
         d1=(gd1(lx)-x)/uu                                                      !/*gd        429*/
         if(d1.le.ep)d1=huge                                                    !/*gd        430*/
         d2=(gd1(lx-1)-x)/uu                                                    !/*gd        431*/
         if(d2.le.ep)d2=huge                                                    !/*gd        432*/
         dx=min(d1,d2)                                                          !/*gd        433*/
      endif                                                                     !/*gd        434*/
!                                                                               !/*gd        435*/
      dy=huge                                                                   !/*gd        436*/
      if(vv.ne.0.) then                                                         !/*gd        437*/
        d1=(gd2(ly)-y)/vv                                                       !/*gd        438*/
        if(d1.le.ep)d1=huge                                                     !/*gd        439*/
        d2=(gd2(ly-1)-y)/vv                                                     !/*gd        440*/
        if(d2.le.ep)d2=huge                                                     !/*gd        441*/
        dy=min(d1,d2)                                                           !/*gd        442*/
      endif                                                                     !/*gd        443*/
!                                                                               !/*gd        444*/
      dz=huge                                                                   !/*gd        445*/
      if(ww.ne.0.) then                                                         !/*gd        446*/
        d1=(gd3(lz)-z)/ww                                                       !/*gd        447*/
        if(d1.le.ep)d1=huge                                                     !/*gd        448*/
        d2=(gd3(lz-1)-z)/ww                                                     !/*gd        449*/
        if(d2.le.ep)d2=huge                                                     !/*gd        450*/
        dz=min(d1,d2)                                                           !/*gd        451*/
      endif                                                                     !/*gd        452*/
!                                                                               !/*gd        453*/
      d=min(dx,dy,dz,ds)                                                        !/*gd        454*/
      dm=d                                                                      !/*gd        455*/
      if(d.le.ep) then                                                          !/*gd        456*/
         dm=ep                                                                  !/*gd        457*/
         go to 345                                                              !/*gd        458*/
      endif                                                                     !/*gd        459*/
!                                                                               !/*gd        460*/
      vo=abs((gd1(lx)-gd1(lx-1))*                                               !/*gd        461*/
     &       (gd2(ly)-gd2(ly-1))*                                               !/*gd        462*/
     &       (gd3(lz)-gd3(lz-1)))                                               !/*gd        463*/
      if(vo.le.0.)call expirx(1,'gd1tal',                                       !/*gd1c       11*/
     & 'Detected a zero or negative mesh volume!')                              !/*gd        465*/
!                                                                               !/*gd        466*/
  305 jj=0                                                                      !/*gd        467*/
      fc=0.                                                                     !/*gd        468*/
      if(da.gt.0.)fc=d/da                                                       !/*gd        469*/
      do 340 i=1,mxgv(ig)                                                       !/*gd        470*/
      h=1.0                                                                     !/*gd        471*/
      if(jp.eq.1) then                                                          !/*gd        472*/
         if(ipt.ne.ngv(ig,i))go to 340                                          !/*gd        473*/
         ta=f_wt                                                                !/*gd        474*/
         go to 335                                                              !/*gd        475*/
      endif                                                                     !/*gd        476*/
      if(jp.eq.2) then                                                          !/*gd        477*/
         if(ngv(ig,i).ne.1.and.ngv(ig,i).ne.mt)go to 340                        !/*gd        478*/
         go to 315                                                              !/*gd        479*/
      endif                                                                     !/*gd        480*/
      if(jp.ge.3)go to 310                                                      !/*gd        481*/
      go to (310,308,314,315,312)ngv(ig,i)                                      !/*gd        482*/
!                                                                               !/*gd        483*/
  308 ta=1                                                                      !/*gd        484*/
      go to 333                                                                 !/*gd        485*/
!                                                                               !/*gd        486*/
  310 ta=d*f_wt/vo                                                              !/*gd        487*/
      go to 333                                                                 !/*gd        488*/
!                                                                               !/*gd        489*/
  312 jj=jj+1                                                                   !/*gd        490*/
      n1=mdos(1,jj,ig)                                                          !/*gd        491*/
      if(n1.eq.0)n1=10                                                          !/*gd        492*/
      n2=mdos(2,jj,ig)                                                          !/*gd        493*/
      if(n2.eq.0)n2=1                                                           !/*gd        494*/
      n3=mdos(3,jj,ig)                                                          !/*gd        495*/
      if(n3.eq.0)n3=1                                                           !/*gd        496*/
      rm=rdos(jj,ig)                                                            !/*gd        497*/
      if(rm.eq.0.0)rm=1.0                                                       !/*gd        498*/
      if(n1.ge.10) then                                                         !/*gd        499*/
         ft=dfact(ipt,n1,f_eg,n2,n3,rm)                                         !/*gd        500*/
      else                                                                      !/*gd        501*/
         ft=rfact(n1,n2,n3,rm,f_eg,nemsh(n1))                                   !/*gd        502*/
      endif                                                                     !/*gd        503*/
      ta=ft*d*f_wt/vo                                                           !/*gd        504*/
      go to 335                                                                 !/*gd        505*/
!                                                                               !/*gd        506*/
  314 ta=d*f_wt                                                                 !/*gd        507*/
      go to 333                                                                 !/*gd        508*/
!                                                                               !/*gd        509*/
  315 ta=f_wt/vo                                                                !/*gd        510*/
      if(ipt.eq.1.and.erg.gt.cut_n)go to 340                                    !/*gd        511*/
      if(mk.eq.0.or.(jgp.lt.0.and.ipt.eq.2))go to 340                           !/*gd        512*/
      if(jp.eq.2.and.ipt.eq.2.and.                                              !/*gd        513*/
     & (jgp.lt.0.or.kpt(3).ne.0))go to 340                                      !/*gd        514*/
!                                                                               !/*gd        515*/
!       Particle energy deposition.                                             !/*gd        516*/
  330 h=fc*f_ed                                                                 !/*gd        517*/
      f_eg=f_eg-h                                                               !/*gd        518*/
!                                                                               !/*gd        519*/
!       Energy-dependent multiplier function.                                   !/*gd        520*/
  333 if (m1.ne.0 .and. m1.ne.-1) then                                          !/*gd        521*/
         eg=f_eg                                                                !/*gd        522*/
         if(m3.eq.1)eg=h*ta                                                     !/*gd        523*/
         if(m3.eq.2)eg=h*ta*f_eg                                                !/*gd        524*/
         ta=ta*rfact(m1,m2,m3,rx,eg,nemsh(m1))                                  !/*gd        525*/
      elseif(m1.eq.-1) then                                                     !/*gd        526*/
         ta=ta*rx                                                               !/*gd        527*/
      endif                                                                     !/*gd        528*/
!                                                                               !/*gd        529*/
  335 if(jp.eq.0) then                                                          !/*gd        530*/
         il=-1                                                                  !/*gd        531*/
         do 336 k=1,ntal+mshfm                                                  !/*gd        532*/
         if(nugd(ig).eq.jptal(ljpt+1,k)) then                                   !/*gd        533*/
            il=k                                                                !/*gd        534*/
         endif                                                                  !/*gd        535*/
  336    continue                                                               !/*gd        536*/
         if(il.lt.0)go to 337                                                   !/*gd        537*/
!                                                                               !/*gd        538*/
         if(iptal(lipt+5,2,il).ne.0) then                                       !/*gd        539*/
            ital=il                                                             !/*gd        540*/
            if(iptal(lipt+5,1,il).ne.0)call wtmult(ta,1)                        !/*gd        541*/
            ta=ta*tds(iptal(lipt+5,2,il)+1)                                     !/*gd        542*/
            if(tds(iptal(lipt+5,2,il)+1).lt.0.)ta=-ta*rho(lrho+icl)             !/*gd        543*/
         endif                                                                  !/*gd        544*/
!                                                                               !/*gd        545*/
         if(iptal(lipt+3,2,il).ne.0) then                                       !/*gd        546*/
            ital=il                                                             !/*gd        547*/
            ta=dosef(ta)                                                        !/*gd        548*/
         endif                                                                  !/*gd        549*/
      endif                                                                     !/*gd        550*/
!                                                                               !/*gd        551*/
  337 mx=(i-1)*mxgc(ig)+(i3-2)*(ng1(ig)-1)*(ng2(ig)-1)+                         !/*gd        552*/
     & (i2-2)*(ng1(ig)-1)+i1-1                                                  !/*gd        553*/
      gdata(ktgd(ig)+mx)=gdata(ktgd(ig)+mx)+ta*h                                !/*gdxk        2*/
!                                                                               !/*gd        555*/
  340 continue                                                                  !/*gd        556*/
!                                                                               !/*gd        557*/
!       Move a distance 'd', advance the indicies if required,                  !/*gd        558*/
!       and continue tracking until 'ds'=0 or particle exits mesh.              !/*gd        559*/
  345 ds=ds-d                                                                   !/*gd        560*/
      if(ds.le.ep)go to 400                                                     !/*gd        561*/
      if(d.eq.dx) then                                                          !/*gd        562*/
         i1=i1+1                                                                !/*gd        563*/
         if(uu.lt.0.)i1=i1-2                                                    !/*gd        564*/
         if(i1.eq.1.or.i1.gt.ng1(ig))go to 400                                  !/*gd        565*/
      elseif(d.eq.dy) then                                                      !/*gd        566*/
         i2=i2+1                                                                !/*gd        567*/
         if(vv.lt.0.)i2=i2-2                                                    !/*gd        568*/
         if(i2.eq.1.or.i2.gt.ng2(ig))go to 400                                  !/*gd        569*/
      elseif(d.eq.dz) then                                                      !/*gd        570*/
         i3=i3+1                                                                !/*gd        571*/
         if(ww.lt.0.)i3=i3-2                                                    !/*gd        572*/
         if(i3.eq.1.or.i3.gt.ng3(ig))go to 400                                  !/*gd        573*/
      endif                                                                     !/*gd        574*/
!                                                                               !/*gd        575*/
  350 x=x+d*uu                                                                  !/*gd        576*/
      y=y+d*vv                                                                  !/*gd        577*/
      z=z+d*ww                                                                  !/*gd        578*/
      go to 300                                                                 !/*gd        579*/
!                                                                               !/*gd        580*/
  400 continue                                                                  !/*gd        581*/
!                                                                               !/*gd        582*/
#endif                                                                          /*!gd        583*/
      return                                                                    !/*gd        584*/
      end                                                                       !/*gd        585*/
