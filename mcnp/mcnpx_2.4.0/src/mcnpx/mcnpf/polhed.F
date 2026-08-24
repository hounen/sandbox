!#ident polhed.F from deck pd                                                   !/*pd          1*/
      subroutine polhed(km,ix,ie)                                               !/*pd          2*/
!        calculate the volume and/or areas of the cell as a polyhedron.         !/*pd          3*/
!        km=1 for a cell, 2 for a segment volume, 3 for a segment area.         !/*pd          4*/
!        ix is the index of atsa where segment result is to be stored.          !/*pd          5*/
!        returns ie=1 for success, 0 if not polyhedron, -1 if infinite.         !/*pd          6*/
      use global_data                                                           !/*pdx         1*/
      use imcn_data                                                             !/*pdx         2*/
!                                                                               !/*pdx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*pdx         4*/
      implicit integer(kindi) (i-n)                                             !/*pdx         5*/
!                                                                               !/*pdx         6*/
      dimension cc(4),dd(4),rs(4),rg(2,2),af(2),ta(2),ip(16),im(2),             !/*pd          9*/
     & iv(2),mq(2),mt(2)                                                        !/*pd         10*/
      data ip/0,-1,1,0,0,0,1,1,0,-1,0,-1,0,0,0,0/                               !/*pd         11*/
!                                                                               !/*pd         12*/
!        return with ie=0 if the cell is not a polyhedron.                      !/*pd         13*/
      do 10 j=1,nsv                                                             !/*pd         14*/
   10 if(kst(lkst+ljsv(llsv+j)).gt.4)return                                     !/*pd4a        1*/
      if(dbcn(7).ne.0.)write(iuo,20)                                            !/*pd         16*/
   20 format(16h polyhedral cell)                                               !/*pd         17*/
!                                                                               !/*pd         18*/
!        calculate the volume as a sum over the surfaces.                       !/*pd         19*/
      ie=1                                                                      !/*pd         20*/
      vt=0.                                                                     !/*pd         21*/
      mq(1)=0                                                                   !/*pd         22*/
      mq(2)=0                                                                   !/*pd         23*/
      do 370 ns=1,nsv                                                           !/*pd         24*/
      jt=ljsv(llsv+ns)                                                          !/*pd         25*/
      if(km.eq.3.and.jt.ne.jsu)go to 370                                        !/*pd         26*/
!                                                                               !/*pd         27*/
!        set up the bbb transformation matrix for the facet surface.            !/*pd         28*/
      kk=kst(lkst+jt)                                                           !/*pd4a        2*/
      do 30 i=1,4                                                               !/*pd         30*/
      do 30 j=1,4                                                               !/*pd         31*/
   30 bbb(i,j)=0.                                                               !/*pd         32*/
      bbb(1,1)=1.                                                               !/*pd         33*/
      if(kk.ne.1)go to 60                                                       !/*pd         34*/
      do 40 i=1,3                                                               !/*pd         35*/
   40 bbb(1+i,4)=scf(lsc(llsc+jt)+i)                                            !/*pd         36*/
      t1=1./sqrt(bbb(2,4)**2+bbb(3,4)**2+bbb(4,4)**2)                           !/*pd         37*/
      do 50 i=2,4                                                               !/*pd         38*/
      bbb(i,4)=bbb(i,4)*t1                                                      !/*pd         39*/
   50 bbb(i,1)=bbb(i,4)*scf(lsc(llsc+jt)+4)*t1                                  !/*pd         40*/
      go to 70                                                                  !/*pd         41*/
   60 bbb(kk,4)=1.                                                              !/*pd         42*/
      bbb(kk,1)=scf(lsc(llsc+jt)+1)                                             !/*pd         43*/
   70 call arbobv(bbb(2,4),bbb(2,2),bbb(2,3),i)                                 !/*pd         44*/
      if(dbcn(7).ne.0.)write(iuo,80)nsf(lnsf+jt),                               !/*pd         45*/
     & ((bbb(i,j),j=1,4),i=1,4)                                                 !/*pd         46*/
   80 format(/8h surface,i7,2x,5hbbb =,1p4e20.12/1x,12(1h=),9x,4e20.12/         !/*pd         47*/
     & 2(22x,4e20.12/)/42h q-form coefficients of the other surfaces)           !/*pd         48*/
!                                                                               !/*pd         49*/
!        put the other surfaces into q-form.                                    !/*pd         50*/
      do 130 jl=1,nsv                                                           !/*pd         51*/
      if(jl.eq.ns)go to 130                                                     !/*pd         52*/
      kt=ljsv(llsv+jl)                                                          !/*pd         53*/
      kk=kst(lkst+kt)                                                           !/*pd4a        3*/
      if(kk.ne.1)go to 100                                                      !/*pd         55*/
      do 90 i=1,3                                                               !/*pd         56*/
   90 cc(1+i)=scf(lsc(llsc+kt)+i)                                               !/*pd         57*/
      cc(1)=-scf(lsc(llsc+kt)+4)                                                !/*pd         58*/
      go to 110                                                                 !/*pd         59*/
  100 cc(1)=-scf(lsc(llsc+kt)+1)                                                !/*pd         60*/
      cc(2)=0.                                                                  !/*pd         61*/
      cc(3)=0.                                                                  !/*pd         62*/
      cc(4)=0.                                                                  !/*pd         63*/
      cc(kk)=1.                                                                 !/*pd         64*/
  110 call matmpy(1,4,4,cc,1,bbb,4,dd,1,0)                                      !/*pd         65*/
      scfq(lscq+1,kt)=0.                                                        !/*pd         66*/
      scfq(lscq+3,kt)=0.                                                        !/*pd         67*/
      scfq(lscq+2,kt)=dd(3)                                                     !/*pd         68*/
      scfq(lscq+4,kt)=dd(2)                                                     !/*pd         69*/
      scfq(lscq+5,kt)=dd(1)                                                     !/*pd         70*/
      if(dbcn(7).ne.0.)write(iuo,120)nsf(lnsf+kt),                              !/*pd         71*/
     & (scfq(lscq+i,kt),i=1,5)                                                  !/*pd         72*/
  120 format(i7,1p5e14.5)                                                       !/*pd         73*/
  130 continue                                                                  !/*pd         74*/
!                                                                               !/*pd         75*/
!        calculate the area and the center of area of the facet surface.        !/*pd         76*/
      do 140 i=1,2                                                              !/*pd         77*/
      af(i)=0.                                                                  !/*pd         78*/
      rg(i,1)=0.                                                                !/*pd         79*/
      rg(i,2)=0.                                                                !/*pd         80*/
  140 ta(i)=0.                                                                  !/*pd         81*/
!                                                                               !/*pd         82*/
!        do all of the other surfaces of the cell.                              !/*pd         83*/
      do 300 jl=1,nsv                                                           !/*pd         84*/
      if(jl.eq.ns)go to 300                                                     !/*pd         85*/
      kt=ljsv(llsv+jl)                                                          !/*pd         86*/
      tk=sqrt(scfq(lscq+2,kt)**2+scfq(lscq+4,kt)**2)                            !/*pd         87*/
      if(tk.eq.0.)go to 300                                                     !/*pd         88*/
      call intsec(kt,1,1,ni,ns,i)                                               !/*pd         89*/
!                                                                               !/*pd         90*/
!        do all of the sections of the current other surface.                   !/*pd         91*/
      np=ni+1                                                                   !/*pd         92*/
      im(1)=1                                                                   !/*pd         93*/
      im(2)=1                                                                   !/*pd         94*/
      do 290 ir=1,np                                                            !/*pd         95*/
!                                                                               !/*pd         96*/
!        set up the coordinates of the ends of the section.                     !/*pd         97*/
      rs(1)=rsint(lrsn+1,ir)                                                    !/*pdx         7*/
      rs(2)=rsint(lrsn+2,ir)                                                    !/*pdx         8*/
      rs(3)=rsint(lrsn+1,ir+1)                                                  !/*pdx         9*/
      rs(4)=rsint(lrsn+2,ir+1)                                                  !/*pdx        10*/
      if(np.ne.1)go to 160                                                      !/*pd        100*/
      rs(2)=-scfq(lscq+5,kt)*scfq(lscq+4,kt)/tk                                 !/*pd        101*/
      rs(1)=-scfq(lscq+5,kt)*scfq(lscq+2,kt)/tk                                 !/*pd        102*/
      go to 170                                                                 !/*pd        103*/
  160 if(ir.ne.1)go to 170                                                      !/*pd        104*/
      rs(2)=rs(4)-abs(scfq(lscq+2,kt))                                          !/*pd        105*/
      rs(1)=rs(3)+scfq(lscq+4,kt)*sign(one,scfq(lscq+2,kt))                     !/*pd        106*/
  170 if(ir.ne.np)go to 180                                                     !/*pd        107*/
      rs(4)=rs(2)+abs(scfq(lscq+2,kt))                                          !/*pd        108*/
      rs(3)=rs(1)-scfq(lscq+4,kt)*sign(one,scfq(lscq+2,kt))                     !/*pd        109*/
!                                                                               !/*pd        110*/
!        check whether the section bounds the facet.                            !/*pd        111*/
  180 iv(1)=0                                                                   !/*pd        112*/
      iv(2)=0                                                                   !/*pd        113*/
      r=.51729*rs(1)+.48271*rs(3)                                               !/*pd        114*/
      s=.51729*rs(2)+.48271*rs(4)                                               !/*pd        115*/
      do 190 i=1,nsjv                                                           !/*pd        116*/
      lgc(i)=ljav(llav+i)                                                       !/*pd        117*/
      if(lgc(i).gt.1000000)go to 190                                            !/*pd        118*/
      j=abs(lgc(i))                                                             !/*pd        119*/
      lgc(i)=1                                                                  !/*pd        120*/
      if(j.eq.jt.or.j.eq.kt)go to 190                                           !/*pd        121*/
      t=scfq(lscq+2,j)*r+scfq(lscq+4,j)*s+scfq(lscq+5,j)                        !/*pd        122*/
      if(ljav(llav+i)*t.lt.0.)lgc(i)=0                                          !/*pd        123*/
  190 continue                                                                  !/*pd        124*/
      if(lgeval(lgc,nsjv).eq.0)go to 240                                        !/*pd        125*/
      do 230 j=1,2                                                              !/*pd        126*/
      do 200 i=1,nsjv                                                           !/*pd        127*/
      if(ljav(llav+i).eq.jt)lgc(i)=j-1                                          !/*pd        128*/
  200 if(ljav(llav+i).eq.-jt)lgc(i)=2-j                                         !/*pd        129*/
      do 220 ks=1,2                                                             !/*pd        130*/
      do 210 i=1,nsjv                                                           !/*pd        131*/
      if(ljav(llav+i).eq.kt)lgc(i)=ks-1                                         !/*pd        132*/
  210 if(ljav(llav+i).eq.-kt)lgc(i)=2-ks                                        !/*pd        133*/
      if(lgeval(lgc,nsjv).eq.0)go to 220                                        !/*pd        134*/
      iv(1)=iv(1)+2**(ks-3+2*j)                                                 !/*pd        135*/
      iv(2)=iv(2)+2**(ks+3-2*j)                                                 !/*pd        136*/
  220 continue                                                                  !/*pd        137*/
  230 continue                                                                  !/*pd        138*/
!                                                                               !/*pd        139*/
!        do both sides of the facet surface.                                    !/*pd        140*/
  240 do 280 ms=1,2                                                             !/*pd        141*/
      mt(ms)=ip(iv(ms)+1)                                                       !/*pd        142*/
      if(mt(ms).eq.0.or.im(ms).ne.ir.and.mt(ms).ne.mq(ms))go to 250             !/*pd        143*/
      mq(ms)=mt(ms)                                                             !/*pd        144*/
      go to 280                                                                 !/*pd        145*/
!                                                                               !/*pd        146*/
!        calculate the area and the moment of the section.                      !/*pd        147*/
  250 if(im(ms).eq.ir)go to 270                                                 !/*pd        148*/
      af(ms)=af(ms)-mq(ms)*(rsint(lrsn+2,ir)-rsint(lrsn+2,im(ms)))*             !/*pd        149*/
     & (rsint(lrsn+1,ir)+rsint(lrsn+1,im(ms)))*                                 !/*pd        150*/
     & sign(zero+.5,scfq(lscq+2,kt))                                            !/*pd        151*/
      do 260 i=1,2                                                              !/*pd        152*/
  260 rg(ms,i)=rg(ms,i)-abs(rsint(lrsn+3-i,ir)-rsint(lrsn+3-i,im(ms)))*         !/*pd        153*/
     & (rsint(lrsn+i,ir)**2+rsint(lrsn+i,ir)*rsint(lrsn+i,im(ms))+              !/*pd        154*/
     & rsint(lrsn+i,im(ms))**2)*sign(.5*third,scfq(lscq+2*i,kt))*mq(ms)         !/*pd        155*/
  270 im(ms)=ir+1                                                               !/*pd        156*/
  280 if(mt(ms).ne.0.and.(ir.eq.1.or.ir.eq.np))go to 310                        !/*pd        157*/
  290 continue                                                                  !/*pd        158*/
  300 continue                                                                  !/*pd        159*/
      if(nsv.gt.1)go to 330                                                     !/*pd        160*/
!                                                                               !/*pd        161*/
!        set ie=-1 if the cell is infinite.                                     !/*pd        162*/
  310 ie=-1                                                                     !/*pd        163*/
      if(dbcn(7).ne.0.)write(iuo,320)nsf(lnsf+kt)                               !/*pd        164*/
  320 format(3x,7hsurface,i7,13h is infinite.)                                  !/*pd        165*/
      if(km.ne.1)return                                                         !/*pd        166*/
      vols(lvls+1,icl)=-1.                                                      !/*pd        167*/
      aras(lars+1,jt)=-1.                                                       !/*pd        168*/
      aras(lars+2,jt)=-1.                                                       !/*pd        169*/
      go to 370                                                                 !/*pd        170*/
!                                                                               !/*pd        171*/
!        add in the volume and the area of the facet surface.                   !/*pd        172*/
  330 vq=bbb(4,4)*(bbb(4,1)*(af(1)-af(2))+bbb(4,2)*(rg(1,2)-rg(2,2))+           !/*pd        173*/
     & bbb(4,3)*(rg(1,1)-rg(2,1)))                                              !/*pd        174*/
      if(km.eq.3.or.km.eq.1.and.vols(lvls+1,icl).lt.0.)vq=0.                    !/*pd        175*/
      vt=vt+vq                                                                  !/*pd        176*/
      do 350 i=1,2                                                              !/*pd        177*/
      if(km.ne.1.or.aras(lars+i,jt).lt.0.)go to 340                             !/*pd        178*/
      ta(i)=aras(lars+i,jt)+af(i)                                               !/*pd        179*/
      aras(lars+i,jt)=ta(i)                                                     !/*pd        180*/
  340 if(km.ne.3.or.atsa(lats+i,ix).lt.0.)go to 350                             !/*pd        181*/
      ta(i)=atsa(lats+i,ix)+af(i)                                               !/*pd        182*/
      atsa(lats+i,ix)=ta(i)                                                     !/*pd        183*/
  350 continue                                                                  !/*pd        184*/
!                                                                               !/*pd        185*/
!        print the results if required.                                         !/*pd        186*/
      if(dbcn(7).ne.0.)write(iuo,360)vq,vt,af(1),ta(1),af(2),ta(2)              !/*pd        187*/
  360 format(5x,6hvolume,5x,10hsum volume,6x,5harea-,6x,9hsum area-,            !/*pd        188*/
     & 6x,5harea+,6x,9hsum area+/1p6e13.5)                                      !/*pd        189*/
      if(km.eq.3)return                                                         !/*pd        190*/
  370 continue                                                                  !/*pd        191*/
!                                                                               !/*pd        192*/
!        negative calculated volume means actual volume is infinite.            !/*pd        193*/
      if(km.eq.2.and.jptal(ljpt+2,ital).ge.6)vt=vt*den(lden+icl)                !/*pd        194*/
      if(vt.lt.0.)ie=-1                                                         !/*pd        195*/
      if(vt.lt.0.)vt=-1.                                                        !/*pd        196*/
      if(km.eq.1.and.vols(lvls+1,icl).eq.0.)vols(lvls+1,icl)=vt                 !/*pd        197*/
      if(km.eq.2.and.vt.gt.0.)atsa(lats+1,ix)=atsa(lats+1,ix)+vt                !/*pd        198*/
      if(km.eq.2.and.vt.lt.0.)atsa(lats+1,ix)=vt                                !/*pd        199*/
      if(dbcn(7).ne.0..and.vt.lt.0.)write(iuo,380)                              !/*pd        200*/
  380 format(/30h the current cell is infinite.)                                !/*pd        201*/
      return                                                                    !/*pd        202*/
      end                                                                       !/*pd        203*/
