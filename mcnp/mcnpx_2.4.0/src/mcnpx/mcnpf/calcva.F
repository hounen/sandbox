!#ident calcva.F from deck cv                                                   !/*cv          1*/
      subroutine calcva(km,ix,ie)                                               !/*cv          2*/
!        calculate the volume and/or the areas of the current cell.             !/*cv          3*/
!        km=1 for a cell, 2 for a segment volume, 3 for a segment area.         !/*cv          4*/
!        ix is the index of atsa where segment result is to be stored.          !/*cv          5*/
!        returns ie=0 if successful, -1 if infinite, -2 if asymmetric,          !/*cv          6*/
!        -5 if quart fails.                                                     !/*cv          7*/
      use global_data                                                           !/*cvx         1*/
      use imcn_data                                                             !/*cvx         2*/
!                                                                               !/*cvx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*cvx         4*/
      implicit integer(kindi) (i-n)                                             !/*cvx         5*/
!                                                                               !/*cvx         6*/
      external tarea                                                            !/*cv         10*/
      dimension rs(4)                                                           !/*cv         11*/
!                                                                               !/*cv         12*/
!        put the current cell into q-form.                                      !/*cv         13*/
      call putnq(ie)                                                            !/*cv         14*/
      if(ie.eq.0)go to 10                                                       !/*cv         15*/
      call axis(ie)                                                             !/*cv         16*/
      if(ie.eq.0)call putnq(ie)                                                 !/*cv         17*/
      if(ie.ne.0)return                                                         !/*cv         18*/
!                                                                               !/*cv         19*/
!        do each appropriate surface of the current cell.                       !/*cv         20*/
   10 vt=0.                                                                     !/*cv         21*/
      ncrn=0                                                                    !/*cv         22*/
      do 250 n=1,nsv                                                            !/*cv         23*/
      kt=ljsv(llsv+n)                                                           !/*cv         24*/
      if(km.eq.3.and.kt.ne.jsu)go to 250                                        !/*cv         25*/
!                                                                               !/*cv         26*/
!        if torus, do both the upper branch and the lower branch.               !/*cv         27*/
      rb=-.5*scfq(lscq+2,kt)                                                    !/*cv         28*/
      lh=1                                                                      !/*cv         29*/
      if(scfq(lscq+2,kt).ne.0.)lh=2                                             !/*cv         30*/
      do 240 jh=1,lh                                                            !/*cv         31*/
      call intsec(kt,jh,lh,ni,0,ie)                                             !/*cv         32*/
      if(ie.eq.-5)return                                                        !/*cv         33*/
!                                                                               !/*cv         34*/
!        find appropriate sections to integrate.                                !/*cv         35*/
      mh=0                                                                      !/*cv         36*/
      ir=0                                                                      !/*cv         37*/
      np=ni+1                                                                   !/*cv         38*/
      do 230 im=1,np                                                            !/*cv         39*/
!                                                                               !/*cv         40*/
!        set ie=-1 if the cell is infinite.                                     !/*cv         41*/
      if(im.ne.1.and.im.ne.np)go to 70                                          !/*cv         42*/
      mx=max(1,im-1)                                                            !/*cvx         7*/
      rs(1)=rsint(lrsn+1,mx)                                                    !/*cvx         8*/
      rs(2)=rsint(lrsn+2,mx)                                                    !/*cvx         9*/
      rs(3)=rsint(lrsn+1,mx+1)                                                  !/*cvx        10*/
      rs(4)=rsint(lrsn+2,mx+1)                                                  !/*cvx        11*/
      if(scfq(lscq+1,kt).eq.0.)go to 30                                         !/*cv         45*/
      if(ni.eq.0)go to 50                                                       !/*cv         46*/
      if(lh.eq.2.or.rsint(lrsn+1,max(2,im)).eq.0.)go to 230                     !/*cv         47*/
      rs(2)=rsint(lrsn+2,max(2,im))+min(4,2*im)-3.                              !/*cv         48*/
      go to 50                                                                  !/*cv         49*/
   30 rs(2)=rsint(lrsn+2,max(2,im))                                             !/*cv         50*/
      rs(1)=rsint(lrsn+1,np)+1.                                                 !/*cv         51*/
      if(im.eq.np)go to 50                                                      !/*cv         52*/
      if(scfq(lscq+3,kt).eq.0.)go to 230                                        !/*cv         53*/
      do 40 i=2,np                                                              !/*cv         54*/
   40 if(rsint(lrsn+2,i).eq.rs(2))rs(1)=3.*rsint(lrsn+1,i)+1.                   !/*cv         55*/
   50 call midpnt(kt,rs,rs(3),jh,l,m1)                                          !/*cv         56*/
      if(l.eq.0)go to 230                                                       !/*cv         57*/
      ie=-1                                                                     !/*cv         58*/
      if(dbcn(7).ne.0.)write(iuo,60)nsf(lnsf+kt)                                !/*cv         59*/
   60 format(3x,7hsurface,i7,13h is infinite.)                                  !/*cv         60*/
      if(km.ne.1)return                                                         !/*cv         61*/
      vols(lvls+1,icl)=-1.                                                      !/*cv         62*/
      aras(lars+1,kt)=-1.                                                       !/*cv         63*/
      aras(lars+2,kt)=-1.                                                       !/*cv         64*/
      go to 250                                                                 !/*cv         65*/
!                                                                               !/*cv         66*/
!        determine the suitability and extent of the section.                   !/*cv         67*/
   70 if(im.lt.ir+1)go to 230                                                   !/*cv         68*/
      do 90 ir=im,ni                                                            !/*cv         69*/
      rs(1)=rsint(lrsn+1,ir)                                                    !/*cvx        12*/
      rs(2)=rsint(lrsn+2,ir)                                                    !/*cvx        13*/
      rs(3)=rsint(lrsn+1,ir+1)                                                  !/*cvx        14*/
      rs(4)=rsint(lrsn+2,ir+1)                                                  !/*cvx        15*/
      call midpnt(kt,rs,rs(3),jh,l,m2)                                          !/*cv         72*/
      if(ir.eq.im.and.l.eq.0)go to 230                                          !/*cv         73*/
      if(ir.gt.im.and.(l.eq.0.or.m2.ne.m1))go to 100                            !/*cv         74*/
   90 m1=m2                                                                     !/*cv         75*/
  100 t0=0.                                                                     !/*cv         76*/
      t2=0.                                                                     !/*cv         77*/
      vq=0.                                                                     !/*cv         78*/
      ar=0.                                                                     !/*cv         79*/
!                                                                               !/*cv         80*/
!        add the ends of the section to the list of corners.                    !/*cv         81*/
      do 120 i=1,2                                                              !/*cv         82*/
      k=im+(i-1)*(ir-im)                                                        !/*cv         83*/
      do 110 l=1,ncrn                                                           !/*cv         84*/
  110 if(rscrn(lrsc+1,l).eq.rsint(lrsn+1,k).and.rscrn(lrsc+2,l).eq.             !/*cv         85*/
     & rsint(lrsn+2,k).and.(icrn(licr+1,l).eq.kt.and.icrn(licr+2,l).eq.         !/*cv         86*/
     & iint(liin+k).or.icrn(licr+1,l).eq.iint(liin+k).and.                      !/*cv         87*/
     & icrn(licr+2,l).eq.kt))go to 120                                          !/*cv         88*/
      ncrn=ncrn+1                                                               !/*cv         89*/
      rscrn(lrsc+1,ncrn)=rsint(lrsn+1,k)                                        !/*cv         90*/
      rscrn(lrsc+2,ncrn)=rsint(lrsn+2,k)                                        !/*cv         91*/
      icrn(licr+1,ncrn)=kt                                                      !/*cv         92*/
      icrn(licr+2,ncrn)=iint(liin+k)                                            !/*cv         93*/
  120 continue                                                                  !/*cv         94*/
!                                                                               !/*cv         95*/
!        calculate the volume under the section and add it in.                  !/*cv         96*/
      if(km.eq.3.or.km.eq.1.and.vols(lvls+1,icl).lt.0.)go to 130                !/*cv         97*/
      if(scfq(lscq+1,kt).eq.0.)go to 130                                        !/*cv         98*/
      q1=rsint(lrsn+2,im)                                                       !/*cv         99*/
      q2=rsint(lrsn+2,ir)                                                       !/*cv        100*/
      if(rb.ne.0.)t0=sqqint(-scfq(lscq+3,kt),-scfq(lscq+4,kt),                  !/*cv        101*/
     & rb**2-scfq(lscq+5,kt),q2,q1)                                             !/*cv        102*/
      vq=pie*(2.*rb*((3-2*jh)*t0+rb*(q2-q1))-                                   !/*cv        103*/
     & scfq(lscq+1,kt)*(scfq(lscq+3,kt)*(q2**3-q1**3)/3.+                       !/*cv        104*/
     & scfq(lscq+4,kt)*(q2**2-q1**2)/2.+scfq(lscq+5,kt)*(q2-q1)))               !/*cv        105*/
      vq=vq*sign(one,scfq(lscq+1,kt)*(3-2*jh))*(3-2*m1)                         !/*cv        106*/
      vt=vt+vq                                                                  !/*cv        107*/
!                                                                               !/*cv        108*/
!        calculate the area of the section and add it in.                       !/*cv        109*/
  130 if(km.eq.2.or.km.eq.1.and.aras(lars+m1,kt).lt.0..or.                      !/*cv        110*/
     & km.eq.3.and.atsa(lats+m1,ix).lt.0.)go to 200                             !/*cv        111*/
      i1=2                                                                      !/*cv        112*/
      if(scfq(lscq+1,kt).eq.0.)i1=1                                             !/*cv        113*/
!                                                                               !/*cv        114*/
      if(scfq(lscq+2,kt).ne.0.)go to 150                                        !/*cv        115*/
      if(scfq(lscq+1,kt).ne.0.)go to 140                                        !/*cv        116*/
!                                                                               !/*cv        117*/
!        plane.                                                                 !/*cv        118*/
      ar=pie*abs(rsint(lrsn+i1,im)**2-rsint(lrsn+i1,ir)**2)                     !/*cv        119*/
      go to 190                                                                 !/*cv        120*/
!                                                                               !/*cv        121*/
!        quadratic.                                                             !/*cv        122*/
  140 t1=scfq(lscq+3,kt)/scfq(lscq+1,kt)-1.                                     !/*cv        123*/
      q2=rsint(lrsn+i1,im)                                                      !/*cv        124*/
      q1=rsint(lrsn+i1,ir)                                                      !/*cv        125*/
      t0=sqqint(t1*scfq(lscq+3,kt)/scfq(lscq+1,kt),                             !/*cv        126*/
     & t1*scfq(lscq+4,kt)/scfq(lscq+1,kt),.25*(scfq(lscq+4,kt)/                 !/*cv        127*/
     & scfq(lscq+1,kt))**2-scfq(lscq+5,kt)/scfq(lscq+1,kt),q2,q1)               !/*cv        128*/
      ar=2.*pie*abs(t0)                                                         !/*cv        129*/
      go to 190                                                                 !/*cv        130*/
!                                                                               !/*cv        131*/
!        torus.                                                                 !/*cv        132*/
  150 rb=-.5*scfq(lscq+2,kt)                                                    !/*cv        133*/
      t5=rb**2-scfq(lscq+5,kt)+.25*scfq(lscq+4,kt)**2/scfq(lscq+3,kt)           !/*cv        134*/
      b=sqrt(t5/scfq(lscq+3,kt))                                                !/*cv        135*/
      t6=max(-one,min(one,(rsint(lrsn+i1,im)+                                   !/*cv        136*/
     & .5*scfq(lscq+4,kt)/scfq(lscq+3,kt))/b))                                  !/*cv        137*/
      t7=max(-one,min(one,(rsint(lrsn+i1,ir)+                                   !/*cv        138*/
     & .5*scfq(lscq+4,kt)/scfq(lscq+3,kt))/b))                                  !/*cv        139*/
      t1=acos(t6)                                                               !/*cv        140*/
      t2=acos(t7)                                                               !/*cv        141*/
      t0=t6-t7                                                                  !/*cv        142*/
      t4=t1-t2                                                                  !/*cv        143*/
      if(abs(scfq(lscq+3,kt)-1.).le.1.e-8)go to 170                             !/*cv        144*/
      if(jh.eq.1)go to 160                                                      !/*cv        145*/
      t1=2.*pie-t1                                                              !/*cv        146*/
      t2=2.*pie-t2                                                              !/*cv        147*/
  160 ajsh=scfq(lscq+3,kt)-1.                                                   !/*cv        148*/
      t0=sqqint(ajsh,zero,one,t6,t7)                                            !/*cv        149*/
      t4=qnc7(tarea,t1,t2,zero+1.e-5,100,k)                                     !/*cv        150*/
      if(k.lt.0)go to 180                                                       !/*cv        151*/
  170 ar=2.*pie*b*(rb*abs(t4)+sqrt(t5)*(3-2*jh)*abs(t0))                        !/*cv        152*/
      if(ar.gt.0.)go to 190                                                     !/*cv        153*/
  180 ar=-1.e30                                                                 !/*cv        154*/
  190 if(km.eq.3)t2=atsa(lats+m1,ix)+ar                                         !/*cv        155*/
      if(km.eq.3)atsa(lats+m1,ix)=atsa(lats+m1,ix)+ar                           !/*cv        156*/
      if(km.eq.1)t2=aras(lars+m1,kt)+ar                                         !/*cv        157*/
      if(km.eq.1)aras(lars+m1,kt)=aras(lars+m1,kt)+ar                           !/*cv        158*/
!                                                                               !/*cv        159*/
!        print the results if required.                                         !/*cv        160*/
  200 if(mh.eq.0.and.dbcn(7).ne.0.)write(iuo,210)nsf(lnsf+kt)                   !/*cv        161*/
  210 format(51h corners, volumes, and areas of sections of surface,i7/         !/*cv        162*/
     & 5x,2hj1,4x,2hr1,11x,2hs1,9x,2hj2,4x,2hr2,11x,2hs2,10x,                   !/*cv        163*/
     & 6hvolume,5x,10hsum volume,6x,4harea,7x,8hsum area)                       !/*cv        164*/
      mh=1                                                                      !/*cv        165*/
      if(dbcn(7).ne.0..and.(vq.ne.0..or.ar.ne.0.))write(iuo,220)                !/*cv        166*/
     & min(1,iint(liin+im))*nsf(lnsf+max(1,iint(liin+im))),                     !/*cv        167*/
     & (rsint(lrsn+k,im),k=1,2),                                                !/*cv        168*/
     & min(1,iint(liin+ir))*nsf(lnsf+max(1,iint(liin+ir))),                     !/*cv        169*/
     & (rsint(lrsn+k,ir),k=1,2),vq,vt,ar,t2                                     !/*cv        170*/
  220 format(2x,1p,2(i7,e12.5,e13.5),4e13.5)                                    !/*cv        171*/
  230 continue                                                                  !/*cv        172*/
  240 continue                                                                  !/*cv        173*/
      if(km.eq.3)return                                                         !/*cv        174*/
  250 continue                                                                  !/*cv        175*/
!                                                                               !/*cv        176*/
!        negative calculated volume means actual volume is infinite.            !/*cv        177*/
      if(km.eq.2.and.jptal(ljpt+2,ital).ge.6)vt=vt*den(lden+icl)                !/*cv        178*/
      if(vt.lt.0.)ie=-1                                                         !/*cv        179*/
      if(vt.lt.0.)vt=-1.                                                        !/*cv        180*/
      if(km.eq.1.and.vols(lvls+1,icl).eq.0.)vols(lvls+1,icl)=vt                 !/*cv        181*/
      if(km.eq.2.and.vt.gt.0.)atsa(lats+1,ix)=atsa(lats+1,ix)+vt                !/*cv        182*/
      if(km.eq.2.and.vt.lt.0.)atsa(lats+1,ix)=vt                                !/*cv        183*/
      if(dbcn(7).ne.0..and.vt.lt.0.)write(iuo,260)                              !/*cv        184*/
  260 format(/30h the current cell is infinite.)                                !/*cv        185*/
      return                                                                    !/*cv        186*/
      end                                                                       !/*cv        187*/
