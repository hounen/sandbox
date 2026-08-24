!#ident photp.F from deck aq                                                    !/*aq          1*/
      subroutine photp                                                          !/*aq          2*/
!        print information about the photons produced at neutron                !/*aq          3*/
!        collisions.                                                            !/*aq          4*/
      use global_data                                                           !/*aqx         1*/
!                                                                               !/*aqx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*aqx         3*/
      implicit integer(kindi) (i-n)                                             !/*aqx         4*/
!                                                                               !/*aqx         5*/
      character hl(2)*5                                                         !/*aq          6*/
      data hl/'  mev','jerks'/                                                  !/*aq          7*/
!                                                                               !/*aq          8*/
      write(iuo,10)                                                             !/*aq          9*/
   10 format(50h1summary of photons produced in neutron collisions/)            !/*aq         10*/
      if(pax(1,igain_photon_from_neutrons,2).ne.zero)go to 30                   !/*aqx         6*/
      write(iuo,20)                                                             !/*aq         12*/
   20 format(/21h no photons produced.)                                         !/*aq         13*/
      return                                                                    !/*aq         14*/
!                                                                               !/*aq         15*/
!        print data for each cell.                                              !/*aq         16*/
   30 lw=0                                                                      !/*aq         17*/
      do 40 i=1,ntal                                                            !/*aq         18*/
   40 if(jptal(ljpt+2,i).eq.6.and.ktp(lktp+2,i).ne.0.and.                       !/*aq         19*/
     & jptal(ljpt+4,i).eq.1)lw=1                                                !/*aq         20*/
      write(iuo,50)hl(lw+1)                                                     !/*aq         21*/
   50 format(8x,4hcell,6x,9hnumber of,6x,10hweight per,5x,10henergy per,        !/*aq         22*/
     & 5x,10havg photon,4x,a5,7h/gm per,4x,11hweight/neut,4x,                   !/*aq         23*/
     & 11henergy/neut/19x,7hphotons,7x,11hsource neut,4x,11hsource neut,        !/*aq         24*/
     & 6x,6henergy,7x,11hsource neut,5x,9hcollision,6x,9hcollision/)            !/*aq         25*/
      do 60 i=1,3                                                               !/*aq         26*/
   60 tpp(i)=0.                                                                 !/*aq         27*/
      f=1.                                                                      !/*aq         28*/
      if(lw.eq.1)f=1.60219e-22                                                  !/*aq         29*/
      do 80 ic=1,mxa                                                            !/*aq         30*/
      t=vol(lvol+ic)*den(lden+ic)                                               !/*aq         31*/
      if(t.eq.0.)t=1.                                                           !/*aq         32*/
      t1=0.                                                                     !/*aq         33*/
      if(pcc(lpcc+2,ic).ne.0.)t1=pcc(lpcc+3,ic)/pcc(lpcc+2,ic)                  !/*aq         34*/
      t2=1.                                                                     !/*aq         35*/
      if(pac(lpac+1,4,ic).ne.0.)t2=pac(lpac+1,4,ic)                             !/*aq         36*/
      do 70 i=1,3                                                               !/*aq         37*/
   70 tpp(i)=tpp(i)+pcc(lpcc+i,ic)                                              !/*aq         38*/
   80 write(iuo,90)ic,ncl(lncl+ic),pcc(lpcc+1,ic),fpi*pcc(lpcc+2,ic),           !/*aq4a        1*/
     & fpi*pcc(lpcc+3,ic),t1,f*fpi*pcc(lpcc+3,ic)/t,pcc(lpcc+2,ic)/t2,          !/*aq4a        2*/
     & pcc(lpcc+3,ic)/t2                                                        !/*aq4a        3*/
   90 format(2i6,f15.0,tl1,1pe18.5,5e15.5)                                      !/*aq4a        4*/
      t=0.                                                                      !/*aq         43*/
      if(tpp(2).ne.0.)t=tpp(3)/tpp(2)                                           !/*aq         44*/
      write(iuo,100)tpp(1),fpi*tpp(2),fpi*tpp(3),t                              !/*aq4a        5*/
  100 format(7x,5htotal,f15.0,tl1,1pe18.5,2e15.5)                               !/*aq4a        6*/
!                                                                               !/*aq         47*/
!        print data for each photon energy interval.                            !/*aq         48*/
      if(mxa.gt.30)write(iuo,110)                                               !/*aq         49*/
  110 format(51h1energy distribution of photons produced in neutron,            !/*aq         50*/
     & 11h collisions)                                                          !/*aq         51*/
      write(iuo,120)                                                            !/*aq         52*/
  120 format(///2x,6henergy,6x,9hnumber of,8x,6hnumber,7x,10hcum number,        !/*aq         53*/
     & 6x,9hweight of,7x,6hweight,7x,10hcum weight/9h interval,6x,              !/*aq         54*/
     & 7hphotons,8x,9hfrequency,4x,12hdistribution,6x,7hphotons,7x,             !/*aq         55*/
     & 9hfrequency,4x,12hdistribution/1h )                                      !/*aq         56*/
      t1=0.                                                                     !/*aq         57*/
      t2=0.                                                                     !/*aq         58*/
      n=16                                                                      !/*aq         59*/
      if(mcal.ne.0)n=jgm(2)                                                     !/*aq         60*/
      do 130 i=1,n                                                              !/*aq         61*/
      t1=t1+febl(lfeb+1,i)/pax(1,igain_photon_from_neutrons,2)                  !/*aqx         7*/
      t2=t2+febl(lfeb+2,i)/pax(2,igain_photon_from_neutrons,2)                  !/*aqx         8*/
      e=ebl(lebl+i)                                                             !/*aq4b        3*/
  130 write(iuo,140)e,febl(lfeb+1,i),                                           !/*aqx         9*/
     & febl(lfeb+1,i)/pax(1,igain_photon_from_neutrons,2),t1,                   !/*aqx        10*/
     & febl(lfeb+2,i)*fpi,                                                      !/*aqx        11*/
     & febl(lfeb+2,i)/pax(2,igain_photon_from_neutrons,2),t2                    !/*aqx        12*/
  140 format(f9.3,f15.0,tl1,1pe17.5,4e15.5)                                     !/*aq4b        6*/
      write(iuo,150)pax(1,igain_photon_from_neutrons,2),1.,                     !/*aqx        13*/
     & pax(2,igain_photon_from_neutrons,2)*fpi,1.                               !/*aqx        14*/
  150 format(/3x,5htotal,f16.0,tl1,1pe17.5,e30.5,e15.5)                         !/*aq4a       11*/
      return                                                                    !/*aq         71*/
      end                                                                       !/*aq         72*/
