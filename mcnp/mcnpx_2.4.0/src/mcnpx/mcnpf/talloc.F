!#ident talloc.F from deck tc                                                   !/*tc          1*/
      subroutine talloc(lw)                                                     !/*tc          2*/
!        set up the tally locator arrays.                                       !/*tc          3*/
      use global_data                                                           !/*tcx         1*/
      use imcn_data                                                             !/*tcx         2*/
!                                                                               !/*tcx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*tcx         4*/
      implicit integer(kindi) (i-n)                                             !/*tcx         5*/
!                                                                               !/*tcx         6*/
      dimension ki(8,3)                                                         !/*tc          6*/
      data ki/0,0,0,1,0,1,1,0,1,1,13*0,1/                                       !/*tc          7*/
!                                                                               !/*tc          8*/
!        set up the tally locator arrays for cell and surface tallies.          !/*tc          9*/
      itds(lw)=0                                                                !/*tc4c        1*/
      do 70 m=1,3                                                               !/*tc         10*/
      mx=mxa                                                                    !/*tc         11*/
      if(m.eq.2)mx=mxj                                                          !/*tc         12*/
      mi=mipt                                                                   !/*tc         13*/
      if(m.eq.3)mi=1                                                            !/*tc         14*/
      do 60 k=1,mx                                                              !/*tc         15*/
      do 60 np=1,mi                                                             !/*tc         16*/
      do 50 ital=1,ntal                                                         !/*tc         17*/
      if(ktp(lktp+np,ital).eq.0.and.m.ne.3)go to 50                             !/*tc         18*/
      i=jptal(ljpt+2,ital)                                                      !/*tc         19*/
      if(ki(i,m).eq.0)go to 50                                                  !/*tc         20*/
      lr=iptal(lipt+1,1,ital)                                                   !/*tc         21*/
      if(lr.eq.0)go to 50                                                       !/*tc         22*/
      do 40 i=1,iptal(lipt+1,3,ital)                                            !/*tc         23*/
      do 30 j=1,itds(itds(lr+i))                                                !/*tc         24*/
      if(itds(itds(lr+i)+j).ne.k)go to 30                                       !/*tc         25*/
      if(m.eq.1)lo=locct(llct+np,k)                                             !/*tc         26*/
      if(m.eq.2)lo=locst(llst+np,k)                                             !/*tc         27*/
      if(m.eq.3)lo=locph(llph+k)                                                !/*tc         28*/
      if(lo.ne.0)go to 10                                                       !/*tc         29*/
      l=lw+1                                                                    !/*tc         30*/
      if(m.eq.1)locct(llct+np,k)=l                                              !/*tc         31*/
      if(m.eq.2)locst(llst+np,k)=l                                              !/*tc         32*/
      if(m.eq.3)locph(llph+k)=l                                                 !/*tc         33*/
      lw=lw+2                                                                   !/*tc         34*/
      li=lw                                                                     !/*tc         35*/
      itds(l)=0                                                                 !/*tc         36*/
      itds(li)=0                                                                !/*tc         37*/
   10 if(itds(li).eq.ital)go to 20                                              !/*tc         38*/
      li=lw                                                                     !/*tc         39*/
      itds(l)=itds(l)+1                                                         !/*tc         40*/
      itds(li)=ital                                                             !/*tc         41*/
      itds(li+1)=0                                                              !/*tc         42*/
      lw=lw+2                                                                   !/*tc         43*/
   20 itds(lw)=i                                                                !/*tc         44*/
      itds(li+1)=itds(li+1)+1                                                   !/*tc         45*/
      lw=lw+1                                                                   !/*tc         46*/
      go to 40                                                                  !/*tc         47*/
   30 continue                                                                  !/*tc         48*/
   40 continue                                                                  !/*tc         49*/
   50 continue                                                                  !/*tc         50*/
   60 continue                                                                  !/*tc         51*/
   70 continue                                                                  !/*tc         52*/
!                                                                               !/*tc         53*/
!        set up detector locator array and coincident detector control.         !/*tc         54*/
      ital=0                                                                    !/*tc         55*/
      ld=0                                                                      !/*tc4b        1*/
      do 130 i=1,ndtt                                                           !/*tc         56*/
      if(locdt(1,i).ne.ital)i0=i                                                !/*tc         57*/
      ital=locdt(1,i)                                                           !/*tc         58*/
      if(i.eq.1)go to 130                                                       !/*tc         59*/
      i1=5                                                                      !/*tcx         7*/
#ifdef RADIOG                                                                   /*!tcx         8*/
      if(jptal(ljpt+8,ital).gt.2)i1=19                                          !/*tcx         9*/
#endif                                                                          /*!tcx        10*/
      ip=iptal(lipt+1,1,ital)+i1*(i-i0)                                         !/*tcx        11*/
      jt=0                                                                      !/*tc         61*/
      do 90 j=1,i-1                                                             !/*tc         62*/
      if(locdt(1,j).ne.jt)jd=j                                                  !/*tc         63*/
      jt=locdt(1,j)                                                             !/*tc         64*/
      jp=iptal(lipt+1,1,jt)+i1*(j-jd)                                           !/*tcx        12*/
      ko=0                                                                      !/*tcx        13*/
      do 75 kk=1,mipt                                                           !/*tcx        14*/
   75 if(ktp(lktp+kk,jt).ne.ktp(lktp+kk,ital))ko=1                              !/*tcx        15*/
      if(ko.eq.1.or.                                                            !/*tcx        16*/
     & tds(jp+1).ne.tds(ip+1).or.tds(jp+2).ne.tds(ip+2).or.                     !/*tc         67*/
     & tds(jp+3).ne.tds(ip+3).or.ddg(1,i).ne.ddg(1,j).or.                       !/*tc         68*/
     & tds(iptal(lipt+8,1,jt)+iptal(lipt+8,4,jt)-1).ne.                         !/*tc4a        1*/
     & tds(iptal(lipt+8,1,ital)+iptal(lipt+8,4,ital)-1).or.                     !/*tc4a        2*/
     & jptal(ljpt+8,jt).ne.jptal(ljpt+8,ital))go to 90                          !/*tc         69*/
#ifdef RADIOG                                                                   /*!tcx        17*/
      if (jptal(ljpt+8,ital).eq.3) then                                         !/*tcx        18*/
         if(tds(jp+7).ne.tds(ip+7).or.tds(jp+8).ne.tds(ip+8).or.                !/*tcx        19*/
     &    tds(jp+9).ne.tds(ip+9))go to 90                                       !/*tcx        20*/
         if(tds(jp+i1-3).ge.0..or.tds(ip+i1-3).ge.0..or.                        !/*tcx        21*/
     &    tds(ip+i1-1).ne.tds(jp+i1-1))go to 90                                 !/*tcx        22*/
      endif                                                                     !/*tcx        23*/
#endif                                                                          /*!tcx        24*/
      if(iptal(lipt+2,2,jt).eq.iptal(lipt+2,2,ital))go to 100                   !/*tc         70*/
      if(iptal(lipt+2,2,jt).eq.0.or.iptal(lipt+2,2,ital).eq.0)go to 90          !/*tc         71*/
      do 80 k=1,mxa                                                             !/*tc         72*/
   80 if(tds(iptal(lipt+2,2,jt)+k).ne.tds(iptal(lipt+2,2,ital)+k))              !/*tc         73*/
     & go to 90                                                                 !/*tc         74*/
      go to 100                                                                 !/*tc         75*/
   90 continue                                                                  !/*tc         76*/
      go to 130                                                                 !/*tc         77*/
  100 idrc(i)=j                                                                 !/*tc         78*/
      if(ld.eq.0.and.ink(35).ne.0)write(iuo,110)                                !/*tc4b        2*/
  110 format(40h1the following detectors are coincident:,64x,6hprint ,          !/*tc         80*/
     & 8htable 35//5x,8hdetector,5x,5htally,10x,8hdetector,5x,5htally/)         !/*tc         81*/
      if(ink(35).ne.0)write(iuo,120)i-i0+1,jptal(ljpt+1,ital),                  !/*tc         82*/
     & j-jd+1,jptal(ljpt+1,jt)                                                  !/*tc         83*/
  120 format(i10,i12,i16,i12)                                                   !/*tc         84*/
      ld=1                                                                      !/*tc4b        3*/
      idrc(j)=-1                                                                !/*tc         86*/
  130 locdt(2,i)=jptal(ljpt+5,ital)+(i-i0)*iptal(lipt+2,5,ital)                 !/*tc         87*/
!                                                                               !/*tc         88*/
!        check for inconsistencies.                                             !/*tc         89*/
      do 140 i=1,ntal                                                           !/*tc         90*/
      do 140 kk=2,mipt                                                          !/*tcx        25*/
  140 if(ktp(lktp+kk,i).ne.0)go to 150                                          !/*tcx        26*/
      if(mcal.ge.2)go to 150                                                    !/*tcx        27*/
      ko=0                                                                      !/*tcx        28*/
      do 145 kk=2,mipt                                                          !/*tcx        29*/
  145 if(kpt(kk).ne.0)ko=1                                                      !/*tcx        30*/
      if(ko.eq.1)call erprnt(1,2,0,0,0,0,0,0,                                   !/*tcx        31*/
     & '47hthere are only neutron tallies in this problem.')                    !/*tc         93*/
  150 if(ndtt.eq.0)return                                                       !/*tc         94*/
      do 160 j=1,mxj                                                            !/*tc         95*/
  160 if(ksu(lksu+j).ne.0)go to 170                                             !/*tc4a        3*/
      go to 180                                                                 !/*tc4c        2*/
  170 call erprnt(1,2,0,0,0,0,0,0,                                              !/*tc         98*/
     & '58hdetector-reflector combo gives wrong answers.  see chap. 2')         !/*tc         99*/
!                                                                               !/*tc4c        3*/
!        check for slave and master or 2 identical slaves in same tally.        !/*tc4c        4*/
  180 ital=locdt(1,1)                                                           !/*tc4c        5*/
      n=0                                                                       !/*tc4c        6*/
      do 210 i=2,ndtt                                                           !/*tc4c        7*/
      if(locdt(1,i).eq.ital)go to 190                                           !/*tc4c        8*/
      ital=locdt(1,i)                                                           !/*tc4c        9*/
      n=i-1                                                                     !/*tc4c       10*/
      go to 210                                                                 !/*tc4c       11*/
  190 if(idrc(i).le.0)go to 210                                                 !/*tc4c       12*/
      l=idrc(i)                                                                 !/*tc4c       13*/
      if(l.gt.n)call erprnt(1,2,3,l-n,i-n,jptal(ljpt+1,ital),0,0,               !/*tc4c       14*/
     & '8hdetector,i3,13h and detector,i3,22h are the same in tally,i4')        !/*tc4c       15*/
      do 200 j=n+1,i-1                                                          !/*tc4c       16*/
  200 if(idrc(j).eq.l)call erprnt(1,2,3,j-n,i-n,jptal(ljpt+1,ital),0,0,         !/*tc4c       17*/
     & '8hdetector,i3,13h and detector,i3,22h are the same in tally,i4')        !/*tc4c       18*/
  210 continue                                                                  !/*tc4c       19*/
      return                                                                    !/*tc        100*/
      end                                                                       !/*tc        101*/
