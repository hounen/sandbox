!#ident volume.F from deck vo                                                   !/*vo          1*/
      subroutine volume                                                         !/*vo          2*/
!        calculate volumes and areas of cells, surfaces, and segments.          !/*vo          3*/
      use global_data                                                           !/*vox         1*/
      use imcn_data                                                             !/*vox         2*/
!                                                                               !/*vox         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*vox         4*/
      implicit integer(kindi) (i-n)                                             !/*vox         5*/
!                                                                               !/*vox         6*/
      character hh*23                                                           !/*voxa        1*/
!                                                                               !/*vo         12*/
!        assume y-axis symmetry to start with.                                  !/*vo         13*/
      do 30 i=1,4                                                               !/*vo         14*/
      do 30 j=1,4                                                               !/*vo         15*/
   30 bbb(i,j)=0.                                                               !/*vo         16*/
      bbb(1,1)=1.                                                               !/*vo         17*/
      bbb(2,3)=1.                                                               !/*vo         18*/
      bbb(3,2)=1.                                                               !/*vo         19*/
      bbb(4,4)=1.                                                               !/*vo         20*/
!                                                                               !/*vo         21*/
!        set up pointers to atsa for tally segment volumes and areas.           !/*vo         22*/
      na=1                                                                      !/*vo         23*/
      do 40 i=1,ntal                                                            !/*vo         24*/
      if(iptal(lipt+4,1,i).eq.0.or.jptal(ljpt+2,i).eq.1)go to 40                !/*vo         25*/
      lsat(llsa+i)=na                                                           !/*vo         26*/
      na=na+iptal(lipt+1,3,i)*iptal(lipt+4,3,i)+1                               !/*vo         27*/
   40 continue                                                                  !/*vo         28*/
      if(novol.ne.0)go to 260                                                   !/*vo         29*/
!                                                                               !/*vo         30*/
!        calculate volumes and areas cell by cell.                              !/*vo         31*/
      if(dbcn(7).ne.0.)write(iuo,50)                                            !/*vo         32*/
   50 format(1h1,5x,39hdetails of volume and area calculations)                 !/*vo         33*/
      do 250 icl=1,mxa                                                          !/*vo         34*/
#ifdef PCDOS                                                                    /*!vo4a        1*/
      if(lockl)call pttyin                                                      !/*vo4a        2*/
#endif                                                                          /*!cpp       128*/
      if(irup.ne.0)call qttyin(ncl(lncl+icl),                                   !/*vo4a        3*/
     & '26hcalculating volume in cell,i5')                                      !/*vo4a        4*/
      if(dbcn(7).ne.0.)write(iuo,60)ncl(lncl+icl)                               !/*vo         35*/
   60 format(//5h cell,i6,4x,104(1h*))                                          !/*vo         36*/
!                                                                               !/*vo         37*/
!        set up cell description in ljav and list of surfaces in ljsv.          !/*vo         38*/
      n1=abs(lca(llca+icl))                                                     !/*vo         39*/
      nsjv=abs(lca(llca+icl+1))-n1+2                                            !/*vo         40*/
      ie=0                                                                      !/*vo4b        1*/
      if(nsjv.gt.mlgc)ie=-7                                                     !/*vo4b        2*/
      nj=nsjv                                                                   !/*vo         42*/
      do 70 i=1,nsjv-2                                                          !/*vo         43*/
   70 ljav(llav+1+i)=lja(llja+n1+i-1)                                           !/*vo         44*/
      ljav(llav+1)=1000001                                                      !/*vo         45*/
      ljav(llav+nsjv)=1000002                                                   !/*vo         46*/
      nsv=0                                                                     !/*vo         47*/
      do 90 n=1,nsjv                                                            !/*vo         48*/
      if(ljav(llav+n).gt.1000000)go to 90                                       !/*vo         49*/
      do 80 i=1,nsv                                                             !/*vo         50*/
   80 if(ljsv(llsv+i).eq.abs(ljav(llav+n)))go to 90                             !/*vo         51*/
      nsv=nsv+1                                                                 !/*vo         52*/
      ljsv(llsv+nsv)=abs(ljav(llav+n))                                          !/*vo         53*/
   90 continue                                                                  !/*vo         54*/
      nz=nsv                                                                    !/*vo         55*/
!                                                                               !/*vo         56*/
!        calculate the volume and areas of the cell.                            !/*vo         57*/
      e=0.                                                                      !/*vo         58*/
      if(ie.eq.0)call polhed(1,0,ie)                                            !/*vo4b        3*/
      if(ie.eq.0)call calcva(1,0,ie)                                            !/*vo         61*/
      if(ie.lt.0)vols(lvls+1,icl)=ie                                            !/*vo         62*/
      if(ie.lt.0)e=ie                                                           !/*vo         63*/
      if(ie.ge.-1)go to 110                                                     !/*vo         64*/
      do 100 n=1,nsjv                                                           !/*vo         65*/
      j=abs(ljav(llav+n))                                                       !/*vo         66*/
  100 if(j.lt.1000000)aras(lars+max(1,min(2,ljav(llav+n)+1)),j)=e               !/*vo         67*/
  110 if(ie.eq.0)call pieces                                                    !/*vo         68*/
!                                                                               !/*vo         69*/
!        calculate the volumes or masses of the cell tally segments.            !/*vo         70*/
      if(locct(llct+1,icl)+locct(llct+2,icl)+locct(llct+3,icl).eq.0)            !/*vo4a        5*/
     & go to 170                                                                !/*vo4a        6*/
      do 160 ital=1,ntal                                                        !/*vo         72*/
      if(lsat(llsa+ital).eq.0.or.jptal(ljpt+2,ital).lt.4)go to 160              !/*vo         73*/
      do 150 i1=1,iptal(lipt+1,3,ital)                                          !/*vo         74*/
      ip=itds(iptal(lipt+1,1,ital)+i1)                                          !/*vo         75*/
      do 120 i=1,itds(ip)                                                       !/*vo         76*/
  120 if(itds(ip+i).eq.icl)go to 130                                            !/*vo         77*/
      go to 150                                                                 !/*vo         78*/
  130 nsjv=nj                                                                   !/*vo         79*/
      nsv=nz                                                                    !/*vo         80*/
      jp=lsat(llsa+ital)+(i1-1)*iptal(lipt+4,3,ital)                            !/*vo         81*/
      do 140 ib=1,iptal(lipt+4,4,ital)-1                                        !/*vo         82*/
      ie=e                                                                      !/*vo         83*/
      if(ie.ne.-7)call setcel(ib,ie)                                            !/*vo4b        4*/
      if(atsa(lats+1,jp+ib).lt.0.)go to 140                                     !/*vo         86*/
      if(ie.eq.0)call polhed(2,jp+ib,ie)                                        !/*vo         87*/
      if(ie.eq.0)call calcva(2,jp+ib,ie)                                        !/*vo         88*/
      if(ie.lt.0)atsa(lats+1,jp+ib)=ie                                          !/*vo         89*/
  140 continue                                                                  !/*vo         90*/
  150 continue                                                                  !/*vo         91*/
  160 continue                                                                  !/*vo         92*/
!                                                                               !/*vo         93*/
!        calculate the areas of the surface tally segments.                     !/*vo         94*/
  170 do 240 n=1,nz                                                             !/*vo         95*/
      jsu=ljsv(llsv+n)                                                          !/*vo         96*/
      if(locst(llst+1,jsu)+locst(llst+2,jsu)+locst(llst+3,jsu).eq.0)            !/*vo4a        7*/
     & go to 240                                                                !/*vo4a        8*/
      ks=0                                                                      !/*vo         98*/
      kt=0                                                                      !/*vo         99*/
      do 180 i=1,nj                                                             !/*vo        100*/
      if(ljav(llav+i).eq.jsu)kt=1                                               !/*vo        101*/
  180 if(ljav(llav+i).eq.-jsu)ks=1                                              !/*vo        102*/
      do 230 ital=1,ntal                                                        !/*vo        103*/
      if(lsat(llsa+ital).eq.0.or.jptal(ljpt+2,ital).ge.4)go to 230              !/*vo        104*/
      do 220 i1=1,iptal(lipt+1,3,ital)                                          !/*vo        105*/
      ip=itds(iptal(lipt+1,1,ital)+i1)                                          !/*vo        106*/
      do 190 i=1,itds(ip)                                                       !/*vo        107*/
  190 if(itds(ip+i).eq.jsu)go to 200                                            !/*vo        108*/
      go to 220                                                                 !/*vo        109*/
  200 nsjv=nj                                                                   !/*vo        110*/
      nsv=nz                                                                    !/*vo        111*/
      jp=lsat(llsa+ital)+(i1-1)*iptal(lipt+4,3,ital)                            !/*vo        112*/
      do 210 ib=1,iptal(lipt+4,4,ital)-1                                        !/*vo        113*/
      ie=e                                                                      !/*vo        114*/
      if(ie.ne.-7)call setcel(ib,ie)                                            !/*vo4b        5*/
      if((ks.eq.0.or.atsa(lats+1,jp+ib).lt.0.).and.(kt.eq.0.or.                 !/*vo        117*/
     & atsa(lats+2,jp+ib).lt.0.))go to 210                                      !/*vo        118*/
      if(ie.eq.0)call polhed(3,jp+ib,ie)                                        !/*vo        119*/
      if(ie.eq.0)call calcva(3,jp+ib,ie)                                        !/*vo        120*/
      if(ie.lt.0.and.ks.ne.0)atsa(lats+1,jp+ib)=ie                              !/*vo        121*/
      if(ie.lt.0.and.kt.ne.0)atsa(lats+2,jp+ib)=ie                              !/*vo        122*/
  210 continue                                                                  !/*vo        123*/
  220 continue                                                                  !/*vo        124*/
  230 continue                                                                  !/*vo        125*/
  240 continue                                                                  !/*vo        126*/
  250 continue                                                                  !/*vo        127*/
!                                                                               !/*vo        128*/
!        print the results and move them to permanent arrays.                   !/*vo        129*/
  260 call prinv                                                                !/*vo        130*/
!                                                                               !/*vo        131*/
!        set up any source distributions that have the v option.                !/*vo        132*/
      do 310 ir=1,msd                                                           !/*vo        133*/
      if(ksd(lksd+10,ir).eq.0.or.ksd(lksd+12,ir).eq.0)go to 310                 !/*vo        134*/
      do 300 m=1,ksd(lksd+4,ir)                                                 !/*vo        135*/
      ic=namrsd(ir,m,2,hh,mkcp,jf)                                              !/*voxa        2*/
      if(ic.gt.0.and.ic.le.mxa)go to 290                                        !/*vo4b        7*/
      call erprnt(1,1,1,ksd(lksd+1,ir),0,0,0,0,                                 !/*vo        146*/
     & '51htrouble with cell specifiers of source distribution,i4')             !/*vo        147*/
      go to 310                                                                 !/*vo        148*/
  290 if(vol(lvol+ic).eq.0.)call erprnt                                         !/*vo        149*/
     & (1,1,2,ncl(lncl+ic),ksd(lksd+1,ir),0,0,0,                                !/*vo        150*/
     & '14hvolume of cell,i6,23h of source distribution,i4,8h is zero')         !/*vo        151*/
      spf(ksd(lksd+13,ir)+2,m)=spf(ksd(lksd+13,ir)+2,m)*vol(lvol+ic)            !/*vo        152*/
  300 if(spf(ksd(lksd+13,ir)+2,m).eq.0.)                                        !/*vo        153*/
     & spf(ksd(lksd+13,ir)+2,m)=vol(lvol+ic)                                    !/*vo        154*/
      call normh(ir)                                                            !/*vo        155*/
      if(ink(10).ne.0)call prsdst(ir)                                           !/*vo        156*/
  310 continue                                                                  !/*vo        157*/
      return                                                                    !/*vo        158*/
      end                                                                       !/*vo        164*/
