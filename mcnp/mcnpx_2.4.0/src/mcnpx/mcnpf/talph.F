!#ident talph.F from deck tj                                                    !/*tj          1*/
      subroutine talph(ic,e)                                                    !/*tj          2*/
!        record energy e in pulse height tally accounts for cell ic.            !/*tj          3*/
      use global_data                                                           !/*tjx         1*/
!                                                                               !/*tjx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tjx         3*/
      implicit integer(kindi) (i-n)                                             !/*tjx         4*/
!                                                                               !/*tjx         5*/
      if(kf8.gt.0.and.(ipt.eq.1.or.ipt.gt.3))return                             !/*tjx         6*/
!                                                                               !/*tjxc        1*/
!        Allow variance reduction with pulse-height tallies.                    !/*tjxc        2*/
!        abs(kf8)=0,1,2,3,4=none,+f8,*f8,f8,f8+vrt. kf8>0 if p,e, only.         !/*tjxc        3*/
      if(abs(kf8).eq.3.and.abs(wgt-wgt9(1)).gt.1.e-5.and.lfatl.eq.0)            !/*tjxk        1*/
     & call expirx(1,'talph','nonanalog pulse height.')                         !/*tjxc        5*/
!                                                                               !/*tj          5*/
!        include the latent energy of a positron.                               !/*tj          6*/
      t=e                                                                       !/*tj          7*/
      if(jgp.le.-2)t=t+sign(2.*gpt(3),t)                                        !/*cor4-1    176*/
      t=t*wgt                                                                   !/*cor4-1    177*/
      if(t.lt.0.)t=t-1.e-12*t                                                   !/*tj4a        3*/
!                                                                               !/*tj         12*/
!        do all pulse height tallies that include cell ic.                      !/*tj         13*/
      li=lipt                                                                   !/*tj         14*/
      lo=locph(llph+ic)                                                         !/*tj         15*/
      lx=lo+2                                                                   !/*tj         16*/
      do 110 ml=1,itds(lo)                                                      !/*tj         17*/
      ital=itds(lx-1)                                                           !/*tj         18*/
      n1=itds(lx)                                                               !/*tj         19*/
      ta=t                                                                      !/*tj         20*/
      if(ktp(lktp+ipt,ital).eq.0)go to 110                                      !/*tjx         7*/
      if(jptal(ljpt+4,ital).eq.2.and.ipt.ne.3)go to 110                         !/*tj4a        4*/
      if(jptal(ljpt+4,ital).eq.2)ta=-sign(wgt,e*(jgp+2.5))                      !/*tj4a        5*/
      ibu=1                                                                     !/*tj         21*/
      if(lft(llft+6,ital)+lft(llft+7,ital).ne.0)ibu=jft(kjft+ital)              !/*tj         22*/
      if(ibu.eq.0)go to 110                                                     !/*tj         23*/
!                                                                               !/*tj         31*/
!        do the cell bins.                                                      !/*tj         32*/
      mr=1                                                                      !/*tj4a        6*/
      do 100 ir=1,n1                                                            !/*tj4a        7*/
      if(itds(lx+ir).eq.iptal(li+1,4,ital))go to(100,35)mr                      !/*tj4a        8*/
      lr=itds(iptal(li+1,1,ital)+itds(lx+ir))                                   !/*tj4a        9*/
      lr=lr+itds(lr)+1                                                          !/*tj4a       10*/
      nl=lev-1                                                                  !/*tj4a       11*/
      do 30 il=1,itds(lr)                                                       !/*tj4a       12*/
      j=itds(lr+il)                                                             !/*tj4a       13*/
      do 25 in=1,itds(j)                                                        !/*tj4a       14*/
      j=j+1                                                                     !/*tj4a       15*/
      do 10 l=0,nl                                                              !/*tj4a       16*/
   10 if(abs(itds(j)).eq.nint(udt(7,l)))go to 15                                !/*tj4a       17*/
      if(il.gt.1.or.ic.ne.abs(itds(j)))go to 25                                 !/*tj4a       18*/
      l=lev                                                                     !/*tj4a       19*/
   15 if(itds(j).gt.0)go to 30                                                  !/*tj4a       20*/
      do 20 i=1,3                                                               !/*tj4a       21*/
      ix=ijk(i)                                                                 !/*tjx         8*/
      if(l.lt.lev)ix=nint(udt(7+i,l))                                           !/*tj4a       23*/
   20 if(itds(j+i).ne.ix)go to 25                                               !/*tj4a       24*/
      go to 30                                                                  !/*tj4a       25*/
   25 if(itds(j).lt.0)j=j+3                                                     !/*tj4a       26*/
      go to 100                                                                 !/*tj4a       27*/
   30 nl=l-1                                                                    !/*tj4a       28*/
   35 mr=2                                                                      !/*tj4a       29*/
      mu=0                                                                      !/*tj         36*/
      td=ta                                                                     !/*tj         37*/
!                                                                               !/*tj         38*/
!        user bins loop.                                                        !/*tj         39*/
      if(iptal(li+3,1,ital).eq.0)go to 60                                       !/*tj         40*/
      if(lft(llft+7,ital).ne.0)go to 60                                         !/*tj         41*/
      nx=1                                                                      !/*tj         42*/
   40 mu=0                                                                      !/*tj         43*/
      td=ta                                                                     !/*tj         44*/
      ntx=nx                                                                    !/*tj         45*/
      call tallyx(td,mu)                                                        !/*tj         46*/
      if(mu.lt.0)go to 100                                                      !/*tj         47*/
      if(ibu.gt.0.and.ibu.lt.iptal(li+3,4,ital))go to 50                        !/*tj         48*/
      call errprn(1,ntbb(lntb+4,ital),2,zero+jptal(ljpt+1,ital),                !/*tj4b        1*/
     & zero+ibu,'tal','bin','tally not scored beyond last user bin.')           !/*tj4a       31*/
      go to 100                                                                 !/*tj         50*/
   50 nx=nx+1                                                                   !/*tj         51*/
      if(nx.le.iptal(li+1,5,ital)+1)go to 60                                    !/*tj         52*/
      call expirx(1,'talph','tallyx is in an endless loop.')                    !/*tj         53*/
      return                                                                    !/*tj         54*/
!                                                                               !/*tj         55*/
!        totals nest.                                                           !/*tj         56*/
   60 if(td.eq.0.)go to 90                                                      !/*tj         57*/
      j1=ktal+jptal(ljpt+5,ital)+iptal(li+2,5,ital)*(itds(lx+ir)-1)+1           !/*cor4-1    179*/
      iu=ibu                                                                    !/*tj         61*/
   70 j3=j1+(iu-1)*iptal(li+4,5,ital)                                           !/*tj         62*/
      tal(j3)=tal(j3)+td                                                        !/*cor4-1    180*/
      iu=iu+iptal(li+3,4,ital)-ibu                                              !/*cor4-1    181*/
      if(iu.le.iptal(li+3,3,ital))go to 70                                      !/*tjxa        1*/
   90 if(mu.ne.0)go to 40                                                       !/*tj         69*/
  100 continue                                                                  !/*tj         70*/
  110 lx=lx+n1+2                                                                !/*tj         71*/
      return                                                                    !/*tj         72*/
      end                                                                       !/*tj         73*/
