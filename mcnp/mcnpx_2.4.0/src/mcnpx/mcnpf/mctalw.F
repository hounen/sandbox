!#ident mctalw.F from deck mw                                                   !/*mw          1*/
      subroutine mctalw(hm)                                                     !/*mw          2*/
!        write all tally data from the current dump to mctal file hm.           !/*mw          3*/
      use global_data                                                           !/*mwx         1*/
!                                                                               !/*mwx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*mwx         3*/
      implicit integer(kindi) (i-n)                                             !/*mwx         4*/
!                                                                               !/*mwx         5*/
      dimension e(4),y(4),ic(11)                                                !/*mw          5*/
      character hb*5,hf*7,hm*8,hq*80,ht*75                                      !/*mw4c2       1*/
!                                                                               !/*mw          7*/
!        create mctal file with a unique name.  write header records.           !/*mw          8*/
      call unique(hm,jtty)                                                      !/*mw          9*/
      open(iut,file=hm,status='new')                                            !/*mw         10*/
      write(ht,'(f16.0)')rnr                                                    !/*mw4a        1*/
      if(mct.lt.0)write(iut,'(35x,i5,i11,a15)')knod,nps,ht(1:15)                !/*mw4a        2*/
      if(mct.ge.0)write(iut,'(2a8,a19,i5,i11,a15)')kod,ver,probid,              !/*mw4a        3*/
     & knod,nps,ht(1:15)                                                        !/*mw4a        4*/
      write(iut,'(1x,a79)')aid                                                  !/*mw         13*/
      if(npert.eq.0)write(iut,'(4hntal,i6)')ntal                                !/*mw4b        1*/
      if(npert.ne.0)write(iut,'(4hntal,i6,6h npert,i6)')ntal,npert              !/*mw4b        2*/
      write(iut,'(16i5)')(jptal(ljpt+1,i),i=1,ntal)                             !/*mw         15*/
!                                                                               !/*mw         16*/
!        calculate the tally normalization factor.                              !/*mw         17*/
      t=max(1,nps)                                                              !/*mw         18*/
      if(nsr.eq.71.and.kcz.ge.ikz)t=nsrck*(kcz-ikz)                             !/*mw4c        1*/
      if(kc8.lt.0)t=t+nsrck-wt0*nsa                                             !/*mw4c        3*/
      if(knrm.ne.0)t=pax(1,1,1)                                                 !/*mwx        10*/
      if(nsr.eq.6.and.nrrs.ge.nrss)t=max(1,np1)                                 !/*mw         19*/
      if(nsr.eq.6.and.nrrs.lt.nrss)t=max(1,npsr)                                !/*mw         20*/
      fp=1./max(t,one)                                                          !/*mw4b        6*/
!                                                                               !/*mw4c2       2*/
!        determine if surfaces have macrobody facets.                           !/*mw4c2       3*/
      kq=0                                                                      !/*mw4c2       4*/
      do 15 i=1,mxj                                                             !/*mw4c2       5*/
   15 if(ksm(lksm+i).ne.0)kq=1                                                  !/*mwx        12*/
!                                                                               !/*mw         25*/
!        write a block of records for each tally.                               !/*mw         26*/
      do 100 k=1,ntal                                                           !/*mw         27*/
      if(dbcn(26).ne.0.)then                                                    !/*mwx        13*/
      write(iut,'(5htally,2i5)')jptal(ljpt+1,k),                                !/*mwx        14*/
     & ktp(lktp+1,k)+2*ktp(lktp+2,k)+4*ktp(lktp+3,k)                            !/*mwx        15*/
      else                                                                      !/*mwx        16*/
      if(jptal(ljpt+2,k).eq.6.and.jptal(ljpt+4,k).eq.2) then                    !/*mwx        17*/
         j=0                                                                    !/*mwx        18*/
         do 10 i=1,mipt                                                         !/*mwx        19*/
         if(kpt(i).ne.0)j=j+1                                                   !/*mwx        20*/
   10    continue                                                               !/*mwx        21*/
         write(iut,'(5htally,3i5)')jptal(ljpt+1,k),-j,                          !/*mwx        22*/
     &    jptal(ljpt+8,k)                                                       !/*mwx        23*/
         write(iut,'(40i2)')(kpt(i),i=1,mipt)                                   !/*mwx        24*/
      else                                                                      !/*mwx        25*/
         write(iut,'(5htally,3i5)')jptal(ljpt+1,k),-jptal(ljpt+3,k),            !/*mwx        26*/
     &    jptal(ljpt+8,k)                                                       !/*mwx        27*/
         write(iut,'(40i2)')(ktp(lktp+i,k),i=1,mipt)                            !/*mwx        28*/
      endif                                                                     !/*mwx        29*/
      endif                                                                     !/*mwx        30*/
      l=jptal(ljpt+6,k)                                                         !/*mw         29*/
      if(l.eq.0)go to 40                                                        !/*mw         30*/
      n=itds(l)                                                                 !/*mw         31*/
      do 30 i=l+1,l+n,25                                                        !/*mw         32*/
      do 20 j=i,i+24                                                            !/*mw         33*/
   20 ht((j-i)*3+1:(j-i)*3+3)=char(itds(j)/65536)//                             !/*mw         34*/
     & char(mod(itds(j),65536)/256)//char(mod(itds(j),256))                     !/*mw         35*/
   30 write(iut,'(5x,a75)')ht                                                   !/*mw         36*/
   40 do 70 ib=1,8                                                              !/*mw         37*/
      m=iptal(lipt+ib,3,k)                                                      !/*mw         38*/
      hb=ibin(ib:ib)                                                            !/*mw         39*/
      if(m.eq.iptal(lipt+ib,4,k).and.ib.gt.2)hb(2:2)='t'                        !/*mw         40*/
      if(ib.gt.2.and.iptal(lipt+ib,6,k).ne.0)hb(2:2)='c'                        !/*mw4c        5*/
      if(ib.gt.2.and.iptal(lipt+ib,1,k).eq.0.and.m.le.1)m=0                     !/*mw4a        6*/
      write(iut,'(a2,i8)')hb,m                                                  !/*mw         42*/
!                                                                               !/*mw         43*/
!        write cell or surface numbers.                                         !/*mw         44*/
      if(ib.gt.1.or.jptal(ljpt+2,k).eq.5)go to 60                               !/*mw         45*/
      do 50 j=1,m                                                               !/*mw         46*/
      n=mod(j-1,11)+1                                                           !/*mw         47*/
      l=itds(iptal(lipt+1,1,k)+j)                                               !/*mw         48*/
      ic(n)=0                                                                   !/*mw         49*/
      if(itds(l).eq.1.and.jptal(ljpt+2,k).ge.4)ic(n)=ncl(lncl+itds(l+1))        !/*mw         50*/
      if(itds(l).eq.1.and.jptal(ljpt+2,k).le.3)ic(n)=nsf(lnsf+itds(l+1))        !/*mw4c        6*/
!                                                                               !/*mw4c2       7*/
!        write cells or surfaces without macrobody facets.                      !/*mw4c2       8*/
      if((n.eq.11.or.j.eq.m).and.(jptal(ljpt+2,k).ge.4.or.kq.eq.0))             !/*mw4c2       9*/
     & write(iut,'(11i7)')(ic(i),i=1,n)                                         !/*mw4c2      10*/
      if(jptal(ljpt+2,k).ge.4.or.kq.eq.0)go to 50                               !/*mw4c2      11*/
!                                                                               !/*mw4c2      12*/
!        write surfaces with macrobody facets.                                  !/*mw4c2      13*/
      if(n.eq.1)hq=' '                                                          !/*mw4c2      14*/
      hf=' '                                                                    !/*mw4c2      15*/
      if(ic(n).eq.0)kfq=0                                                       !/*mw4c2      16*/
      if(kfq.eq.0.or.ic(n).ge.10000)write(hf,'(i7)')ic(n)                       !/*mw4c2      17*/
      if(kfq.ne.0.and.ic(n).le.9999)write(hf,'(i5,1h.,i1)')ic(n),kfq            !/*mw4c2      18*/
      hq(1+(n-1)*7:n*7)=hf(1:7)                                                 !/*mw4c2      19*/
      if(n.eq.11.or.j.eq.m)write(iut,'(a80)')hq                                 !/*mw4c2      20*/
   50 continue                                                                  !/*mw4c2      21*/
!                                                                               !/*mw         53*/
!        write cosine, energy, and time bins.                                   !/*mw         54*/
   60 if(iptal(lipt+ib,4,k).eq.m)m=m-1                                          !/*mw         55*/
#ifdef RADIOG                                                                   /*!mwx        31*/
      if(jptal(ljpt+8,k).gt.2) then                                             !/*mwx        32*/
         if(m.gt.0.and.(ib.eq.4.or.ib.eq.6))write(iut,'(1p6e13.5)')             !/*mwx        33*/
     &    (tds(iptal(lipt+ib,1,k)+j-1),j=1,m+1)                                 !/*mwx        34*/
         if(m.gt.0.and.ib.ge.7)write(iut,'(1p6e13.5)')                          !/*mwx        35*/
     &    (tds(iptal(lipt+ib,1,k)+j),j=1,m)                                     !/*mwx        36*/
         go to 70                                                               !/*mwx        37*/
      endif                                                                     !/*mwx        38*/
#endif                                                                          /*!mwx        39*/
      if(m.gt.0.and.ib.ge.6)write(iut,'(1p6e13.5)')                             !/*mwx        40*/
     & (tds(iptal(lipt+ib,1,k)+j),j=1,m)                                        !/*mw         57*/
   70 continue                                                                  !/*mwx        41*/
!                                                                               !/*mw         58*/
!        write table of tallies and errors.                                     !/*mw         59*/
      do 100 ip=0,npert                                                         !/*mw4b        7*/
      if(ip.eq.0)write(iut,'(4hvals)')                                          !/*mw4b        8*/
      if(ip.gt.0)write(iut,'(10hvals pert ,i5)')iptb(lipb+1,ip)                 !/*mw4b        9*/
      kp=k+ip*ntal                                                              !/*mw4b       10*/
      do 80 m=1,iptal(lipt+1,5,k)                                               !/*mw         61*/
      i=mod(m-1,4)+1                                                            !/*mw         62*/
      y(i)=tal(ltal+jptal(ljpt+5,k)+mxf+m+ip*mxfp)                              !/*mw4b       11*/
      e(i)=tal(ltal+jptal(ljpt+5,k)+2*mxf+m+ip*mxfp)                            !/*mw4b       12*/
      if(e(i).le.0.)y(i)=0.                                                     !/*mw         65*/
      if(y(i).ne.0.)e(i)=min(e(i)/y(i)**2-fp,one)                               !/*mw         66*/
      if(e(i).gt.0.)e(i)=sqrt(e(i))                                             !/*mw         67*/
      ir=1+(m-1)/iptal(lipt+2,5,k)                                              !/*mw         68*/
      is=1+mod(m-1,iptal(lipt+4,5,k))/iptal(lipt+5,5,k)                         !/*mw         69*/
      if(iptal(lipt+4,2,k).ne.0)                                                !/*mw         70*/
     & y(i)=y(i)/tds(iptal(lipt+4,2,k)+iptal(lipt+4,3,k)*(ir-1)+is)             !/*mw         71*/
      if((jptal(ljpt+2,k).eq.6.or.jptal(ljpt+2,k).eq.7).and.                    !/*mw         72*/
     & jptal(ljpt+4,k).eq.1)y(i)=y(i)*1.60219e-22                               !/*mwx        42*/
      y(i)=y(i)*fp                                                              !/*mw         74*/
   80 if(i.eq.4.or.m.eq.iptal(lipt+1,5,k))                                      !/*mw         75*/
     & write(iut,'(4(1pe13.5,0pf7.4))')(y(j),e(j),j=1,i)                        !/*mw         76*/
!                                                                               !/*mw         77*/
!        write tally fluctuation data.                                          !/*mw         78*/
      n=0                                                                       !/*mw         79*/
      do 90 i=1,20                                                              !/*mw         80*/
   90 if(npc(i).ne.0)n=i                                                        !/*mw         81*/
      if(n.ne.0.and.mct.lt.0)write(iut,'(3htfc,i5,8i8/(i11,1p2e13.5))')         !/*cor4-2    216*/
     & n,(jtf(ljtf+i,k),i=1,8),(npc(i),(tfc(ltfc+j,i,kp),j=1,2),i=1,n)          !/*mw4b       13*/
      if(n.ne.0.and.mct.ge.0)write(iut,'(3htfc,i5,8i8/(i11,1p3e13.5))')         !/*mw4a        7*/
     & n,(jtf(ljtf+i,k),i=1,8),(npc(i),(tfc(ltfc+j,i,kp),j=1,3),i=1,n)          !/*mw4b       14*/
  100 continue                                                                  !/*mw         85*/
!                                                                               !/*mw         86*/
!        write kcode data.                                                      !/*mw         87*/
      if(nsr.ne.71)go to 120                                                    !/*mw4b       15*/
      l=19                                                                      !/*mw4c        7*/
      write(iut,'(5hkcode,3i5)')kcz,ikz,l                                       !/*mwx        45*/
      do 110 j=1,min(mrkp,kcz)                                                  !/*mw4b       22*/
      if(mct.lt.0)rkpl(lrkp+19,j)=0.                                            !/*mw4c       11*/
      if(mct.lt.0)rkpl(lrkp+29,j)=0.                                            !/*mw4c       12*/
  110 write(iut,'(1p5e12.5)')(rkpl(lrkp+i,j),i=1,l)                             !/*mwx        49*/
  120 close(iut)                                                                !/*mw4b       25*/
      return                                                                    !/*mw         91*/
      end                                                                       !/*mw         92*/
