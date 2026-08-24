!#ident tallyh.F from deck th                                                   !/*th          1*/
      subroutine tallyh                                                         !/*th          2*/
!        print the tally heading.                                               !/*th          3*/
      use global_data                                                           !/*thx         1*/
!                                                                               !/*thx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*thx         3*/
      implicit integer(kindi) (i-n)                                             !/*thx         4*/
!                                                                               !/*thx         5*/
      character hk(2,8)*41,hu(2,8)*10,hs(2)*2,                                  !/*thx         6*/
#ifdef RADIOG                                                                   /*!thx         7*/
     & hd(5)*5,                                                                 !/*thx         8*/
#else                                                                           /*!thx         9*/
     & hd(2)*5,                                                                 !/*thx        10*/
#endif                                                                          /*!thx        11*/
     & hv*18,ht*75,hb(mipt)*11,hq*80,hf*9                                       !/*thx        12*/
      data hu/' ','mev','1/cm**2','mev/cm**2',2*' ','1/cm**2',                  !/*th          7*/
     & 'mev/cm**2','1/cm**2','mev/cm**2','mev/gram','jerks/gram',               !/*th          8*/
     & 'mev/gram','jerks/gram','number','mev'/                                  !/*th          9*/
      data hk/'number of particles crossing a surface.',                        !/*th         10*/
     & 'energy crossing a surface.',                                            !/*th         11*/
     & 'particle flux averaged over a surface.',                                !/*th         12*/
     & 'energy flux averaged over a surface.',2*' ',                            !/*th         13*/
     & 'track length estimate of particle flux.',                               !/*th         14*/
     & 'track length estimate of energy flux.',                                 !/*th         15*/
     & 'particle flux at a       detector.',                                    !/*th         16*/
     & 'energy flux at a       detector.',                                      !/*th         17*/
     & 2*'track length estimate of heating.',                                   !/*th         18*/
     & 2*'track length estimate of fission heating.',                           !/*th         19*/
     & 'pulse height distribution.','energy deposition'/                        !/*th         20*/
#ifdef RADIOG                                                                   /*!thx        13*/
      data hd/'point','ring','image','image','image'/                           !/*thx        14*/
#else                                                                           /*!thx        15*/
      data hd/'point','ring'/                                                   !/*thx        16*/
#endif                                                                          /*!thx        17*/
      data hs/' ','*'/                                                          !/*thx        18*/
!                                                                               !/*th         22*/
!        print tally number and user-supplied description, if any.              !/*th         23*/
      k=jptal(ljpt+1,ital)                                                      !/*th         24*/
      l=jptal(ljpt+6,ital)                                                      !/*th         25*/
      if(nps.eq.0)write(iuo,10)k                                                !/*th         26*/
   10 format(6h1tally,i4,94x,14hprint table 30)                                 !/*th         27*/
      if(nps.gt.0)write(iuo,20)k,nps                                            !/*th         28*/
   20 format(6h1tally,i4,8x,5hnps =,i9)                                         !/*th         29*/
      if(l.eq.0)go to 50                                                        !/*th         30*/
      n=itds(l)                                                                 !/*th         31*/
      hv='+'                                                                    !/*th         32*/
      do 40 i=l+1,l+n,25                                                        !/*th         33*/
      do 30 j=i,i+24                                                            !/*th         34*/
   30 ht((j-i)*3+1:(j-i)*3+3)=char(itds(j)/65536)//                             !/*th         35*/
     & char(mod(itds(j),65536)/256)//char(mod(itds(j),256))                     !/*th         36*/
      write(iuo,'(a1,35x,a75)')hv,ht                                            !/*th         37*/
   40 hv=' '                                                                    !/*th         38*/
!                                                                               !/*th         39*/
!        print tally type and units.                                            !/*th         40*/
   50 iy=jptal(ljpt+2,ital)                                                     !/*th         41*/
      js=min(jptal(ljpt+4,ital),1)                                              !/*th4a        1*/
      if(iy.eq.6.and.jptal(ljpt+4,ital).eq.2)js=0                               !/*thx        19*/
      if(iy.eq.5)hk(js+1,5)(20-2*js:24-2*js)=hd(jptal(ljpt+8,ital))             !/*th         43*/
#ifdef PCDOS                                                                    /*!th4a        2*/
      if(lockl)call pttyin                                                      !/*th4a        3*/
#endif                                                                          /*!cpp       117*/
      if(irup.ne.0)call qttyin(iy,'16hprocessing tally,i5')                     !/*th4a        4*/
      hv=' '                                                                    !/*th         44*/
      if(nps.eq.0.or.iptal(lipt+5,2,ital).ne.0.or.                              !/*th         45*/
     & iptal(lipt+6,2,ital).ne.0.or.                                            !/*th         46*/
     & iptal(lipt+7,2,ital).ne.0.or.iptal(lipt+8,2,ital).ne.0.or.               !/*th         47*/
     & iptal(lipt+3,2,ital).ne.0.or.iy.eq.1.and.js.eq.0)go to 60                !/*th         48*/
      hv='units   '//hu(js+1,iy)                                                !/*th         49*/
   60 n=0                                                                       !/*th         50*/
      do 70 i=1,mipt                                                            !/*th         51*/
#ifdef MESHTAL                                                                  /*!thx        20*/
      if (iy.eq.6.and.jptal(ljpt+4,ital).eq.2) then                             !/*thx        21*/
         if(kpt(i).eq.0.or.(i.eq.2.and.kpt(3).ne.0))go to 70                    !/*thx        22*/
      else                                                                      !/*thx        23*/
         if(ktp(lktp+i,ital).eq.0)go to 70                                      !/*thx        24*/
      endif                                                                     !/*thx        25*/
#else                                                                           /*!thx        26*/
      if(ktp(lktp+i,ital).eq.0)go to 70                                         !/*th         52*/
#endif                                                                          /*!thx        27*/
      n=n+1                                                                     !/*th         53*/
      hb(n)=hnp(i)                                                              !/*thx        28*/
   70 continue                                                                  !/*th         55*/
      if (iy.eq.6) then                                                         !/*thx        29*/
         if(jptal(ljpt+4,ital).eq.2)write(iuo,80)iy,'+',hk(2,8),hv              !/*thx        30*/
         if (jptal(ljpt+4,ital).ne.2) then                                      !/*thx        31*/
            if (ktp(lktp+1,ital).eq.0.and.ktp(lktp+2,ital).eq.0) then           !/*thx        32*/
               write(iuo,80)iy,' ',hk(2,8),hv                                   !/*thx        33*/
            else                                                                !/*thx        34*/
               write(iuo,80)iy,hs(js+1),hk(js+1,iy),hv                          !/*thx        35*/
            endif                                                               !/*thx        36*/
         endif                                                                  !/*thx        37*/
      else                                                                      !/*thx        38*/
         if(jptal(ljpt+4,ital).ne.2)write(iuo,80)iy,hs(js+1),                   !/*thx        39*/
     &    hk(js+1,iy),hv                                                        !/*thx        40*/
         if(jptal(ljpt+4,ital).eq.2)write(iuo,80)iy,'+',                        !/*thx        41*/
     &    'charge deposition','units   charge'                                  !/*thx        42*/
      endif                                                                     !/*thx        43*/
   80 format(11x,10htally type,i2,a2,2x,a41,4x,a18)                             !/*th         57*/
      if(dbcn(26).ne.0.)then                                                    !/*thx        44*/
      do 89 i=1,n                                                               !/*thx        45*/
   89 hb(i)=hb(i)(1:leng(hb(i)))//'s'                                           !/*thx        46*/
      write(iuo,'(11x,10htally for ,9(1x,a9))')(hb(i),i=1,n)                    !/*thx        47*/
      else                                                                      !/*thx        48*/
      write(iuo,90)(hb(i),i=1,n)                                                !/*thx        49*/
   90 format(11x,12hparticle(s):,8(1x,a11)/(23x,8(1x,a11)))                     !/*thx        50*/
      endif                                                                     !/*thx        51*/
!                                                                               !/*th         60*/
!        print lists of flagging cells and surfaces.                            !/*th         61*/
      if(iy.eq.5)go to 130                                                      !/*th4c        1*/
      if(iptal(lipt+2,1,ital).le.0)go to 110                                    !/*th         63*/
      l=iptal(lipt+2,1,ital)                                                    !/*th         64*/
      write(iuo,100)(ncl(lncl+itds(l+i)),i=1,itds(l))                           !/*th         65*/
  100 format(11x,15hflagging cells:,15i6/(26x,15i6))                            !/*th         66*/
  110 if(iptal(lipt+2,2,ital).eq.0)go to 130                                    !/*th4c        2*/
      l=iptal(lipt+2,2,ital)                                                    !/*th         68*/
!                                                                               !/*th4c2       2*/
!        include the possibility of a surface flagging facet.                   !/*th4c2       3*/
      m=itds(l)                                                                 !/*th4c2       4*/
      do 120 i=1,m                                                              !/*th4c2       5*/
      n=mod(i-1,8)+1                                                            !/*th4c2       6*/
      ic=nsf(lnsf+itds(l+i))                                                    !/*th4c2       7*/
      if(n.eq.1)hq=' '                                                          !/*th4c2       8*/
      hf=' '                                                                    !/*th4c2       9*/
      if(kfq.eq.0)write(hf,'(i7,2h  )')ic                                       !/*th4c2      10*/
      if(kfq.ne.0)write(hf,'(i7,1h.,i1)')ic,kfq                                 !/*th4c2      11*/
      hq(1+(n-1)*9:n*9)=hf(1:9)                                                 !/*th4c2      12*/
      if(i.le.8.and.(n.eq.8.or.i.eq.m))write(iuo,115)hq                         !/*th4c2      13*/
  115 format(11x,18hflagging surfaces:,a80)                                     !/*th4c2      14*/
      if(i.gt.8.and.(n.eq.8.or.i.eq.m))write(iuo,'(29x,a80)')hq                 !/*th4c2      15*/
  120 continue                                                                  !/*th4c2      16*/
!                                                                               !/*th4c        3*/
!        print which bins are cumulative.                                       !/*th4c        4*/
  130 do 140 i=3,8                                                              !/*th4c        5*/
  140 if(iptal(lipt+i,6,ital).ne.0)write(iuo,150)ilbl(i)                        !/*th4c        6*/
  150 format(9x,a8,21h bins are cumulative.)                                    !/*th4c        7*/
      return                                                                    !/*th         71*/
      end                                                                       !/*th         72*/
