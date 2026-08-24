!#ident binlin.F from deck bl                                                   !/*bl          1*/
      subroutine binlin(ib,jb,hs)                                               !/*bl          2*/
!        print a line describing bin ib of bin type jb of tally ital.           !/*bl          3*/
!        if hs is not blank, print the bin designator on the line.              !/*bl          4*/
      use global_data                                                           !/*blx         1*/
!                                                                               !/*blx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*blx         3*/
      implicit integer(kindi) (i-n)                                             !/*blx         4*/
!                                                                               !/*blx         5*/
      dimension ll(8)                                                           !/*bl          6*/
      character hs*(*),ht*119,hb(3)*6,hq(3)*3,hf*7,hz*7                         !/*bl          7*/
      data hb/'cosine','energy','time'/,hq/'-1.',' 0.','-i'/                    !/*bl          8*/
      data ll/7,0,0,12,18,11,11,9/                                              !/*bl          9*/
!                                                                               !/*bl         10*/
      ht=' '                                                                    !/*bl         11*/
      iy=jptal(ljpt+2,ital)                                                     !/*bl         12*/
      l=ll(abs(jb))                                                             !/*bl4a        1*/
      if(hs.ne.' ')l=l+10                                                       !/*bl         14*/
      go to(10,70,130,160,190,260,260,260)abs(jb)                               !/*bl4a        2*/
!                                                                               !/*bl         16*/
! >>>>>  jb=1 -- cell, surface, or detector bin.                                !/*bl         17*/
   10 if(iy.ne.5)go to 40                                                       !/*bl         18*/
      i=iptal(lipt+1,1,ital)+5*(ib-1)                                           !/*bl         19*/
!                                                                               !/*blx         6*/
#ifdef RADIOG                                                                   /*!blx         7*/
      if (jptal(ljpt+8,ital).gt.2) then                                         !/*blx         8*/
         i=iptal(lipt+1,1,ital)+19*(ib-1)                                       !/*blx         9*/
         if(jptal(ljpt+8,ital).eq.3)write(iuo,12)(tds(i+j),j=1,3),              !/*blx        10*/
     &    (tds(i+3+k),k=1,3),tds(i+16),tds(i+17)                                !/*blx        11*/
   12    format(1x,'Pinhole radiography image detector:'/                       !/*blx        12*/
     &    5x,'Pinhole centered at x,y,z =',1p3e12.5/                            !/*blx        13*/
     &    5x,'Image grid centered at x,y,z =',3e12.5/                           !/*blx        14*/
     &    5x,'Pinhole, collimator radii =',2e12.5// )                           !/*blx        15*/
         if(jptal(ljpt+8,ital).eq.4)write(iuo,14)(tds(i+j),j=1,3)               !/*blx        16*/
   14    format(1x,'Shadow radiography image detector:'/                        !/*blx        17*/
     &    5x,'Image grid centered at x,y,z =',1p3e12.5// )                      !/*blx        18*/
         nt=iptal(lipt+6,3,ital)                                                !/*blx        19*/
         ns=iptal(lipt+4,3,ital)                                                !/*blx        20*/
         lt=iptal(lipt+6,1,ital)                                                !/*blx        21*/
         ls=iptal(lipt+4,1,ital)                                                !/*blx        22*/
         write(iuo,16)nt,tds(lt),tds(lt+nt),ns,tds(ls),tds(ls+ns)               !/*blx        23*/
   16    format(10x,'There are ',i6,' grid points along the t-axis'/5x,         !/*blx        24*/
     &    'starting at ',1pe12.5,' and ending at ',1pe12.5/1h /10x,             !/*blx        25*/
     &    'There are ',i6,' grid points along the s-axis'/5x,                   !/*blx        26*/
     &    'starting at ',1pe12.5,' and ending at ',1pe12.5/1h / )               !/*blx        27*/
      endif                                                                     !/*blx        28*/
!                                                                               !/*blx        29*/
#endif                                                                          /*!blx        30*/
      if(jptal(ljpt+8,ital).eq.1)write(iuo,20)(tds(i+j),j=1,3)                  !/*blx        31*/
   20 format(28h detector located at x,y,z =,1p3e12.5)                          !/*bl         21*/
      k=tds(i+3)                                                                !/*bl         22*/
      if(jptal(ljpt+8,ital).eq.2)write(iuo,30)                                  !/*bl         23*/
     & char(ichar('w')+k),char(ichar('w')+k),tds(i+1),tds(i+2)                  !/*bl         24*/
   30 format(26h detector symmetric about ,a1,17h-axis located at ,a1,          !/*bl         25*/
     & 2h =,1pe12.5,14h with radius =,e12.5)                                    !/*bl         26*/
      return                                                                    !/*bl         27*/
   40 if(iy.lt.4)ht(l-5:l+1)='surface'                                          !/*bl         28*/
      if(iy.ge.4)ht(l-5:l-2)='cell'                                             !/*bl         29*/
      if(iy.lt.4)l=l+3                                                          !/*bl         30*/
      if(ib.eq.iptal(lipt+1,4,ital))ht(l:l+10)='union total'                    !/*bl4a        3*/
      if(ib.eq.iptal(lipt+1,4,ital))go to 280                                   !/*bl4a        4*/
      if(hs.eq.' ')go to 50                                                     !/*bl         31*/
      hz=hs                                                                     !/*bl         32*/
      ht(l:l+4)=hz(6:7)//' is'                                                  !/*bl         33*/
      l=l+6                                                                     !/*bl         34*/
   50 i=itds(iptal(lipt+1,1,ital)+ib)                                           !/*bl         35*/
      n=itds(i+itds(i)+1)+1                                                     !/*bl4a        5*/
      if(jb.lt.0)n=1                                                            !/*bl4a        6*/
      if(n.gt.1)ht(l:l)='('                                                     !/*bl4a        7*/
      do 65 il=1,n                                                              !/*bl4a        8*/
      j=i                                                                       !/*bl4a        9*/
      if(il.gt.1)j=itds(i+itds(i)+il)                                           !/*bl4a       10*/
      n2=itds(j)                                                                !/*bl4a       11*/
      if(n.eq.1.and.n2.gt.1)ht(l:l)='('                                         !/*bl4a       12*/
      do 60 in=1,n2                                                             !/*bl4a       13*/
      j=j+1                                                                     !/*bl4a       14*/
      kfq=0                                                                     !/*bl4c        1*/
      if(iy.gt.3.or.il.ne.1)go to 53                                            !/*bl4c        2*/
      r=nsf(lnsf+itds(j))                                                       !/*bl4c        3*/
   53 if(iy.gt.3.or.il.gt.1)r=ncl(lncl+abs(itds(j)))                            !/*bl4c        4*/
      l1=log10(.5+r)+1.                                                         !/*bl4a       17*/
      if(l+l1+1.gt.119)call begone(ht,l)                                        !/*bl4a       18*/
      if(kfq.ne.0.and.l+l1+3.gt.119)call begone(ht,l)                           !/*bl4c        5*/
      write(hf,'(i7)')nint(r)                                                   !/*bl4a       19*/
      ht(l+1:l+l1)=hf(8-l1:7)                                                   !/*bl4a       20*/
      l=l+l1+1                                                                  !/*bl4a       21*/
      if(kfq.ne.0)write(ht(l:l+1),'(1h.,i1)')kfq                                !/*bl4c        6*/
      if(kfq.ne.0)l=l+2                                                         !/*bl4c        7*/
      if(itds(j).gt.0)go to 60                                                  !/*bl4a       22*/
      ht(l:l)='['                                                               !/*bl4a       23*/
      do 55 m=1,3                                                               !/*bl4a       24*/
      r=abs(itds(j+m))                                                          !/*bl4a       25*/
      l1=log10(.5+r)+1.                                                         !/*bl4a       26*/
      if(itds(j+m).le.0)l1=l1+1                                                 !/*bl4a       27*/
      if(l+l1+1.gt.119)call begone(ht,l)                                        !/*bl4a       28*/
      write(hf,'(i7)')itds(j+m)                                                 !/*bl4a       29*/
      ht(l+1:l+l1)=hf(8-l1:7)                                                   !/*bl4a       30*/
   55 l=l+l1+1                                                                  !/*bl4a       31*/
      ht(l:l)=']'                                                               !/*bl4a       32*/
      if(l+1.gt.119)call begone(ht,l)                                           !/*bl4a       33*/
      l=l+1                                                                     !/*bl4a       34*/
      j=j+3                                                                     !/*bl4a       35*/
   60 continue                                                                  !/*bl4a       36*/
   65 if(n.gt.1)ht(l:l)='<'                                                     !/*bl4a       37*/
      if(n.gt.1.or.n2.gt.1)ht(l:l)=')'                                          !/*bl4a       38*/
      go to 280                                                                 !/*bl         47*/
!                                                                               !/*bl         48*/
! >>>>>  jb=2 -- total vs. flagged or total vs. direct.                         !/*bl         49*/
   70 if(ib.eq.1)return                                                         !/*bl         50*/
      if(iy.eq.5)go to 80                                                       !/*bl         51*/
      write(iuo,'(16h flagged tallies)')                                        !/*bl         52*/
      return                                                                    !/*bl         53*/
   80 continue                                                                  !/*blx        32*/
#ifdef RADIOG                                                                   /*!blx        33*/
      if(jptal(ljpt+8,ital).gt.2)go to 124                                      !/*blx        34*/
#endif                                                                          /*!blx        35*/
      if(ktp(lktp+1,ital).eq.0)go to 100                                        !/*blx        36*/
      write(iuo,90)                                                             !/*bl         55*/
   90 format(24h uncollided neutron flux)                                       !/*bl         56*/
      return                                                                    !/*bl         57*/
  100 if(ecf(2).lt.emcf(2).and.nocoh.eq.0)write(iuo,110)                        !/*bl4a       39*/
  110 format(23h uncollided photon flux)                                        !/*bl         59*/
      if(ecf(2).ge.emcf(2).or.nocoh.eq.1)write(iuo,120)                         !/*bl4a       40*/
  120 format(51h uncollided photon flux without coherent scattering)            !/*bl         61*/
      return                                                                    !/*bl         62*/
#ifdef RADIOG                                                                   /*!blx        37*/
!                                                                               !/*blx        38*/
!       Give proper title to direct ray-trace contributions.                    !/*blx        39*/
  124 write(iuo,126)                                                            !/*blx        40*/
  126 format(35h Direct ray-trace source image flux)                            !/*blx        41*/
      return                                                                    !/*blx        42*/
#endif                                                                          /*!blx        43*/
!                                                                               !/*bl         63*/
! >>>>>  jb=3 -- user bin.                                                      !/*bl         64*/
  130 if(iptal(lipt+3,1,ital).lt.0)return                                       !/*bl         65*/
      if(ib.ne.iptal(lipt+3,4,ital))write(iuo,140)                              !/*bl         66*/
     & tds(iptal(lipt+3,1,ital)+ib)                                             !/*bl         67*/
  140 format(9h user bin,1pe13.5)                                               !/*bl         68*/
      if(ib.eq.iptal(lipt+3,4,ital))write(iuo,150)                              !/*bl         69*/
  150 format(15h user bin total)                                                !/*bl         70*/
      return                                                                    !/*bl         71*/
!                                                                               !/*bl         72*/
! >>>>>  jb=4 -- segment bin.                                                   !/*bl         73*/
  160 continue                                                                  !/*blx        44*/
#ifdef RADIOG                                                                   /*!blx        45*/
      if(iy.eq.5)go to 275                                                      !/*blx        46*/
#endif                                                                          /*!blx        47*/
      ht(l-10:l-4)='segment'                                                    !/*blx        48*/
      if(hs.ne.' ')write(ht(l-2:l-1),'(i2)')ib                                  !/*bl         75*/
      if(hs.ne.' ')l=l+3                                                        !/*bl         76*/
      ht(l-3:l-3)=':'                                                           !/*bl         77*/
      if(ib.ne.iptal(lipt+4,4,ital))go to 170                                   !/*bl         78*/
      ht(l+1:l+5)='whole'                                                       !/*bl         79*/
      if(iy.lt.4)ht(l+7:l+13)='surface'                                         !/*bl         80*/
      if(iy.ge.4)ht(l+7:l+10)='cell'                                            !/*bl         81*/
      l=l+16                                                                    !/*bl         82*/
      go to 280                                                                 !/*bl         83*/
  170 i=abs(lca(llca+iptal(lipt+4,1,ital)))                                     !/*bl         84*/
      do 180 m=1,min(ib,iptal(lipt+4,4,ital)-2)                                 !/*bl         85*/
      if(l+9.gt.119)call begone(ht,l)                                           !/*bl4c        8*/
      k=sign(nsf(lnsf+abs(lja(llja+i+m-1))),lja(llja+i+m-1))                    !/*bl         87*/
      if(m.lt.ib)k=-k                                                           !/*bl         88*/
      write(hf(1:7),'(i7)')k                                                    !/*bl4c        9*/
      ht(l+3:l+9)=hf(1:7)                                                       !/*bl4c       10*/
      if(kfq.ne.0)write(ht(l+1:l+9),'(a7,1h.,i1)')hf(1:7),kfq                   !/*bl4c       11*/
  180 l=l+9                                                                     !/*bl4c       12*/
      go to 280                                                                 !/*bl         91*/
!                                                                               !/*bl         92*/
! >>>>>  jb=5 -- multiplier bin.                                                !/*bl         93*/
  190 km=0                                                                      !/*bl         94*/
      do 210 j=1,ib                                                             !/*bl         95*/
  200 km=km+1                                                                   !/*bl         96*/
  210 if(tds(iptal(lipt+5,1,ital)+km).lt.-2.5)go to 200                         !/*bl4b        1*/
      ip=tds(iptal(lipt+5,1,ital)+km)                                           !/*bl         98*/
      ht(l-16:l-3)='multiplier bin'                                             !/*bl         99*/
      if(hs.ne.' ')write(ht(l-1:l),'(i2)')ib                                    !/*bl        100*/
      if(hs.ne.' ')l=l+3                                                        !/*bl        101*/
      ht(l-2:l-2)=':'                                                           !/*bl        102*/
      ht(l+1:l+5)='total'                                                       !/*bl        103*/
      if(ib.eq.iptal(lipt+5,4,ital))go to 280                                   !/*bl        104*/
      write(ht(l+1:l+12),'(1pe12.5)')tds(iptal(lipt+5,2,ital)+km)               !/*bl        105*/
      if(ip.lt.0)write(ht(l+14:l+15),'(i2)')ip                                  !/*bl4b        2*/
      if(ip.le.0)go to 280                                                      !/*bl        107*/
      if(tds(ip-1).lt.0)go to 240                                               !/*bl        108*/
!                                                                               !/*bl        109*/
!        material-reaction multipliers.                                         !/*bl        110*/
      write(ht(l+14:l+17),'(i4)')nmt(lnmt+int(tds(ip-1)))                       !/*bl        111*/
      l=l+21                                                                    !/*bl        112*/
      do 220 i=1,mod(int(abs(tds(ip))),10000000)                                !/*bl        113*/
      if(l+5.gt.119)call begone(ht,l)                                           !/*bl        114*/
      k=tds(ip+i)                                                               !/*bl        115*/
      if(k.eq.1000003)ht(l+5:l+5)=':'                                           !/*bl        116*/
      if(k.ne.1000003)write(ht(l+1:l+7),'(i7)')k                                !/*bl        117*/
  220 l=l+7                                                                     !/*bl        118*/
      if(abs(tds(ip)).lt.10000000)go to 280                                     !/*bl        119*/
      call begone(ht,l)                                                         !/*bl        120*/
      i=-1                                                                      !/*bl        121*/
      if(abs(tds(ip)).lt.20000000)i=1                                           !/*bl        122*/
      j=km                                                                      !/*bl        123*/
  230 j=j+i                                                                     !/*bl        124*/
      ip=-tds(iptal(lipt+5,1,ital)+j)                                           !/*bl        125*/
      if(ip.le.1)go to 230                                                      !/*bl        126*/
      l=21                                                                      !/*bl        127*/
      if(hs.ne.' ')l=l+10                                                       !/*bl        128*/
      ht(l-17:l-2)='with attenuator:'                                           !/*bl        129*/
      write(ht(l+1:l+12),'(1pe12.5)')tds(iptal(lipt+5,2,ital)+j)                !/*bl        130*/
!                                                                               !/*bl        131*/
!        material-rho*x pairs for line-of-sight attenuators.                    !/*bl        132*/
  240 l=l+13                                                                    !/*bl        133*/
      do 250 i=1,int(abs(tds(ip))),2                                            !/*bl        134*/
      if(l+16.gt.119)call begone(ht,l)                                          !/*bl        135*/
      write(ht(l+1:l+16),'(i4,1pe12.5)')nmt(lnmt+int(tds(ip+i))),               !/*bl        136*/
     & tds(ip+i+1)                                                              !/*bl        137*/
  250 l=l+16                                                                    !/*bl        138*/
      go to 280                                                                 !/*bl        139*/
!                                                                               !/*bl        140*/
! >>>>>  jb=6, 7, or 8 -- cosine, energy, or time bin.                          !/*bl        141*/
  260 ht(2:7)=hb(jb-5)                                                          !/*bl        142*/
#ifdef RADIOG                                                                   /*!blx        49*/
      if(iy.eq.5.and.jb.eq.6)go to 275                                          !/*blx        50*/
#endif                                                                          /*!blx        51*/
      ht(l-2:l+1)='bin:'                                                        !/*bl        143*/
      if(ib.ne.iptal(lipt+jb,4,ital))go to 270                                  !/*bl        144*/
      ht(l+4:l+8)='total'                                                       !/*bl        145*/
      go to 280                                                                 !/*bl        146*/
  270 if(ib.eq.1)ht(l+4:l+6)=hq(jb-5)                                           !/*bl        147*/
      if(ib.gt.1)write(ht(l+4:l+15),'(1pe12.5)')                                !/*bl        148*/
     & tds(iptal(lipt+jb,1,ital)+ib-1)                                          !/*bl        149*/
      ht(l+17:l+18)='to'                                                        !/*bl        150*/
      write(ht(l+20:l+31),'(1pe12.5)')tds(iptal(lipt+jb,1,ital)+ib)             !/*bl        151*/
      if(iptal(lipt+jb,2,ital).eq.0)go to 280                                   !/*bl        152*/
      ht(l+38:l+49)='multiplier ='                                              !/*bl        153*/
      write(ht(l+51:l+62),'(1pe12.5)')tds(iptal(lipt+jb,2,ital)+ib)             !/*bl        154*/
#ifdef RADIOG                                                                   /*!blx        52*/
      go to 280                                                                 !/*blx        53*/
!                                                                               !/*blx        54*/
!        s- and t- fluor axis bins for image detector tally.                    !/*blx        55*/
  275 if(jb.eq.4)ht(2:7)='s-axis'                                               !/*blx        56*/
      if(jb.eq.6)ht(2:7)='t-axis'                                               !/*blx        57*/
      if(ib.ne.iptal(lipt+jb,4,ital))go to 278                                  !/*blx        58*/
      ht(l+4:l+8)='total'                                                       !/*blx        59*/
      go to 280                                                                 !/*blx        60*/
  278 write(ht(l+4:l+15),'(1pe12.5)')tds(iptal(lipt+jb,1,ital)+ib-1)            !/*blx        61*/
      ht(l+17:l+18)='to'                                                        !/*blx        62*/
      write(ht(l+20:l+31),'(1pe12.5)')tds(iptal(lipt+jb,1,ital)+ib)             !/*blx        63*/
#endif                                                                          /*!blx        64*/
!                                                                               !/*bl        155*/
!        flush the line buffer.                                                 !/*bl        156*/
  280 call begone(ht,l)                                                         !/*bl        157*/
      return                                                                    !/*bl        158*/
      end                                                                       !/*bl        159*/
