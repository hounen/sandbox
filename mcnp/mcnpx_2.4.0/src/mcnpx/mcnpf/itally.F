!#ident itally.F from deck it                                                   !/*it          1*/
      subroutine itally                                                         !/*it          2*/
!        process the tally specifications.                                      !/*it          3*/
!        the ipnt(lipn+2,j,ital) items from each tally card are in rtp,         !/*it          4*/
!        starting at rtp(lrtp+ipnt(lipn+1,j,ital)).                             !/*it          5*/
!        card:  f fx fy fz c e t fs fc cf sf fm sd cm em tm pd fu fq            !/*it          6*/
!           j:  2  2  2  2 3 4 5  6  7  8  9 10 11 12 13 14 15 16 17            !/*it          7*/
!        card:  de df tf dd ft                                                  !/*it          8*/
!           j:  18 19 20 21 22                                                  !/*it          9*/
!        ipnt(lipn+1,1,ital) contains bits for totals over bins.                !/*it         10*/
#ifdef RADIOG                                                                   /*!itx         1*/
!        ipnt(lipn+2,1,ital)= 1 - 7 for f,fx,fy,fz,pi,tir,tic; < 0 if *         !/*itx         2*/
#else                                                                           /*!itx         3*/
!        ipnt(lipn+2,1,ital) = 1,2,3,4 for f,fx,fy,fz. neg if *                 !/*itx         4*/
#endif                                                                          /*!itx         5*/
      use global_data                                                           !/*itx         6*/
      use imcn_data                                                             !/*itx         7*/
!                                                                               !/*itx         8*/
      implicit real(kindr) (a-h,o-z)                                            !/*itx         9*/
      implicit integer(kindi) (i-n)                                             !/*itx        10*/
!                                                                               !/*itx        11*/
      dimension kd(2,3),mi(8),nz(mkft),ik(6)                                    !/*it4a        1*/
      character hc(3)*6,hf(3)*1                                                 !/*it         16*/
      data hc/'cosine','energy',' time'/,hf/'c','e','t'/                        !/*it         17*/
      data kd/6*0/,mi/16,8,0,0,8,0,0,24/,kl/0/,jd/0/,ip/0/,ks/0/                !/*cor4-1     72*/
      data nz/3,3,2,0,0,1,0,1,0/                                                !/*it4a        2*/
      jb(i,j)=mod(ipnt(lipn+1,1,j)/2**(i-1),2)                                  !/*it         20*/
!                                                                               !/*it         21*/
      mxafs=mxa                                                                 !/*it         22*/
#ifdef MESHTAL                                                                  /*!itx        12*/
      if (nmesh.eq.0) then                                                      !/*itx        13*/
         if(ntal.eq.0.and.nsr.ne.71)call erprnt(1,2,0,0,0,0,0,0,                !/*itx        14*/
     &    '37hthere are no tallies in this problem.')                           !/*itx        15*/
      else                                                                      !/*itx        16*/
         if(ntal.eq.0.and.nsr.ne.71)call erprnt(1,2,0,0,0,0,0,0,                !/*itx        17*/
     &    '44hthere are only mesh tallies in this problem.')                    !/*itx        18*/
      endif                                                                     !/*itx        19*/
#else                                                                           /*!itx        20*/
      if(ntal.eq.0.and.nsr.ne.71)call erprnt(1,2,0,0,0,0,0,0,                   !/*it         23*/
     & '37hthere are no tallies in this problem.')                              !/*it         24*/
#endif                                                                          /*!itx        21*/
      li=litd+1                                                                 !/*it         25*/
      ld=ltds+1                                                                 !/*it         26*/
#ifndef MESHTAL                                                                 /*!itx        22*/
      do 930 ital=1,ntal                                                        !/*it         27*/
#else                                                                           /*!itx        23*/
      do 930 ital=1,ntal+nmesh                                                  !/*itx        24*/
      if(ital.gt.ntal)go to 380                                                 !/*itx        25*/
#endif                                                                          /*!itx        26*/
!                                                                               !/*it         28*/
!        set up the basic numbers in jptal.                                     !/*it         29*/
      jptal(ljpt+1,ital)=abs(jptal(ljpt+1,ital))                                !/*it         30*/
      kl=jptal(ljpt+1,ital)                                                     !/*it         31*/
      iy=mod(kl,10)                                                             !/*it         32*/
      jptal(ljpt+2,ital)=iy                                                     !/*it         33*/
      if(iy.eq.7)itfxs=1                                                        !/*it         34*/
      np=ktp(lktp+1,ital)+2*ktp(lktp+2,ital)                                    !/*itx        27*/
      jptal(ljpt+4,ital)=min(1,max(0,-ipnt(lipn+2,1,ital)))                     !/*it         36*/
#ifdef MESHTAL                                                                  /*!itx        28*/
      if((iy.eq.8.or.iy.eq.6).and.                                              !/*itx        29*/
#else                                                                           /*!itx        30*/
      if(iy.eq.8.and.                                                           !/*itx        31*/
#endif                                                                          /*!itx        32*/
     & ipnt(lipn+2,1,ital).eq.9)jptal(ljpt+4,ital)=2                            !/*itx        33*/
      jptal(ljpt+5,ital)=mxf                                                    !/*it         37*/
      if(iy.eq.5)jptal(ljpt+8,ital)=min(abs(ipnt(lipn+2,1,ital)),2)             !/*it         38*/
#ifdef RADIOG                                                                   /*!itx        34*/
      if (iy.eq.5.and.abs(ipnt(lipn+2,1,ital)).ge.5) then                       !/*itx        35*/
         jptal(ljpt+8,ital)=abs(ipnt(lipn+2,1,ital))-2                          !/*itx        36*/
      endif                                                                     !/*itx        37*/
#endif                                                                          /*!itx        38*/
!                                                                               !/*it4a        4*/
!        set ddm(lddm+1,ital)=-huge for tallies that are all negative.          !/*it4a        5*/
      do 5 i=0,npert                                                            !/*it4b        1*/
    5 ddm(lddm+1,ital+i*ntal)=-huge                                             !/*it4b        2*/
!                                                                               !/*it         39*/
!        set up the default bin counts.                                         !/*it         40*/
      do 10 i=1,8                                                               !/*it         41*/
      iptal(lipt+i,4,ital)=2                                                    !/*it         42*/
   10 iptal(lipt+i,3,ital)=1                                                    !/*it         43*/
!                                                                               !/*it         44*/
!        process fc data:  tally comment                                        !/*it         45*/
      if(ipnt(lipn+2,7,ital).eq.0)go to 30                                      !/*it         46*/
      jptal(ljpt+6,ital)=li                                                     !/*it         47*/
      itds(li)=ipnt(lipn+2,7,ital)                                              !/*it         48*/
      do 20 i=1,itds(li)                                                        !/*it         49*/
   20 itds(li+i)=rtp(lrtp+ipnt(lipn+1,7,ital)+i-1)                              !/*it         50*/
      li=li+itds(li)+1                                                          !/*it         51*/
!                                                                               !/*it         52*/
!        process cf and sf data:  cell and surface flagging                     !/*it         53*/
   30 do 50 m=1,2                                                               !/*it         54*/
      if(ipnt(lipn+2,7+m,ital).eq.0.or.iy.eq.5)go to 50                         !/*it         55*/
      iptal(lipt+2,3,ital)=2                                                    !/*it         56*/
      iptal(lipt+2,m,ital)=li                                                   !/*it         57*/
      if(m.eq.2.and.iptal(lipt+2,1,ital).eq.0)iptal(lipt+2,1,ital)=-1           !/*it         58*/
      itds(li)=ipnt(lipn+2,7+m,ital)                                            !/*it         59*/
      do 40 i=1,itds(li)                                                        !/*it         60*/
      j=rtp(lrtp+ipnt(lipn+1,7+m,ital)-1+i)                                     !/*it         61*/
      k=namchg(2*m-1,j)                                                         !/*it4c        1*/
      if(k.eq.0)go to 40                                                        !/*it4c        2*/
!                                                                               !/*it4c        3*/
!        if macrobody identical surface, use master surface number.             !/*it4c        4*/
      if(m.ne.2)go to 32                                                        !/*it4c        5*/
!        check if a bounding surface, include facet if macrobody.               !/*it4c2       1*/
      x=rtp(lrtp+ipnt(lipn+1,7+m,ital)-1+i)                                     !/*it4c2       2*/
      jq=(x-j+.001)*10.                                                         !/*it4c2       3*/
      do 31 kk=1,nlja                                                           !/*it4c2       4*/
      k=abs(lja(llja+kk))                                                       !/*it4c2       5*/
      if(k.ge.1000000)go to 31                                                  !/*it4c2       6*/
      if(nsf(lnsf+k).ne.j)go to 31                                              !/*it4c2       7*/
      if(kfq.eq.jq)go to 33                                                     !/*it4c2       8*/
   31 continue                                                                  !/*it4c2       9*/
      k=0                                                                       !/*it4c2      10*/
      go to 40                                                                  !/*it4c2      11*/
   33 if(idns(lids+k).eq.0)go to 32                                             !/*it4c2      12*/
      if(idnt(lidt+k).eq.k)go to 32                                             !/*it4c        7*/
      k=abs(idnt(lidt+k))                                                       !/*it4c        8*/
      j=nsf(lnsf+k)                                                             !/*it4c        9*/
      rtp(lrtp+ipnt(lipn+1,7+m,ital)-1+i)=j                                     !/*it4c       10*/
!                                                                               !/*it4c       11*/
   32 itds(li+i)=k                                                              !/*it4c       12*/
   40 if(m.eq.2.and.k.eq.0)call erprnt(1,1,2,j,kl,0,0,0,                        !/*it4c       13*/
     & 'i7,10h on the sf,i3,33h  card is not a bounding surface.')              !/*it         64*/
      li=li+itds(li)+1                                                          !/*it         65*/
   50 continue                                                                  !/*it         66*/
!                                                                               !/*it4c       14*/
!        set flags for cumulative tallies.                                      !/*it4c       15*/
      do 55 i=3,8                                                               !/*it4c       16*/
      je=ital                                                                   !/*it4c       17*/
      if(i.eq.3.or.i.eq.5)go to 55                                              !/*it4c       18*/
      iw=6                                                                      !/*it4c       19*/
      if(i.gt.5)iw=i-3                                                          !/*it4c       20*/
      if(ipnt(lipn+2,iw,ital).eq.0.and.ipnt(lipn+2,iw,0).ne.0)je=0              !/*it4c       21*/
   55 iptal(lipt+i,6,ital)=jb(i+6,je)                                           !/*it4c       22*/
!                                                                               !/*it         67*/
!        print the page heading, which includes comment and flagging.           !/*it         68*/
      if(ink(30).ne.0)call tallyh                                               !/*it         69*/
!                                                                               !/*it         70*/
!        process f data:  cell, surface, or detector bins                       !/*it         71*/
      if(ipnt(lipn+2,2,ital).eq.0)go to 930                                     !/*it         72*/
      l=ipnt(lipn+1,2,ital)-1                                                   !/*it         73*/
      n1=ipnt(lipn+2,2,ital)                                                    !/*it         74*/
      if(iy.eq.5)go to 140                                                      !/*it         75*/
!                                                                               !/*it         76*/
!        set up the surface or cell bins.                                       !/*it         77*/
      kt=li-1                                                                   !/*it         78*/
      iptal(lipt+1,1,ital)=kt                                                   !/*it         79*/
!                                                                               !/*it4a        7*/
!        complexity of this card requires two loops through the data.           !/*it4a        8*/
      lm=0                                                                      !/*it4a        9*/
      do 117 jl=1,2                                                             !/*it4a       10*/
      if(jl.eq.1)go to 60                                                       !/*it4a       11*/
      li=li+jb(1,ital)                                                          !/*it4a       12*/
      iptal(lipt+1,3,ital)=li-kt-1                                              !/*it4a       13*/
      iptal(lipt+1,4,ital)=li-kt-jb(1,ital)                                     !/*it4a       14*/
   60 m=0                                                                       !/*it4a       15*/
      k1=0                                                                      !/*it4a       16*/
      il=0                                                                      !/*it4a       17*/
!                                                                               !/*it4a       18*/
!        loop over the entries on the f card.                                   !/*it4a       19*/
      do 117 i=1,n1                                                             !/*it4a       20*/
      k=nint(rtp(lrtp+l+i))                                                     !/*it4a       21*/
      kn=0                                                                      !/*it4a       22*/
      if(i.lt.n1)kn=nint(rtp(lrtp+l+i+1))                                       !/*it4a       23*/
      kp=0                                                                      !/*it4a       24*/
      if(i.gt.1)kp=nint(rtp(lrtp+l+i-1))                                        !/*it4a       25*/
      if(k.lt.1000000)go to 86                                                  !/*it4a       26*/
!                                                                               !/*it4a       27*/
!        process character data.                                                !/*it4a       28*/
      go to(61,62,64,117,67,70,72,117,117,85)k-1000000                          !/*it4a       29*/
!                                                                               !/*it4a       30*/
! >>>>>  process (, m=1 for new bin, m=2 for multiple entries per level.        !/*it4a       31*/
   61 if(il.gt.0)go to 116                                                      !/*it4a       32*/
      m=m+1                                                                     !/*it4a       33*/
      go to (88,117,116)m                                                       !/*it4a       34*/
!                                                                               !/*it4a       35*/
! >>>>>  process ), m=0 for end of bin, m=1 for end of level entries.           !/*it4a       36*/
   62 if(il.gt.0)go to 116                                                      !/*it4a       37*/
      m=m-1                                                                     !/*it4a       38*/
      if(k2.eq.0.or.m.eq.1)itds(l3)=-abs(itds(l3))                              !/*it4a       39*/
      go to(116,68,117)m+2                                                      !/*it4a       40*/
!                                                                               !/*it4a       41*/
! >>>>>  process lattice index symbol, :, il=flag for # of colons in [].        !/*it4a       42*/
   64 go to(116,116,65,65,65,116)il+1                                           !/*it4a       43*/
   65 il=il+1                                                                   !/*it4a       44*/
      if(mb.eq.0.or.mb.gt.5.or.kn.eq.1000003)go to 116                          !/*it4a       45*/
      go to 117                                                                 !/*it4a       46*/
!                                                                               !/*it4a       47*/
! >>>>>  process level symbol, <, k2=# levels, itds(l3)=# level entries.        !/*it4a       48*/
   67 if(il.gt.0)go to 116                                                      !/*it4a       49*/
      if(m.ne.1)go to 116                                                       !/*it4a       50*/
   68 if(itds(l3).eq.0)go to 116                                                !/*it4a       51*/
      itds(l3)=-itds(l3)                                                        !/*it4a       52*/
      if(k.eq.1000002)go to 98                                                  !/*it4a       53*/
      k2=k2+1                                                                   !/*it4a       54*/
      if(k2.gt.mxlv)go to 116                                                   !/*it4a       55*/
      if(k2.eq.1)li=li+nl+1                                                     !/*it4a       56*/
      itds(l2+abs(itds(l2))+1+k2)=li                                            !/*it4a       57*/
      go to 89                                                                  !/*it4a       58*/
!                                                                               !/*it4a       59*/
! >>>>>  process lattice data left bracket, [, k4=# entries in [].              !/*it4a       60*/
   70 go to(116,116,71,116,116,116)il+1                                         !/*it4a       61*/
   71 l4=li                                                                     !/*it4a       62*/
      itds(l4-1)=-itds(l4-1)                                                    !/*it4a       63*/
      k4=0                                                                      !/*it4a       64*/
      mb=0                                                                      !/*it4a       65*/
      go to 117                                                                 !/*it4a       66*/
!                                                                               !/*it4a       67*/
! >>>>>  process lattice data right bracket, ], with il=2 thru 5.               !/*it4a       68*/
   72 if(k4.eq.0)go to 116                                                      !/*it4a       69*/
      n=-mfl(lmfl+1,abs(itds(l4-1)))                                            !/*it4a       70*/
      go to(116,116,73,79,79,79)il+1                                            !/*it4a       71*/
   73 if(k4.gt.1)go to 77                                                       !/*it4a       72*/
!                                                                               !/*it4a       73*/
!        lattice indices in single entry format.                                !/*it4a       74*/
      if(n.le.0)go to 114                                                       !/*it4a       75*/
      j=0                                                                       !/*it4a       76*/
      do 74 i3=laf(n+3,1),laf(n+3,1)+laf(n+3,2)-1                               !/*it4a       77*/
      do 74 i2=laf(n+2,1),laf(n+2,1)+laf(n+2,2)-1                               !/*it4a       78*/
      do 74 i1=laf(n+1,1),laf(n+1,1)+laf(n+1,2)-1                               !/*it4a       79*/
      j=j+1                                                                     !/*it4a       80*/
   74 if(j.eq.itds(l4))go to 75                                                 !/*it4a       81*/
      go to 114                                                                 !/*it4a       82*/
   75 li=l4                                                                     !/*it4a       83*/
      itds(li)=i1                                                               !/*it4a       84*/
      itds(li+1)=i2                                                             !/*it4a       85*/
      itds(li+2)=i3                                                             !/*it4a       86*/
      li=li+3                                                                   !/*it4a       87*/
      go to 84                                                                  !/*it4a       88*/
!                                                                               !/*it4a       89*/
!        lattice indices in triplet format without colons.                      !/*it4a       90*/
   77 if(mod(k4,3).ne.0)go to 116                                               !/*it4a       91*/
      itds(l3)=itds(l3)+k4/3-1                                                  !/*it4a       92*/
      li=li-1                                                                   !/*it4a       93*/
      if(n.le.0)go to 84                                                        !/*it4a       94*/
      do 78 jj=1,k4/3                                                           !/*it4a       95*/
      do 78 j=1,3                                                               !/*it4a       96*/
      in=itds(l4-1+j+4*(jj-1))                                                  !/*it4a       97*/
   78 if(in.lt.laf(n+j,1).or.in.gt.laf(n+j,1)+laf(n+j,2)-1)go to 114            !/*it4a       98*/
      go to 84                                                                  !/*it4a       99*/
!                                                                               !/*it4a      100*/
!        lattice indices in colon format.                                       !/*it4a      101*/
   79 if(k4.ne.il+1)go to 116                                                   !/*it4a      102*/
      do 80 j=1,3                                                               !/*it4a      103*/
   80 if(ik(2*j-1).gt.ik(2*j))go to 114                                         !/*it4a      104*/
      if(n.le.0) go to 82                                                       !/*it4a      105*/
      do 81 j=1,3                                                               !/*it4a      106*/
      do 81 jj=1,2                                                              !/*it4a      107*/
      in=ik(jj+2*(j-1))                                                         !/*it4a      108*/
   81 if(in.lt.laf(n+j,1).or.in.gt.laf(n+j,1)+laf(n+j,2)-1)go to 114            !/*it4a      109*/
   82 li=l4-1                                                                   !/*it4a      110*/
      n=0                                                                       !/*it4a      111*/
      do 83 i3=ik(5),ik(6)                                                      !/*it4a      112*/
      do 83 i2=ik(3),ik(4)                                                      !/*it4a      113*/
      do 83 i1=ik(1),ik(2)                                                      !/*it4a      114*/
      itds(li)=itds(l4-1)                                                       !/*it4a      115*/
      itds(li+1)=i1                                                             !/*it4a      116*/
      itds(li+2)=i2                                                             !/*it4a      117*/
      itds(li+3)=i3                                                             !/*it4a      118*/
      li=li+4                                                                   !/*it4a      119*/
   83 n=n+1                                                                     !/*it4a      120*/
      itds(l3)=itds(l3)+n-1                                                     !/*it4a      121*/
   84 il=0                                                                      !/*it4a      122*/
      go to 117                                                                 !/*it4a      123*/
   85 if(kp.ne.1000009)go to 116                                                !/*it4a      124*/
      go to 117                                                                 !/*it4a      125*/
!                                                                               !/*it4a      126*/
!        process integer data.                                                  !/*it4a      127*/
   86 go to(87,116,112,112,112,112)il+1                                         !/*it4a      128*/
!                                                                               !/*it4a      129*/
! >>>>>  process level data, except lattice index data.                         !/*it4a      130*/
   87 if(m.ne.0)go to 90                                                        !/*it4a      131*/
!                                                                               !/*it4a      132*/
!        set up for the next bin.                                               !/*it4a      133*/
   88 nl=itds(kt+k1+1)                                                          !/*it4a      134*/
      if(jl.eq.1)nl=mxlv                                                        !/*it4a      135*/
      if(jl.eq.2)itds(li)=0                                                     !/*it4a      136*/
      if(jl.eq.2)li=li+1                                                        !/*it4a      137*/
      itds(kt+k1+1)=li                                                          !/*it4a      138*/
      l2=li                                                                     !/*it4a      139*/
      k2=0                                                                      !/*it4a      140*/
   89 l3=li                                                                     !/*it4a      141*/
      li=li+1                                                                   !/*it4a      142*/
      itds(l3)=0                                                                !/*it4a      143*/
      if(k.gt.1000000)go to 117                                                 !/*it4a      144*/
   90 if(kp.lt.1000009)go to 95                                                 !/*it4a      145*/
!                                                                               !/*it4a      146*/
!        generate cell entries that are filled by the given universe.           !/*it4a      147*/
      if(iy.lt.3.and.k2.eq.0.or.junf.eq.0)go to 116                             !/*it4a      148*/
      n=itds(l3)                                                                !/*it4a      149*/
      jj=1                                                                      !/*it4a      150*/
      do 94 j=1,mxa                                                             !/*it4a      151*/
      lp=-mfl(lmfl+1,j)                                                         !/*it4a      152*/
      if(lp.gt.0.and.jj.gt.0)go to 91                                           !/*it4a      153*/
      if(-lp.ne.k)go to 94                                                      !/*it4a      154*/
      itds(l3)=itds(l3)+1                                                       !/*it4a      155*/
      itds(li)=j                                                                !/*it4a      156*/
      li=li+1                                                                   !/*it4a      157*/
      go to 94                                                                  !/*it4a      158*/
   91 jj=2                                                                      !/*it4a      159*/
      do 93 i3=laf(lp+3,1),laf(lp+3,1)+laf(lp+3,2)-1                            !/*it4a      160*/
      do 93 i2=laf(lp+2,1),laf(lp+2,1)+laf(lp+2,2)-1                            !/*it4a      161*/
      do 93 i1=laf(lp+1,1),laf(lp+1,1)+laf(lp+1,2)-1                            !/*it4a      162*/
      jj=jj+1                                                                   !/*it4a      163*/
      if(laf(lp+1,jj).ne.k)go to 93                                             !/*it4a      164*/
      if(k2.gt.0)go to 92                                                       !/*it4a      165*/
      jj=-1                                                                     !/*it4a      166*/
      go to 94                                                                  !/*it4a      167*/
   92 itds(l3)=itds(l3)+1                                                       !/*it4a      168*/
      itds(li)=-j                                                               !/*it4a      169*/
      itds(li+1)=i1                                                             !/*it4a      170*/
      itds(li+2)=i2                                                             !/*it4a      171*/
      itds(li+3)=i3                                                             !/*it4a      172*/
      li=li+4                                                                   !/*it4a      173*/
   93 continue                                                                  !/*it4a      174*/
   94 continue                                                                  !/*it4a      175*/
      if(itds(l3).eq.n)go to 115                                                !/*it4a      176*/
      if(jj.lt.0)call erprnt(1,2,2,k1+1,kl,0,0,0,                               !/*it4a      177*/
     & '39hlattice omitted from u=# of f card bin ,i4,7h tally ,i4,1h.')        !/*it4a      178*/
      if(m.eq.0)itds(l3)=-itds(l3)                                              !/*it4a      179*/
      go to 97                                                                  !/*it4a      180*/
!                                                                               !/*it4a      181*/
!        process surface and cell data, il=2 for lattice cell with [].          !/*it4a      182*/
!                                                                               !/*it4c       23*/
!        macrobody case.                                                        !/*it4c       24*/
!        check for changing to master surface, if identical surface.            !/*it4c       25*/
   95 if(iy.gt.3.or.k2.ne.0)go to 48                                            !/*it4c       26*/
      k=rtp(lrtp+l+i)                                                           !/*it4c       27*/
      kc=namchg(2,k)                                                            !/*it4c       28*/
      if(kc.eq.0)go to 48                                                       !/*it4c       29*/
      if(ksm(lksm+kc).ne.0)go to 44                                             !/*it4c       30*/
      if(idns(lids+kc).eq.0)go to 48                                            !/*it4c       31*/
      if(kc.eq.abs(idnt(lidt+kc)))go to 48                                      !/*it4c       32*/
!                                                                               !/*it4c       33*/
!        change ordinary surface to another ordinary identical surface.         !/*it4c       34*/
      kc=abs(idnt(lidt+kc))                                                     !/*it4c       35*/
      k=nsf(lnsf+kc)                                                            !/*it4c       36*/
      rtp(lrtp+l+i)=k                                                           !/*it4c       37*/
      go to 48                                                                  !/*it4c       38*/
!                                                                               !/*it4c       39*/
!        facet of macrobody.                                                    !/*it4c       40*/
   44 zk=rtp(lrtp+l+i)                                                          !/*it4c       41*/
      yk=zk-k+.001                                                              !/*it4c       42*/
      kz=0                                                                      !/*it4c       43*/
      if(yk.lt..1001.or.yk.gt..999)go to 46                                     !/*it4c       44*/
!                                                                               !/*it4c       45*/
!        increment for facet of macrobody.                                      !/*it4c       46*/
      kz=yk*10                                                                  !/*it4c       47*/
      kc=kc+kz                                                                  !/*it4c       48*/
      jz=6                                                                      !/*it4c       49*/
      ji=-ksm(lksm+kc-kz)                                                       !/*it4c       50*/
      if(ji.eq.33.or.ji.eq.34.or.ji.eq.36)jz=3                                  !/*it4c       51*/
      if(ji.eq.39)jz=8                                                          !/*it4c       52*/
      if(ji.gt.4000)jz=ji/1000                                                  !/*it4c2      13*/
      if(ji.eq.37)jz=5                                                          !/*it4c       53*/
      if(ji.eq.1030.or.ji.eq.1031)jz=4                                          !/*it4c       54*/
      if(kz.gt.jz)go to 46                                                      !/*it4c       55*/
      if(idns(lids+kc).eq.0)go to 48                                            !/*it4c       56*/
      if(kc.eq.abs(idnt(lidt+kc)))go to 48                                      !/*it4c       57*/
      kc=abs(idnt(lidt+kc))                                                     !/*it4c       58*/
      if(ksm(lksm+kc).ne.0)go to 45                                             !/*it4c       59*/
!                                                                               !/*it4c       60*/
!        change from facet to a regular surface.                                !/*it4c       61*/
      k=nsf(lnsf+kc)                                                            !/*it4c       62*/
      rtp(lrtp+l+i)=k                                                           !/*it4c       63*/
      go to 48                                                                  !/*it4c       64*/
!                                                                               !/*it4c       65*/
!        change from a facet to a facet of another macrobody.                   !/*it4c       66*/
   45 jq=ksm(lksm+kc)                                                           !/*it4c       67*/
      k=nsf(lnsf+jq)                                                            !/*it4c       68*/
      kz=kfq                                                                    !/*it4c       69*/
      if(kfq.lt.1.or.kfq.gt.8)go to 46                                          !/*it4c       70*/
      rtp(lrtp+l+i)=k+.1*kfq                                                    !/*it4c       71*/
      go to 48                                                                  !/*it4c       72*/
   46 call erprnt(1,1,3,k,kz,kl,0,0,                                            !/*it4c       73*/
     & '5hfacet,i6,1h.,i1,9h of tally,i5,16h does not exist.')                  !/*it4c       74*/
!                                                                               !/*it4c       75*/
!        regular surface and cell data.  il=2 for lattice cell with [].         !/*it4c       76*/
   48 if(iy.le.3.and.k2.eq.0.and.kc.eq.0)call erprnt(1,1,2,k,kl,0,0,0,          !/*it4c       77*/
     & '7hsurface,i7,9h of tally,i5,11h not found.')                            !/*it4c       78*/
      if(iy.gt.3.or.k2.gt.0)kc=namchg(1,k)                                      !/*it4a      184*/
      if(kc.eq.0)go to 116                                                      !/*it4a      185*/
      if(iy.lt.3.and.k2.eq.0.or.junf.eq.0)go to 96                              !/*it4a      186*/
      if(k2.eq.0.and.kn.eq.1000006)go to 116                                    !/*it4a      187*/
      if(k2.gt.0.and.mfl(lmfl+1,kc)+lat(llat+1,kc).eq.0)go to 116               !/*it4a      188*/
      if(lat(llat+1,kc).ne.0.and.kn.eq.1000006)il=2                             !/*it4a      189*/
   96 itds(l3)=itds(l3)+1                                                       !/*it4a      190*/
      itds(li)=kc                                                               !/*it4a      191*/
      li=li+1                                                                   !/*it4a      192*/
   97 if(m.ne.0.or.kn.eq.1000006)go to 117                                      !/*it4a      193*/
!                                                                               !/*it4a      194*/
!        finish current bin.                                                    !/*it4a      195*/
   98 itds(l2+abs(itds(l2))+1)=k2                                               !/*it4a      196*/
      if(k2.eq.0)li=li+1                                                        !/*it4a      197*/
      k1=k1+1                                                                   !/*it4a      198*/
      k0=k1                                                                     !/*it4a      199*/
!                                                                               !/*it4a      200*/
!        check all levels of current bin for multiple bin format.               !/*it4a      201*/
      nb=1                                                                      !/*it4a      202*/
      do 99 j=1,k2+1                                                            !/*it4a      203*/
      lr=kt+k0                                                                  !/*it4a      204*/
      if(jl.eq.2)lr=itds(lr)                                                    !/*it4a      205*/
      if(j.gt.1)lr=itds(lr+abs(itds(lr))+j)                                     !/*it4a      206*/
   99 if(itds(lr).lt.0)nb=nb*abs(itds(lr))                                      !/*it4a      207*/
      if(nb.eq.1)go to 106                                                      !/*it4a      208*/
!                                                                               !/*it4a      209*/
!        generate multiple bins from current bin.                               !/*it4a      210*/
      do 105 jj=2,nb                                                            !/*it4a      211*/
      if(jl.eq.1)go to 105                                                      !/*it4a      212*/
      itds(li)=itds(itds(kt+k0))                                                !/*it4a      213*/
      if(itds(li).gt.0)itds(li)=1                                               !/*it4a      214*/
      itds(li)=abs(itds(li))                                                    !/*it4a      215*/
      if(jj.le.itds(li))itds(li)=0                                              !/*it4a      216*/
      li=li+1                                                                   !/*it4a      217*/
      itds(kt+k1+1)=li                                                          !/*it4a      218*/
      l2=li                                                                     !/*it4a      219*/
!                                                                               !/*it4a      220*/
!        loop over the levels of the current bin.                               !/*it4a      221*/
      nb=1                                                                      !/*it4a      222*/
      do 104 j=1,k2+1                                                           !/*it4a      223*/
      lr=itds(kt+k0)                                                            !/*it4a      224*/
      if(j.gt.1)lr=itds(lr+abs(itds(lr))+j)                                     !/*it4a      225*/
      if(j.ne.2)go to 100                                                       !/*it4a      226*/
      itds(li)=k2                                                               !/*it4a      227*/
      li=li+k2+1                                                                !/*it4a      228*/
  100 if(j.gt.1)itds(l2+itds(l2)+j)=li                                          !/*it4a      229*/
      itds(li)=1                                                                !/*it4a      230*/
      if(itds(lr).gt.0)itds(li)=itds(lr)                                        !/*it4a      231*/
      li=li+1                                                                   !/*it4a      232*/
      n=lr                                                                      !/*it4a      233*/
      ku=itds(lr)                                                               !/*it4a      234*/
      if(itds(lr).lt.0)ku=mod((jj-1)/nb,abs(itds(lr)))+1                        !/*it4a      235*/
      do 103 k=1,ku                                                             !/*it4a      236*/
      n=n+1                                                                     !/*it4a      237*/
      if(itds(lr).lt.0.and.k.lt.ku)go to 101                                    !/*it4a      238*/
      itds(li)=itds(n)                                                          !/*it4a      239*/
      li=li+1                                                                   !/*it4a      240*/
  101 if(itds(n).gt.0)go to 103                                                 !/*it4a      241*/
      if(itds(lr).lt.0.and.k.lt.ku)go to 103                                    !/*it4a      242*/
      do 102 kk=1,3                                                             !/*it4a      243*/
      itds(li)=itds(n+kk)                                                       !/*it4a      244*/
  102 li=li+1                                                                   !/*it4a      245*/
  103 if(itds(n).lt.0)n=n+3                                                     !/*it4a      246*/
  104 if(itds(lr).lt.0)nb=nb*abs(itds(lr))                                      !/*it4a      247*/
      if(k2.gt.0)go to 105                                                      !/*it4a      248*/
      itds(li)=0                                                                !/*it4a      249*/
      li=li+1                                                                   !/*it4a      250*/
  105 k1=k1+1                                                                   !/*it4a      251*/
!                                                                               !/*it4a      252*/
!        reset flags for multiple bin input - a bit tricky for 1st level        !/*it4a      253*/
  106 if(jl.eq.1)go to 110                                                      !/*it4a      254*/
      do 109 j=1,k2+1                                                           !/*it4a      255*/
      lr=itds(kt+k0)                                                            !/*it4a      256*/
      n=abs(itds(lr))                                                           !/*it4a      257*/
      if(j.gt.1)lr=itds(lr+n+j)                                                 !/*it4a      258*/
      if(itds(lr).gt.0)go to 109                                                !/*it4a      259*/
      if(j.gt.1)go to 108                                                       !/*it4a      260*/
      itds(kt+k0)=lr+n-1                                                        !/*it4a      261*/
      itds(lr+n)=itds(lr+1)                                                     !/*it4a      262*/
      do 107 jj=1,n                                                             !/*it4a      263*/
  107 itds(lr+jj-1)=0                                                           !/*it4a      264*/
      lr=lr+n-1                                                                 !/*it4a      265*/
  108 itds(lr)=1                                                                !/*it4a      266*/
  109 continue                                                                  !/*it4a      267*/
!                                                                               !/*it4a      268*/
!        count the number of bins and levels per bin in first pass.             !/*it4a      269*/
  110 if(jl.eq.2)go to 117                                                      !/*it4a      270*/
      lm=max(lm,li-1)                                                           !/*it4a      271*/
      do 111 j=k0,k1                                                            !/*it4a      272*/
  111 itds(kt+j)=k2                                                             !/*it4a      273*/
      li=kt+k1+1                                                                !/*it4a      274*/
      go to 117                                                                 !/*it4a      275*/
!                                                                               !/*it4a      276*/
! >>>>>  process lattice data, il=2 thru 5.                                     !/*it4a      277*/
  112 itds(li)=k                                                                !/*it4a      278*/
      li=li+1                                                                   !/*it4a      279*/
      k4=k4+1                                                                   !/*it4a      280*/
      if(mod(k4,3).ne.0)go to 113                                               !/*it4a      281*/
      itds(li)=itds(l4-1)                                                       !/*it4a      282*/
      li=li+1                                                                   !/*it4a      283*/
  113 if(mb.gt.5)go to 117                                                      !/*it4a      284*/
      if(kp.eq.1000003.and.kn.eq.1000003)go to 116                              !/*it4a      285*/
      mb=mb+1                                                                   !/*it4a      286*/
      ik(mb)=k                                                                  !/*it4a      287*/
      if(kn.eq.1000003.or.mod(mb,2).eq.0)go to 117                              !/*it4a      288*/
      mb=mb+1                                                                   !/*it4a      289*/
      ik(mb)=k                                                                  !/*it4a      290*/
      go to 117                                                                 !/*it4a      291*/
!                                                                               !/*it4a      292*/
!        common complaints - fatal.                                             !/*it4a      293*/
  114 call erprnt(1,1,3,k2+1,k1+1,kl,0,0,                                       !/*it4a      294*/
     & '18h[i]error in level ,i3,15h of f card bin ,i4,6h tally,i4,1h.')        !/*it4a      295*/
      go to 116                                                                 !/*it4a      296*/
  115 call erprnt(1,1,3,k,k1+1,kl,0,0,                                          !/*it4a      297*/
     & '8huniverse,i5,15h of f card bin ,i4,6h tally,i4,11h not found.')        !/*it4a      298*/
  116 iptal(lipt+1,1,ital)=0                                                    !/*it4a      299*/
      iptal(lipt+1,3,ital)=1                                                    !/*it4a      300*/
      iptal(lipt+1,4,ital)=2                                                    !/*it4a      301*/
      li=kt+1                                                                   !/*it4a      302*/
      go to 260                                                                 !/*it4a      303*/
  117 continue                                                                  !/*it4a      304*/
      kt=kt+k1                                                                  !/*it4a      305*/
!                                                                               !/*it4a      306*/
!        clear any scratch area used during the first pass.                     !/*it4a      307*/
      do 118 i=li,lm                                                            !/*it4a      308*/
  118 itds(i)=0                                                                 !/*it4a      309*/
!                                                                               !/*it4a      310*/
!        if required, make a total bin with first level data.                   !/*it4a      311*/
      if(jb(1,ital).eq.0)go to 260                                              !/*it        109*/
      li=li+1                                                                   !/*it4a      312*/
      itds(kt+1)=li                                                             !/*it        110*/
      li=li+1                                                                   !/*it        111*/
      do 130 i=1,iptal(lipt+1,3,ital)-1                                         !/*it        112*/
      do 120 l=1,itds(itds(iptal(lipt+1,1,ital)+i))                             !/*it        113*/
      nc=itds(itds(iptal(lipt+1,1,ital)+i)+l)                                   !/*it        114*/
      do 119 k=1,itds(itds(kt+1))                                               !/*it4a      313*/
  119 if(itds(itds(kt+1)+k).eq.nc)go to 120                                     !/*it4a      314*/
      itds(li)=nc                                                               !/*it        117*/
      li=li+1                                                                   !/*it        118*/
      itds(itds(kt+1))=itds(itds(kt+1))+1                                       !/*it        119*/
  120 continue                                                                  !/*it        120*/
  130 continue                                                                  !/*it        121*/
      go to 260                                                                 !/*it        122*/
!                                                                               !/*it        123*/
!        set up the detector bins.                                              !/*it        124*/
  140 iptal(lipt+2,1,ital)=jb(2,ital)                                           !/*it        125*/
      iptal(lipt+2,3,ital)=1+iptal(lipt+2,1,ital)                               !/*it        126*/
      i1=4                                                                      !/*it        127*/
      if(jptal(ljpt+8,ital).eq.2)i1=3                                           !/*it        128*/
#ifdef RADIOG                                                                   /*!itx        39*/
      if(jptal(ljpt+8,ital).ge.3)i1=10                                          !/*itx        40*/
#endif                                                                          /*!itx        41*/
      if(mod(n1,i1).ne.0)go to 260                                              !/*it        129*/
      iptal(lipt+1,1,ital)=ld-1                                                 !/*it        130*/
      jsu=0                                                                     !/*it        131*/
      n1=n1/i1                                                                  !/*it        132*/
      iptal(lipt+1,3,ital)=n1                                                   !/*it        133*/
      je=ital                                                                   !/*it        134*/
      if(ipnt(lipn+2,21,ital).eq.0)je=0                                         !/*it        135*/
      do 230 n=1,n1                                                             !/*it        136*/
      if(ndtt+n.gt.mxdt)go to 240                                               !/*it        137*/
      locdt(1,ndtt+n)=ital                                                      !/*it        138*/
      do 150 i=1,i1                                                             !/*it        139*/
  150 tds(ld+i-1)=rtp(lrtp+l+(n-1)*i1+i)                                        !/*it        140*/
      tds(ld+4)=2.*pie*abs(rtp(lrtp+l+n*i1))**3/3.                              !/*it        141*/
      do 160 i=1,3                                                              !/*it        142*/
  160 gpblcm(i)=tds(ld+i-1)                                                     !/*it        143*/
!                                                                               !/*itx        42*/
      if (jptal(ljpt+8,ital).eq.2) then                                         !/*itx        43*/
!                                                                               !/*itx        44*/
!        Ring detector.                                                         !/*itx        45*/
         i=abs(ipnt(lipn+2,1,ital))-1                                           !/*itx        46*/
         tds(ld+2)=i                                                            !/*itx        47*/
         tds(ld+3)=rtp(lrtp+l+3*n)                                              !/*itx        48*/
         if(tds(ld+1).le.0.)call erprnt(1,1,1,kl,0,0,0,0,                       !/*itx        49*/
     &    '49hring detector radius is zero or negative in tally,i3')            !/*itx        50*/
         gpblcm(i)=tds(ld)                                                      !/*itx        51*/
         gpblcm(mod(i,3)+1)=tds(ld+1)                                           !/*itx        52*/
         gpblcm(mod(i+1,3)+1)=0.                                                !/*itx        53*/
#ifdef RADIOG                                                                   /*!itx        54*/
      else if (jptal(ljpt+8,ital).gt.2) then                                    !/*itx        55*/
!                                                                               !/*itx        56*/
!        Image detector.                                                        !/*itx        57*/
         j=l+(n-1)*i1                                                           !/*itx        58*/
         t1=sqrt((rtp(lrtp+j+1)-rtp(lrtp+j+5))**2+(rtp(lrtp+j+2)-               !/*itx        59*/
     &    rtp(lrtp+j+6))**2+(rtp(lrtp+j+3)-rtp(lrtp+j+7))**2)                   !/*itx        60*/
         if(t1.eq.0.)call erprnt(1,1,0,0,0,0,0,0,                               !/*itx        61*/
     &    '38himage grid axis is improperly defined.')                          !/*itx        62*/
         if(t1.eq.0.)go to 215                                                  !/*itx        63*/
         do 162 i=1,3                                                           !/*itx        64*/
            tds(ld+2+i)=rtp(lrtp+j+i)                                           !/*itx        65*/
  162       tds(ld+5+i)=(rtp(lrtp+j+i)-rtp(lrtp+j+i+4))/t1                      !/*itx        66*/
!                                                                               !/*itx        67*/
!        Define tds(ld+19(in tallmg)) as the distance from the origin           !/*itx        68*/
!        to the detector grid plane that is normal to the plane.                !/*itx        69*/
         tds(ld+18)=tds(ld+6)*rtp(lrtp+j+1)+tds(ld+7)*rtp(lrtp+j+2)+            !/*itx        70*/
     &    tds(ld+8)*rtp(lrtp+j+3)                                               !/*itx        71*/
         tds(ld+15)=abs(rtp(lrtp+j+i1-1))                                       !/*itx        72*/
         if(tds(ld+15).eq.0..and.jptal(ljpt+8,ital).gt.3)tds(ld+15)=huge        !/*itx        73*/
         tds(ld+16)=rtp(lrtp+j+8)                                               !/*itx        74*/
         j2=3                                                                   !/*itx        75*/
         if(abs(tds(ld+8)).gt.0.99)j2=2                                         !/*itx        76*/
         i=ld+16                                                                !/*itx        77*/
!                                                                               !/*itx        78*/
!        Determine the direction cosines for the t-axis and the                 !/*itx        79*/
!        s-axis of the detector grid.                                           !/*itx        80*/
!        The t-axis will be defined by the intersection of the grid             !/*itx        81*/
!        plane and a plane formed by the z-axis and the center point            !/*itx        82*/
!        of the grid.  In this case, positive t is in the same                  !/*itx        83*/
!        direction as positive z.                                               !/*itx        84*/
!        If however, the grid is perpendicular to the z-axis, the               !/*itx        85*/
!        t-axis will be defined as being parallel to the positive               !/*itx        86*/
!        y-axis.  The s-axis is always defined such that it completes           !/*itx        87*/
!        a right-handed orthonormal coordinate set with the t-axis              !/*itx        88*/
!        and the outward surface normal to the grid plane.                      !/*itx        89*/
!                                                                               !/*itx        90*/
         if (j2.ne.3) then                                                      !/*itx        91*/
            tds(ld+9)=0.                                                        !/*itx        92*/
            tds(ld+10)=1.                                                       !/*itx        93*/
            tds(ld+11)=0.                                                       !/*itx        94*/
         else                                                                   !/*itx        95*/
            rp=sqrt(tds(ld)**2+tds(ld+1)**2+tds(ld+2)**2)                       !/*itx        96*/
            tds(ld+9)=-(tds(ld)/rp)*tds(ld+8)                                   !/*itx        97*/
            tds(ld+10)=(-tds(ld+1)/rp)*tds(ld+8)                                !/*itx        98*/
            tds(ld+11)=tds(ld+6)*(tds(ld)/rp)-(-tds(ld+1)/rp)*tds(ld+7)         !/*itx        99*/
!           Renormalize the t-axis direction cosines before                     !/*itx       100*/
!           the s-axis direction cosines are determined.                        !/*itx       101*/
            t2=sqrt(tds(ld+9)**2+tds(ld+10)**2+tds(ld+11)**2)                   !/*itx       102*/
            tds(ld+9)=tds(ld+9)/t2                                              !/*itx       103*/
            tds(ld+10)=tds(ld+10)/t2                                            !/*itx       104*/
            tds(ld+11)=tds(ld+11)/t2                                            !/*itx       105*/
         endif                                                                  !/*itx       106*/
!                                                                               !/*itx       107*/
         tds(ld+12)=tds(ld+10)*tds(ld+8)-tds(ld+7)*tds(ld+11)                   !/*itx       108*/
         tds(ld+13)=tds(ld+6)*tds(ld+11)-tds(ld+9)*tds(ld+8)                    !/*itx       109*/
         tds(ld+14)=tds(ld+9)*tds(ld+7)-tds(ld+6)*tds(ld+10)                    !/*itx       110*/
         tds(i+1)=rtp(lrtp+j+i1)                                                !/*itx       111*/
!                                                                               !/*itx       112*/
         if (jptal(ljpt+8,ital).eq.3) then                                      !/*itx       113*/
!           Set coordinates of center of the image grid.                        !/*itx       114*/
            tds(ld+3)=tds(ld+3)+rtp(lrtp+j+i1)*tds(ld+6)                        !/*itx       115*/
            tds(ld+4)=tds(ld+4)+rtp(lrtp+j+i1)*tds(ld+7)                        !/*itx       116*/
            tds(ld+5)=tds(ld+5)+rtp(lrtp+j+i1)*tds(ld+8)                        !/*itx       117*/
         endif                                                                  !/*itx       118*/
#endif                                                                          /*!itx       119*/
      endif                                                                     !/*itx       120*/
!                                                                               !/*itx       121*/
      do 190 i=1,mxa                                                            !/*itx       122*/
      if(junf.eq.0)go to 180                                                    !/*it        154*/
      if(jun(ljun+i).ne.0)go to 190                                             !/*it        155*/
  180 call chkcel(i,0,j)                                                        !/*it        156*/
      if(j.eq.0)go to 200                                                       !/*it        157*/
  190 continue                                                                  !/*it        158*/
      call erprnt(1,1,2,n,kl,0,0,0,                                             !/*it        159*/
     & '12hdetector no.,i2,9h of tally,i4,20h is not in any cell.')             !/*it        160*/
      go to 220                                                                 !/*it        161*/
  200 do 210 j=1,mipt                                                           !/*it        162*/
      if(ktp(lktp+j,ital).ne.0.and.fim(lfim+j,i).eq.0.)                         !/*it        163*/
     & call erprnt(1,1,3,n,kl,ncl(lncl+i),0,0,                                  !/*it        164*/
     & '8hdetector,i2,9h of tally,i4,27h is in zero-importance cell,i5')        !/*it        165*/
  210 if(rho(lrho+i).eq.0..and.tds(ld+3).lt.0..and.junf.eq.0.and.               !/*it        166*/
#ifdef RADIOG                                                                   /*!itx       123*/
     & jptal(ljpt+8,ital).le.2.and.                                             !/*itx       124*/
#endif                                                                          /*!itx       125*/
     & fim(lfim+j,i).ne.0.)call erprnt(1,1,2,n,kl,0,0,0,                        !/*it        167*/
     & '8hdetector,i2,9h of tally,i4,27h has negative r0 in a void.')           !/*it        168*/
#ifdef RADIOG                                                                   /*!itx       126*/
  215 if (jptal(ljpt+8,ital).gt.2) then                                         !/*itx       127*/
         ld=ld+14                                                               !/*itx       128*/
         go to 220                                                              !/*itx       129*/
      endif                                                                     !/*itx       130*/
#endif                                                                          /*!itx       131*/
      if(ioid.ne.0.and.tds(ld+3).lt.0.)tds(ld+3)=0.                             !/*it4a      315*/
  220 ld=ld+5                                                                   !/*it        169*/
!                                                                               !/*it        170*/
!        process dd data:  detector roulette and diagnostics.                   !/*it        171*/
      if(ipnt(lipn+2,21,je).ge.2*n-1)                                           !/*it        172*/
     & ddg(1,ndtt+n)=rtp(lrtp+ipnt(lipn+1,21,je)+2*n-2)                         !/*it        173*/
  230 if(ipnt(lipn+2,21,je).ge.2*n)                                             !/*it        174*/
     & ddg(2,ndtt+n)=rtp(lrtp+ipnt(lipn+1,21,je)+2*n-1)                         !/*it        175*/
  240 ndtt=ndtt+n1                                                              !/*it        176*/
      if(ndtt.gt.mxdt.and.ndtt-n1.le.mxdt)call erprnt(1,1,1,mxdt,0,0,0,         !/*it        177*/
     & 0,'9hmore than,i3,11h detectors.')                                       !/*it        178*/
!                                                                               !/*it        179*/
!        process pd data:  detector cell contribution                           !/*it        180*/
      je=ital                                                                   !/*it        181*/
      if(ipnt(lipn+2,15,ital).eq.0)je=0                                         !/*it        182*/
      if(ip.ne.0)iptal(lipt+2,2,ital)=iptal(lipt+2,2,ip)                        !/*it        183*/
      if(ipnt(lipn+2,15,je).eq.0.or.ip.ne.0.and.je.eq.0)go to 260               !/*it        184*/
      if(je.eq.0)ip=ital                                                        !/*it        185*/
      iptal(lipt+2,2,ital)=ld-1                                                 !/*it        186*/
      do 250 i=1,mxa                                                            !/*it        187*/
  250 tds(ld+i-1)=rtp(lrtp+ipnt(lipn+1,15,je)+i-1)                              !/*it        188*/
      ld=ld+mxa                                                                 !/*it        189*/
!                                                                               !/*it        190*/
!        process fu data:  user-supplied tally bins                             !/*it        191*/
  260 if(ipnt(lipn+1,16,ital).ne.0)iptal(lipt+3,1,ital)=-1                      !/*it        192*/
      if (ipnt(lipn+2,16,ital).ne.0) then                                       !/*itx       132*/
         n=ipnt(lipn+2,16,ital)                                                 !/*itx       133*/
         iptal(lipt+3,3,ital)=n+jb(3,ital)                                      !/*itx       134*/
         iptal(lipt+3,4,ital)=n+1                                               !/*itx       135*/
         do 270 i=1,n                                                           !/*itx       136*/
  270       tds(ld+i-1)=rtp(lrtp+ipnt(lipn+1,16,ital)+i-1)                      !/*itx       137*/
         iptal(lipt+3,1,ital)=ld-1                                              !/*itx       138*/
         ld=ld+n                                                                !/*itx       139*/
      endif                                                                     !/*itx       140*/
!                                                                               !/*itx       141*/
      if (iy.eq.5) then                                                         !/*itx       142*/
#ifdef RADIOG                                                                   /*!itx       143*/
!        add bins for image detector tally if requested:  fs and c              !/*itx       144*/
         m1=6                                                                   !/*itx       145*/
         do 285 m=3,6,3                                                         !/*itx       146*/
            if(ipnt(lipn+2,m,ital).eq.0)go to 285                               !/*itx       147*/
            n=abs(ipnt(lipn+2,m,ital))                                          !/*itx       148*/
            do 282 i=1,n                                                        !/*itx       149*/
  282          tds(ld+i-1)=rtp(lrtp+ipnt(lipn+1,m,ital)+i-1)                    !/*itx       150*/
            iptal(lipt+m1,3,ital)=n+jb(m1,ital)-1                               !/*itx       151*/
            iptal(lipt+m1,4,ital)=n                                             !/*itx       152*/
            iptal(lipt+m1,1,ital)=ld                                            !/*itx       153*/
            iptal(lipt+m1,2,ital)=0                                             !/*itx       154*/
            ld=ld+n                                                             !/*itx       155*/
  285       m1=4                                                                !/*itx       156*/
#endif                                                                          /*!itx       157*/
         go to 380                                                              !/*itx       158*/
      endif                                                                     !/*itx       159*/
!                                                                               !/*it        201*/
!        process fs data:  segment bins                                         !/*it        202*/
      je=ital                                                                   !/*it        204*/
      if(ipnt(lipn+2,6,ital).eq.0)je=0                                          !/*it        205*/
      if(ks.eq.0)go to 290                                                      !/*it        206*/
      iptal(lipt+4,1,ital)=iptal(lipt+4,1,ks)                                   !/*it        207*/
      iptal(lipt+4,3,ital)=iptal(lipt+4,3,ks)                                   !/*it        208*/
      iptal(lipt+4,4,ital)=iptal(lipt+4,4,ks)                                   !/*it        209*/
  290 if(ipnt(lipn+2,6,je).eq.0.or.ks.ne.0.and.je.eq.0)go to 330                !/*it        210*/
      if(je.eq.0)ks=ital                                                        !/*it        211*/
      mxafs=mxafs+1                                                             !/*it        212*/
      iptal(lipt+4,1,ital)=mxafs                                                !/*it        213*/
      n=ipnt(lipn+2,6,je)                                                       !/*it        214*/
      iptal(lipt+4,3,ital)=n+1+jb(4,je)                                         !/*it        215*/
      iptal(lipt+4,4,ital)=n+2                                                  !/*it        216*/
      lca(llca+mxafs)=-lca(llca+mxafs)                                          !/*it        217*/
      lca(llca+mxafs+1)=abs(lca(llca+mxafs))+n                                  !/*it        218*/
      do 310 i=1,n                                                              !/*it        219*/
      r=rtp(lrtp+ipnt(lipn+1,6,je)-1+i)                                         !/*it4c       79*/
      j=r                                                                       !/*it4c       80*/
      if(abs(r-anint(r)).lt..001)j=nint(r)                                      !/*it4c       81*/
      k=namchg(2,j)                                                             !/*it4c       82*/
      if(k.eq.0)go to 304                                                       !/*it4c       83*/
      if(ksm(lksm+k).ge.0)go to 304                                             !/*it4c       84*/
      rr=abs(rtp(lrtp+ipnt(lipn+1,6,je)-1+i))                                   !/*it4c       85*/
      rr=rr-abs(j)+0.001                                                        !/*it4c       86*/
      jq=rr*10                                                                  !/*it4c       87*/
      if(jq.lt.1.or.jq.gt.8)call erprnt(1,1,2,jq,kl,0,0,0,                      !/*it4c       88*/
     & '15hsegment facet  ,i7,9h of tally,i5,11h not found.')                   !/*it4c       89*/
      if(jq.lt.1.or.jq.gt.8)k=0                                                 !/*it4c       90*/
      if(k.ne.0)k=k+jq                                                          !/*it4c       91*/
  304 if(k.eq.0)call erprnt(1,1,2,j,kl,0,0,0,                                   !/*it4c       92*/
     & '15hsegment surface,i7,9h of tally,i5,11h not found.')                   !/*it4c       93*/
      lja(llja+abs(lca(llca+mxafs))-1+i)=sign(k,j)                              !/*it4c       94*/
      if(iy.gt.3)go to 310                                                      !/*it        223*/
      do 300 m=1,iptal(lipt+1,3,ital)                                           !/*it        224*/
      do 300 j=1,itds(itds(iptal(lipt+1,1,ital)+m))                             !/*it        225*/
  300 if(itds(itds(iptal(lipt+1,1,ital)+m)+j).eq.k)go to 320                    !/*it        226*/
  310 continue                                                                  !/*it        227*/
      go to 330                                                                 !/*it        228*/
  320 call erprnt(1,1,1,kl,0,0,0,0,                                             !/*it        229*/
     & '47hsegment surface same as tally surface for tally,i4')                 !/*it        230*/
!                                                                               !/*it        231*/
!        process sd data:  segment volumes, masses, or areas                    !/*it        232*/
  330 if((iy.eq.1.or.iy.eq.8).and.ipnt(lipn+1,11,ital).eq.0)go to 380           !/*it        233*/
      i1=iptal(lipt+1,3,ital)                                                   !/*it        234*/
      j1=iptal(lipt+4,3,ital)                                                   !/*it        235*/
      iptal(lipt+4,2,ital)=ld-1                                                 !/*it        236*/
      if(ipnt(lipn+2,11,ital).eq.0.or.iptal(lipt+1,1,ital).eq.0)                !/*it4a      316*/
     & go to 370                                                                !/*it4a      317*/
      l=ipnt(lipn+1,11,ital)-1                                                  !/*it        238*/
      jl=1                                                                      !/*it4a      318*/
  335 jl=jl-1                                                                   !/*it4a      319*/
      k=1                                                                       !/*it        239*/
      do 350 i=1,i1                                                             !/*it        240*/
      if(jl.lt.0)itds(itds(iptal(lipt+1,1,ital)+i)-1)=-1                        !/*it4a      320*/
      nl=itds(itds(iptal(lipt+1,1,ital)+i)-1)                                   !/*it4a      321*/
      if(nl.gt.0)go to 345                                                      !/*it4a      322*/
      m=0                                                                       !/*it        241*/
      if(rtp(lrtp+l+k).eq.1000001)m=1                                           !/*it        242*/
      if(rtp(lrtp+l+k).eq.1000001)k=k+1                                         !/*it        243*/
      do 340 j=1,j1                                                             !/*it        244*/
      if(rtp(lrtp+l+k).eq.1000001.or.rtp(lrtp+l+k).eq.1000002)go to 360         !/*it        245*/
      tds(ld-1+j+(i-1)*j1)=rtp(lrtp+l+k)                                        !/*it        246*/
  340 k=k+1                                                                     !/*it        247*/
      if(m.eq.1.and.rtp(lrtp+l+k).ne.1000002)go to 360                          !/*it        248*/
      if(m.eq.1)k=k+1                                                           !/*it4a      323*/
      go to 350                                                                 !/*it4a      324*/
  345 do 346 j=1,j1                                                             !/*it4a      325*/
  346 tds(ld-1+j+(i-1)*j1)=tds(ld-1+j+(i-1-nl)*j1)                              !/*it4a      326*/
  350 continue                                                                  !/*it4a      327*/
      if(k.eq.ipnt(lipn+2,11,ital)+1)go to 370                                  !/*it        250*/
  360 if(jl.eq.0)go to 335                                                      !/*it4a      328*/
      call erprnt(1,1,1,kl,0,0,0,0,                                             !/*it4a      329*/
     & '7hcard sd,i3,45h has wrong number of entries or wrong format.')         !/*it        252*/
  370 ld=ld+i1*j1                                                               !/*it        253*/
!                                                                               !/*it        254*/
!        process fm data:  energy-dependent multiplier bins                     !/*it        255*/
  380 if(ipnt(lipn+2,10,ital).eq.0)go to 590                                    !/*it        256*/
      l=ipnt(lipn+1,10,ital)-1                                                  !/*it        257*/
      n=ipnt(lipn+2,10,ital)                                                    !/*it        258*/
      iptal(lipt+5,2,ital)=ld-1                                                 !/*it        259*/
      if(ktp(lktp+3,ital).ne.0.and.n.gt.1)call erprnt(1,1,0,0,0,0,0,0,          !/*it        260*/
     & '52hfm for electron tally has more than just a constant.')               !/*it        261*/
!                                                                               !/*it        262*/
!        count bins, check parentheses and store constant multipliers.          !/*it        263*/
      m=0                                                                       !/*it        264*/
      m1=0                                                                      !/*it        265*/
      m2=0                                                                      !/*it        266*/
      k2=0                                                                      !/*it        267*/
      ni=0                                                                      !/*it        268*/
      k1=0                                                                      !/*it        269*/
      if(rtp(lrtp+l+1).ne.1000001)m1=1                                          !/*it        270*/
      do 420 k=1,n                                                              !/*it        271*/
      if(rtp(lrtp+l+k).ne.1000001.and.rtp(lrtp+k+l).ne.1000002.and.             !/*it        272*/
     & k.ne.k1+1.and.ktp(lktp+1,ital)*ktp(lktp+2,ital).ne.0.and.                !/*itx       160*/
     & jptal(ljpt+3,ital).eq.2)go to 570                                        !/*itx       161*/
      if(rtp(lrtp+l+k).eq.1000002)k2=-abs(k2)                                   !/*it        274*/
      if(rtp(lrtp+l+k).eq.1000002)m=m-1                                         !/*it        275*/
      if(rtp(lrtp+l+k).eq.1000001)m=m+1                                         !/*it        276*/
      if(rtp(lrtp+l+k).eq.1000001.and.m+m1.eq.1)k1=k                            !/*it        277*/
      if(m2.eq.0.or.m+m1.ne.-1.or.rtp(lrtp+l+k).ne.1000002)go to 390            !/*it        278*/
      if(iptal(lipt+5,3,ital).eq.m2)go to 580                                   !/*it        279*/
      m=m+1                                                                     !/*it        280*/
      m2=0                                                                      !/*it        281*/
      k2=0                                                                      !/*it        282*/
  390 if(k.ne.k1+1.or.k.eq.1.or.rtp(lrtp+l+k).ne.1000001)go to 400              !/*it        283*/
      m=m-1                                                                     !/*it        284*/
      k1=k                                                                      !/*it        285*/
      m2=iptal(lipt+5,3,ital)                                                   !/*it        286*/
  400 if(m2.eq.0.or.k.ne.k1+2.or.rtp(lrtp+l+k).gt.0.)go to 410                  !/*it        287*/
      if(k2.ne.0)go to 580                                                      !/*it        288*/
      k2=1                                                                      !/*it        289*/
      iptal(lipt+5,3,ital)=iptal(lipt+5,3,ital)-1                               !/*it        290*/
  410 if(k.ne.k1+1.and.(rtp(lrtp+l+k).ne.1000001.or.k.le.k1+3))go to 420        !/*it        291*/
      tds(ld)=rtp(lrtp+l+k1+1)                                                  !/*it        292*/
      if(tds(ld).lt.0..and.jptal(ljpt+2,ital).ne.4)call erprnt(1,1,1,           !/*it        293*/
     & kl,0,0,0,0,'39hnegative multiplier on fm card of tally,i4')              !/*it        294*/
      if(tds(ld).gt.0..and.iy.eq.4.and.npert.ne.0)                              !/*it4c       95*/
     & call erprnt(1,2,1,kl,0,0,0,0,                                            !/*it4c       96*/
     & '53hperturbation may require negative fm constant.  tally,i4')           !/*it4c       97*/
      ld=ld+1                                                                   !/*it        295*/
      ni=ni+1                                                                   !/*it        296*/
      iptal(lipt+5,3,ital)=iptal(lipt+5,3,ital)+1                               !/*it        297*/
  420 if(m+m1+max(0,k2).gt.2.or.m.lt.0.or.k.eq.n.and.                           !/*it        298*/
     & (m.ne.0.or.m2.ne.0))go to 580                                            !/*it        299*/
      iptal(lipt+5,4,ital)=iptal(lipt+5,3,ital)                                 !/*it        300*/
      iptal(lipt+5,3,ital)=iptal(lipt+5,3,ital)+jb(5,ital)-1                    !/*it        301*/
      if(n.eq.1)go to 590                                                       !/*it        302*/
!                                                                               !/*it        303*/
!        set up the multiplier bins description.                                !/*it        304*/
      tds(ld)=ni                                                                !/*it        305*/
      iptal(lipt+5,1,ital)=ld                                                   !/*it        306*/
      ld=ld+ni+1                                                                !/*it        307*/
      k1=0                                                                      !/*it        308*/
      nb=0                                                                      !/*it        309*/
      do 560 k=1,n                                                              !/*it        310*/
      if(rtp(lrtp+l+k).ne.1000002)go to 440                                     !/*it        311*/
      if(m2.eq.0.or.m+m1.ne.0)go to 430                                         !/*it        312*/
      m2=0                                                                      !/*it        313*/
      go to 560                                                                 !/*it        314*/
  430 m=m-1                                                                     !/*it        315*/
      if(k.eq.n)go to 560                                                       !/*it        316*/
      if(rtp(lrtp+l+k+1).ne.1000001.and.rtp(lrtp+l+k+1).ne.1000002.or.          !/*it        317*/
     & k.eq.k1+1)go to 580                                                      !/*it        318*/
      go to 560                                                                 !/*it        319*/
  440 if(rtp(lrtp+l+k).ne.1000001)go to 460                                     !/*it        320*/
      if(k.ne.k1+1.or.k.eq.1)go to 450                                          !/*it        321*/
      m2=1                                                                      !/*it        322*/
      k1=k                                                                      !/*it        323*/
      go to 560                                                                 !/*it        324*/
  450 m=m+1                                                                     !/*it        325*/
      if(m+m1.eq.1)k1=k                                                         !/*it        326*/
      if(k.eq.k1+1.or.k.eq.k1+2)go to 580                                       !/*it        327*/
      if(k.le.k1+3)go to 560                                                    !/*it        328*/
      go to 480                                                                 !/*it        329*/
  460 if(m+m1.eq.0)go to 580                                                    !/*it        330*/
      if(k.ne.k1+1)go to 500                                                    !/*it        331*/
      if(rtp(lrtp+l+k+1).eq.1000002)go to 490                                   !/*it        332*/
      tds(iptal(lipt+5,1,ital)+nb+1)=min(-1,nint(rtp(lrtp+l+k+1)))              !/*it4b        3*/
      if(n.eq.k+1)go to 490                                                     !/*it        334*/
      if(rtp(lrtp+l+k+2).eq.1000002)go to 490                                   !/*it        335*/
      i=-1                                                                      !/*it        336*/
      if(rtp(lrtp+l+k+1).lt.0)go to 480                                         !/*it        337*/
      do 470 i=1,nmat                                                           !/*it        338*/
  470 if(nmt(lnmt+i).eq.rtp(lrtp+l+k+1))go to 480                               !/*it        339*/
  480 tds(ld)=i                                                                 !/*it        340*/
      kq=1                                                                      !/*it        341*/
      l1=ld+1                                                                   !/*it        342*/
      if(m2.ne.0.and.i.gt.0)tds(l1)=10000000*m2                                 !/*it        343*/
      tds(iptal(lipt+5,1,ital)+nb+1)=sign(l1,i*m2)                              !/*it        344*/
      if(m2.ne.0.and.i.lt.0)m2=2                                                !/*it        345*/
      ld=ld+2                                                                   !/*it        346*/
  490 nb=nb+1                                                                   !/*it        347*/
      go to 560                                                                 !/*it        348*/
  500 if(k.lt.k1+3)go to 560                                                    !/*it        349*/
      if(i.gt.0)go to 540                                                       !/*it        350*/
      if(i.eq.-2)go to 530                                                      !/*it        351*/
      do 510 j=1,nmat                                                           !/*it        352*/
  510 if(nmt(lnmt+j).eq.rtp(lrtp+l+k))go to 520                                 !/*it        353*/
  520 tds(ld)=j                                                                 !/*it        354*/
      tds(l1)=tds(l1)+2.                                                        !/*it        355*/
      i=-2                                                                      !/*it        356*/
      go to 550                                                                 !/*it        357*/
  530 tds(ld)=rtp(lrtp+l+k)                                                     !/*it        358*/
      i=-3                                                                      !/*it        359*/
      go to 550                                                                 !/*it        360*/
  540 tds(ld)=rtp(lrtp+l+k)                                                     !/*it        361*/
      j=tds(ld)                                                                 !/*it        362*/
      if(ktp(lktp+1,ital).ne.0.and.j.le.-6)itfxs=1                              !/*it        363*/
      if(ktp(lktp+1,ital).ne.0.and.j.lt.-8.or.                                  !/*it4b        4*/
     & ktp(lktp+2,ital).ne.0.and.j.lt.-6)call erprnt(1,1,2,j,kl,0,0,0,          !/*it        365*/
     & '32hillegal special reaction number,,i4,7h, on fm,i3,6h card.')          !/*it        366*/
      if(ktp(lktp+2,ital).ne.0.and.j.gt.0.and.j.lt.10000.and.ispn.eq.0)         !/*itxk        1*/
     & call erprnt(1,1,2,j,kl,0,0,0,                                            !/*itx       163*/
     & '31hillegal photon reaction number,,i4,7h, on fm,i3,6h card.')           !/*itx       164*/
      if(mcal.eq.0.and.ktp(lktp+1,ital).ne.0.and.(j.eq.1.or.j.eq.2))            !/*it        367*/
     & kq=-1                                                                    !/*it        368*/
      tds(l1)=kq*(abs(tds(l1))+1.)                                              !/*it        369*/
  550 ld=ld+1                                                                   !/*it        370*/
  560 continue                                                                  !/*it        371*/
      go to 590                                                                 !/*it        372*/
  570 call erprnt(1,1,1,kl,0,0,0,0,                                             !/*it        373*/
     & '25hfm card of combined tally,i4,20h is too complicated.')               !/*it        374*/
      go to 590                                                                 !/*it        375*/
  580 call erprnt(1,1,1,kl,0,0,0,0,                                             !/*it        376*/
     & '16hfm card of tally,i4,18h has wrong format.')                          !/*it        377*/
!                                                                               !/*it        378*/
!        process ft data:  special treatments                                   !/*it        379*/
  590 if(ipnt(lipn+2,22,ital).eq.0)go to 700                                    !/*it        380*/
      mb=2                                                                      !/*it        381*/
      kq=0                                                                      !/*it        382*/
      l=lrtp+ipnt(lipn+1,22,ital)-1                                             !/*it        383*/
      do 650 i=1,ipnt(lipn+2,22,ital)                                           !/*it        384*/
      tds(ld)=rtp(l+i)                                                          !/*it        385*/
      if(i.ne.mb)go to 650                                                      !/*it        386*/
      k=rtp(l+i-1)                                                              !/*it        387*/
      if(rtp(l+i).ne.nz(k))go to 660                                            !/*it        388*/
      if(k.eq.3.and.rtp(l+i+1).ge.rtp(l+i+2))go to 670                          !/*it        389*/
      if(k.eq.3.and.ipnt(lipn+2,5,ital).le.1.and.ipnt(lipn+2,5,0).le.1)         !/*it        390*/
     & call erprnt(1,1,1,kl,0,0,0,0,                                            !/*it        391*/
     & '41htime convolution without time bins, tally,i4')                       !/*it        392*/
      if(k.eq.8.and.(rtp(l+i+1).lt.1..or.rtp(l+i+1).gt.3.))go to 670            !/*it        393*/
      if((k.eq.4.or.k.eq.5.or.k.eq.7).and.iptal(lipt+3,1,ital).le.0)            !/*it        394*/
     & go to 680                                                                !/*it        395*/
      if(k.eq.5.and.jptal(ljpt+2,ital).ne.5)go to 690                           !/*it        396*/
      if(k.eq.6.or.k.eq.7)ifft=1                                                !/*it        397*/
      mb=mb+rtp(l+i)+2                                                          !/*it        398*/
      lft(llft+k,ital)=ld                                                       !/*it        399*/
      if(k.ne.1)go to 600                                                       !/*it        400*/
      t=1./sqrt(rtp(l+i+1)**2+rtp(l+i+2)**2+rtp(l+i+3)**2)                      !/*it        401*/
      rtp(l+i+1)=rtp(l+i+1)*t                                                   !/*it        402*/
      rtp(l+i+2)=rtp(l+i+2)*t                                                   !/*it        403*/
      rtp(l+i+3)=rtp(l+i+3)*t                                                   !/*it        404*/
      go to 650                                                                 !/*it        405*/
  600 if(k.ne.6)go to 620                                                       !/*it        406*/
      do 610 j=1,msd                                                            !/*it        407*/
      if(ksd(lksd+1,j).ne.rtp(l+i+1))go to 610                                  !/*it        408*/
      rtp(l+i+1)=j                                                              !/*it        409*/
      iptal(lipt+3,3,ital)=ksd(lksd+4,j)+ksd(lksd+5,j)                          !/*it        410*/
      iptal(lipt+3,4,ital)=iptal(lipt+3,3,ital)                                 !/*it        411*/
      if(iptal(lipt+3,3,ital).gt.1)go to 650                                    !/*it        412*/
  610 continue                                                                  !/*it        413*/
      go to 670                                                                 !/*it        414*/
  620 if(k.ne.7)go to 640                                                       !/*it        415*/
      do 630 j1=1,iptal(lipt+3,4,ital)-1                                        !/*it        416*/
      iw=tds(iptal(lipt+3,1,ital)+j1)                                           !/*it        417*/
      do 630 j=1,msd                                                            !/*it        418*/
  630 if(ksd(lksd+1,j).eq.iw)tds(ld+j1)=j                                       !/*it        419*/
      ld=ld+iptal(lipt+3,4,ital)                                                !/*it        420*/
      go to 650                                                                 !/*it        421*/
  640 if(k.ne.8)go to 650                                                       !/*it        422*/
      if(rtp(l+i+1).eq.1.)go to 650                                             !/*it        423*/
      iptal(lipt+3,3,ital)=3                                                    !/*it        424*/
      iptal(lipt+3,4,ital)=3                                                    !/*it        425*/
      kq=1                                                                      !/*it        426*/
  650 ld=ld+1                                                                   !/*it        427*/
      if(lft(llft+4,ital)+lft(llft+5,ital)+lft(llft+6,ital)+kq+                 !/*it        428*/
     & lft(llft+7,ital).gt.max(lft(llft+4,ital),lft(llft+5,ital),kq,            !/*it        429*/
     & lft(llft+6,ital),lft(llft+7,ital)))call erprnt(1,1,1,kl,0,0,0,0,         !/*it        430*/
     & '2hft,i3,37h has incompatible special treatments.')                      !/*it        431*/
      go to 700                                                                 !/*it        432*/
  660 call erprnt(1,1,1,kl,0,0,0,0,                                             !/*it        433*/
     & '33hwrong number of parameters for ft,i3,4h '//hft(k))                   !/*it        434*/
      go to 700                                                                 !/*it        435*/
  670 call erprnt(1,1,1,kl,0,0,0,0,                                             !/*it        436*/
     & '27hincorrect parameters for ft,i3,4h '//hft(k))                         !/*it        437*/
      go to 700                                                                 !/*it        438*/
  680 call erprnt(1,1,2,kl,kl,0,0,0,                                            !/*it        439*/
     & '2hft,i3,16h '//hft(k)//' requires fu,i3,6h data.')                      !/*it        440*/
      go to 700                                                                 !/*it        441*/
  690 call erprnt(1,1,1,kl,0,0,0,0,                                             !/*it        442*/
     & '2hft,i3,37h '//hft(k)//' can be used only with detectors.')             !/*it        443*/
!                                                                               !/*it        444*/
!        process c, e, and t data:  cosine, energy, and time bins               !/*it        445*/
  700 continue                                                                  !/*itx       165*/
#ifdef MESHTAL                                                                  /*!itx       166*/
      if(ital.gt.ntal)go to 825                                                 !/*itx       167*/
#endif                                                                          /*!itx       168*/
      tc=-huge                                                                  !/*itx       169*/
      do 710 i=1,mipt                                                           !/*it        447*/
  710 if(ktp(lktp+i,ital).ne.0)tc=max(tc,tco(i))                                !/*it        448*/
      do 820 iw=1,3                                                             !/*it        449*/
      n6=0                                                                      !/*it        450*/
      tn=huge                                                                   !/*it        451*/
      if(iw.eq.2.and.ktp(lktp+1,ntal).ne.0.and.img.eq.0)tn=emx(1)               !/*it        452*/
      if(iw.eq.3.and.iy.ne.5)tn=tc                                              !/*it        453*/
#ifdef RADIOG                                                                   /*!itx       170*/
      if(iy.eq.5.and.iw.eq.1)go to 820                                          !/*itx       171*/
#endif                                                                          /*!itx       172*/
      je=ital                                                                   !/*it        454*/
      if(ipnt(lipn+2,iw+2,ital).eq.0)je=0                                       !/*it        455*/
      if(iy.ne.1.and.iw.eq.1.and.je.eq.0)go to 800                              !/*it        456*/
      if(iy.eq.8.and.iw.eq.3.and.je.eq.0)go to 800                              !/*cor4-1     73*/
      if(ipnt(lipn+2,iw+2,je).eq.0)go to 800                                    !/*it        458*/
      if(je.ne.0)go to 720                                                      !/*it        459*/
      if(kd(1,iw).eq.0)go to 720                                                !/*cor4-1     74*/
      if((jptal(ljpt+2,kd(1,iw)).eq.8.and.iy.ne.8).or.                          !/*cor4-1     75*/
     & (jptal(ljpt+2,kd(1,iw)).ne.8.and.iy.eq.8))                               !/*cor4-1     76*/
     & call erprnt(1,1,2,jptal(ljpt+1,kd(1,iw)),kl,0,0,0,                       !/*cor4-1     77*/
     & '5htally,i4,10h and tally,i4,22h cannot share e0 card.')                 !/*cor4-1     78*/
      iptal(lipt+iw+5,1,ital)=iptal(lipt+iw+5,1,kd(1,iw))                       !/*cor4-1     79*/
      iptal(lipt+iw+5,3,ital)=iptal(lipt+iw+5,3,kd(1,iw))                       !/*cor4-1     80*/
      iptal(lipt+iw+5,4,ital)=iptal(lipt+iw+5,4,kd(1,iw))                       !/*cor4-1     81*/
  720 n=ipnt(lipn+2,iw+2,je)                                                    !/*it        464*/
      n6=n                                                                      !/*it        465*/
      if(kd(1,iw).ne.0.and.je.eq.0)go to 800                                    !/*cor4-1     82*/
      if(je.eq.0)kd(1,iw)=ital                                                  !/*cor4-1     83*/
      do 730 i=1,n                                                              !/*it        468*/
      a=rtp(lrtp+ipnt(lipn+1,iw+2,je)+i-1)                                      !/*it        469*/
      if(a.ge.tn)go to 740                                                      !/*it        470*/
  730 tds(ld+i-1)=a                                                             !/*it        471*/
      go to 750                                                                 !/*it        472*/
  740 tds(ld+i-1)=tn                                                            !/*it        473*/
      if(n.gt.i.or.a.gt.tn)call erprnt(1,2,1,kl,0,0,0,0,                        !/*it        474*/
     & '36h'//hc(iw)//' bin limits adjusted for tally,i4')                      !/*it        475*/
      n=i                                                                       !/*it        476*/
  750 if(iw.ne.2.or.np.eq.3)go to 790                                           !/*it        477*/
      b=-huge                                                                   !/*it        478*/
      do 760 i=1,mipt                                                           !/*it        479*/
  760 if(ktp(lktp+i,ital).ne.0)b=max(b,ecf(i))                                  !/*it        480*/
      do 770 i=1,n                                                              !/*it        481*/
      if(iy.eq.8.and.tds(ld+i-1).eq.0.)go to 790                                !/*cor4-1     84*/
  770 if(tds(ld+i-1).gt.b)go to 780                                             !/*it        482*/
  780 if(iy.ne.8.and.i.gt.1.and.mcal.ne.2)call erprnt(1,2,2,i-1,kl,0,0,         !/*cor4-1     85*/
     & 0,'i4,21h energy bins of tally,i4,25h are below energy cutoff.')         !/*cor4-1     86*/
      if(iy.eq.8)call erprnt(1,2,1,kl,0,0,0,0,                                  !/*cor4-1     87*/
     & '5htally,i4,46h needs zero energy bin for negative f8 scores.')          !/*cor4-1     88*/
  790 i=kl                                                                      !/*it        485*/
      if(je.eq.0)i=0                                                            !/*it        486*/
      if(iw.eq.3.and.tds(ld+n-1).lt..99999*tc)call erprnt(1,2,1,kl,0,0,         !/*it        487*/
     & 0,0,'22hlast time bin of tally,i4,26h is less than time cutoff.')        !/*it        488*/
      iptal(lipt+iw+5,1,ital)=ld-1                                              !/*it        489*/
      if(n.ne.1)iptal(lipt+iw+5,3,ital)=n+jb(iw+5,je)                           !/*it        490*/
      iptal(lipt+iw+5,4,ital)=n+1                                               !/*it        491*/
      ld=ld+n                                                                   !/*it        492*/
!                                                                               !/*it        493*/
!        process cm, em, and tm data:  tally bin multipliers                    !/*it        494*/
  800 if(ipnt(lipn+2,iw+11,ital).ne.0.and.iptal(lipt+iw+5,1,ital).eq.0)         !/*it        495*/
     & call erprnt(1,2,1,kl,0,0,0,0,'5htally,i4,4x,38h'//                       !/*it        496*/
     & hf(iw)//'m card ineffective because no '//hf(iw)//' card.')              !/*it        497*/
      if(iptal(lipt+iw+5,1,ital).eq.0)go to 820                                 !/*it        498*/
      je=ital                                                                   !/*it        499*/
      if(ipnt(lipn+2,iw+11,ital).eq.0)je=0                                      !/*it        500*/
      if(kd(2,iw).ne.0)iptal(lipt+iw+5,2,ital)=iptal(lipt+iw+5,2,               !/*cor4-1     89*/
     & kd(2,iw))                                                                !/*cor4-1     90*/
      if(ipnt(lipn+2,iw+11,je).eq.0)go to 820                                   !/*it4c       98*/
      if(ipnt(lipn+2,iw+11,je).ne.n6)call erprnt(1,1,1,kl,0,0,0,0,              !/*it4c       99*/
     & '50hno. of '//hc(iw)//' bins and multipliers differ in tally,i4')        !/*it4c      100*/
      if(kd(2,iw).ne.0.and.je.eq.0)go to 820                                    !/*it4c      101*/
      if(je.eq.0)kd(2,iw)=ital                                                  !/*cor4-1     93*/
      iptal(lipt+iw+5,2,ital)=ld-1                                              !/*it        506*/
      n=iptal(lipt+iw+5,4,ital)-1                                               !/*it        507*/
      do 810 i=1,n                                                              !/*it        510*/
  810 tds(ld+i-1)=rtp(lrtp+ipnt(lipn+1,iw+11,je)+i-1)                           !/*it        511*/
      ld=ld+n                                                                   !/*it        512*/
  820 continue                                                                  !/*it        513*/
#ifdef RADIOG                                                                   /*!itx       173*/
!                                                                               !/*itx       174*/
!       Get rid of total bins for image grid detector.                          !/*itx       175*/
      if (jptal(ljpt+8,ital).ge.4) then                                         !/*itx       176*/
         iptal(lipt+4,3,ital)=iptal(lipt+4,4,ital)-1                            !/*itx       177*/
         iptal(lipt+6,3,ital)=iptal(lipt+6,4,ital)-1                            !/*itx       178*/
      endif                                                                     !/*itx       179*/
#endif                                                                          /*!itx       180*/
!                                                                               !/*it        514*/
!        process de and df cards:  dose function                                !/*it        515*/
#ifdef MESHTAL                                                                  /*!itx       181*/
  825 continue                                                                  !/*itx       182*/
#endif                                                                          /*!itx       183*/
      i=ital                                                                    !/*it        516*/
      j=ital                                                                    !/*it        517*/
      if(ipnt(lipn+1,18,ital).eq.0)i=0                                          !/*itxc        1*/
      if(ipnt(lipn+1,19,ital).eq.0)j=0                                          !/*itxc        2*/
      if(i.eq.0.and.j.eq.0.and.jd.ne.0)then                                     !/*itxc        3*/
         iptal(lipt+3,2,ital)=iptal(lipt+3,2,jd)                                !/*itxc        4*/
         go to 880                                                              !/*itxc        5*/
      endif                                                                     !/*itxc        6*/
!        df card.                                                               !/*itxc        7*/
      la=ipnt(lipn+1,19,j)                                                      !/*itxc        8*/
      if(la.eq.0)go to 880                                                      !/*itxc        9*/
      kh=0                                                                      !/*itxc       10*/
      tds(ld)=zero                                                              !/*itxc       11*/
      tds(ld+1)=zero                                                            !/*itxc       12*/
      tds(ld+2)=ten                                                             !/*itxc       13*/
      tds(ld+3)=two                                                             !/*itxc       14*/
      tds(ld+4)=one                                                             !/*itxc       15*/
      kz=0                                                                      !/*itxc       16*/
      na=ipnt(lipn+2,19,j)                                                      !/*itxc       17*/
      if(na.eq.0)tds(ld+1)=-one                                                 !/*itxc       18*/
      do 830 k=1,na                                                             !/*itxc       19*/
      a=rtp(lrtp+la+k-1)                                                        !/*itxc       20*/
      if(kz.gt.4)then                                                           !/*itxc       21*/
         if(tds(ld+1).gt.zero)go to 870                                         !/*itxc       22*/
         if(kz.eq.5)tds(ld+2)=a                                                 !/*itxc       23*/
         if(kz.eq.7.and.nint(a).eq.1)tds(ld+3)=one                              !/*itxc       24*/
         if(kz.gt.8)tds(ld+4)=a                                                 !/*itxc       25*/
         tds(ld+1)=-one                                                         !/*itxc       26*/
         kz=0                                                                   !/*itxc       27*/
         go to 830                                                              !/*itxc       28*/
      endif                                                                     !/*itxc       29*/
      if(a.ge.zero)then                                                         !/*itxc       30*/
         if(tds(ld+1).lt.zero)go to 870                                         !/*itxc       31*/
         tds(ld+kh+2)=a                                                         !/*itxc       32*/
         kh=kh+1                                                                !/*itxc       33*/
      else                                                                      !/*itxc       34*/
         kz=-nint(a)                                                            !/*itxc       35*/
         if(kz.gt.4)go to 830                                                   !/*itxc       36*/
         if(kz.eq.1)tds(ld)=two                                                 !/*itxc       37*/
         if(kz.eq.3)tds(ld)=one                                                 !/*itxc       38*/
         kz=0                                                                   !/*itxc       39*/
      endif                                                                     !/*itxc       40*/
  830 continue                                                                  !/*itxc       41*/
      if(tds(ld+1).lt.zero)then                                                 !/*itxc       42*/
         kz=nint(tds(ld+2))                                                     !/*itxc       43*/
         if(tds(ld).eq.zero.and.kz.ne.10.and.kz.ne.40)tds(ld)=three             !/*itxc       44*/
         if(tds(ld).eq.zero)tds(ld)=one                                         !/*itxc       45*/
         if(kz.ne.10.and.kz.ne.20.and.abs(kz-33).gt.2.and.kz.ne.40)             !/*itxc       46*/
     &   call erprnt(1,1,2,kz,kl,0,0,0,                                         !/*itxc       47*/
     &   '27hillegal dose function, ic =,i2,3h de,i3,5h card')                  !/*itxc       48*/
         iptal(lipt+3,2,ital)=ld+1                                              !/*itxc       49*/
         ld=ld+5                                                                !/*itxc       50*/
         if(i.eq.0.and.j.eq.0)jd=ital                                           !/*itxc       51*/
         go to 880                                                              !/*itxc       52*/
      endif                                                                     !/*itxc       53*/
      tds(ld+1)=kh                                                              !/*itxc       54*/
      if(tds(ld).ne.two)tds(ld)=zero                                            !/*itxc       55*/
!        de card.                                                               !/*itxc       56*/
      kj=0                                                                      !/*itxc       57*/
      la=ipnt(lipn+1,18,i)                                                      !/*itxc       58*/
      do 850 k=1,ipnt(lipn+2,18,i)                                              !/*itxc       59*/
      a=rtp(lrtp+la+k-1)                                                        !/*itxc       60*/
      if(a.ge.zero)then                                                         !/*itxc       61*/
         tds(ld+kj+kh+2)=tds(ld+kj+2)                                           !/*itxc       62*/
         tds(ld+kj+2)=a                                                         !/*itxc       63*/
         if(kj.gt.0.and.tds(ld+kj+2).le.tds(ld+kj+1))                           !/*itxc       64*/
     &   call erprnt(1,1,1,kl,0,0,0,0,                                          !/*itxc       65*/
     &   '41henergies not monotonically increasing, de,i3')                     !/*itxc       66*/
         kj=kj+1                                                                !/*itxc       67*/
      else                                                                      !/*itxc       68*/
         if(tds(ld).ne.zero.and.tds(ld).ne.two)go to 870                        !/*itxc       69*/
         if(nint(a).eq.-1)tds(ld)=tds(ld)+one                                   !/*itxc       70*/
      endif                                                                     !/*itxc       71*/
  850 continue                                                                  !/*itxc       72*/
      if(kj.ne.kh)call erprnt(1,1,1,kl,0,0,0,0,                                 !/*itxc       73*/
     & '38hnumber of values on de and df of tally,i4,11h not equal.')           !/*itxc       74*/
      if(kj.eq.0.or.kj.ne.kh)go to 880                                          !/*itxc       75*/
      iptal(lipt+3,2,ital)=ld+1                                                 !/*itxc       76*/
      ld=ld+2*kh+2                                                              !/*itxc       77*/
      if(i.eq.0.and.j.eq.0)jd=ital                                              !/*itxc       78*/
      go to 880                                                                 !/*itxc       79*/
!        de,df error escape.                                                    !/*itxc       80*/
  870 call erprnt(1,1,1,kl,0,0,0,0,                                             !/*itxc       81*/
     & '22hdose function of tally,i4,8h is bad.')                               !/*itxc       82*/
!                                                                               !/*it        554*/
!        process tf card:  tally fluctuations chart bin indexes.                !/*it        555*/
  880 continue                                                                  !/*itx       184*/
#ifdef MESHTAL                                                                  /*!itx       185*/
      if(ital.gt.ntal) go to 930                                                !/*itx       186*/
#endif                                                                          /*!itx       187*/
      do 890 i=1,8                                                              !/*itx       188*/
      j=iptal(lipt+i,3,ital)                                                    !/*it        557*/
      if(i.eq.1.or.i.eq.2.or.i.eq.5)j=1                                         !/*it        558*/
      if(jtf(ljtf+i,ital).eq.0)jtf(ljtf+i,ital)=j                               !/*it        559*/
  890 if(jtf(ljtf+i,ital).lt.0.or.jtf(ljtf+i,ital).gt.                          !/*it        560*/
     & iptal(lipt+i,3,ital))go to 900                                           !/*it        561*/
      go to 910                                                                 !/*it        562*/
  900 call erprnt(1,1,1,kl,0,0,0,0,                                             !/*it        563*/
     & '27hentries on tf card of tally,i4,15h are incorrect.')                  !/*it        564*/
!                                                                               !/*it        565*/
!        set up tally index coefficients and fluctuations chart index.          !/*it        566*/
  910 iptal(lipt+8,5,ital)=iptal(lipt+8,3,ital)                                 !/*it        567*/
      jptal(ljpt+7,ital)=jptal(ljpt+5,ital)+jtf(ljtf+8,ital)                    !/*it        568*/
      do 920 iz=1,7                                                             !/*it        569*/
      i=8-iz                                                                    !/*it        570*/
      iptal(lipt+i,5,ital)=iptal(lipt+i+1,5,ital)*iptal(lipt+i,3,ital)          !/*it        571*/
  920 jptal(ljpt+7,ital)=jptal(ljpt+7,ital)+(jtf(ljtf+i,ital)-1)*               !/*it        572*/
     & iptal(lipt+i+1,5,ital)                                                   !/*it        573*/
      mxf=mxf+iptal(lipt+1,5,ital)                                              !/*it        574*/
!                                                                               !/*it        576*/
!        print information about the tally.                                     !/*it        577*/
      if(ink(30).ne.0)call italpr                                               !/*it        578*/
  930 continue                                                                  !/*it        579*/
!                                                                               !/*it        580*/
!        set dd card defaults if not done already.                              !/*it        581*/
      d1=.1                                                                     !/*it        582*/
      if(ipnt(lipn+2,21,0).ge.1)d1=rtp(lrtp+ipnt(lipn+1,21,0))                  !/*it        583*/
      d2=1000.                                                                  !/*it        584*/
      if(ipnt(lipn+2,21,0).ge.2)d2=rtp(lrtp+ipnt(lipn+1,21,0)+1)                !/*it        585*/
      do 940 ip=1,mipt                                                          !/*it        586*/
      do 940 i=1,mxdx                                                           !/*it        587*/
      if(ddx(ip,1,i).eq.huge)ddx(ip,1,i)=d1                                     !/*it        588*/
  940 if(ddx(ip,2,i).eq.huge)ddx(ip,2,i)=d2                                     !/*it        589*/
      do 950 i=1,ndtt                                                           !/*it        590*/
      if(ddg(1,i).eq.huge)ddg(1,i)=d1                                           !/*it        591*/
  950 if(ddg(2,i).eq.huge)ddg(2,i)=d2                                           !/*it        592*/
!                                                                               !/*it        593*/
!        set up the tally locators.                                             !/*it        594*/
      if(ntal.ne.0)call talloc(li)                                              !/*it        595*/
      if(li.gt.litd+lit)call expire(0,'itally',                                 !/*it        596*/
     & 'li>litd+lit   the trouble is in the code, not in your problem.')        !/*it        597*/
      if(ld.gt.ltds+ltd)call expire(0,'itally',                                 !/*it        598*/
     & 'ld>ltds+ltd   the trouble is in the code, not in your problem.')        !/*it        599*/
      return                                                                    !/*it        600*/
      end                                                                       !/*it        601*/
