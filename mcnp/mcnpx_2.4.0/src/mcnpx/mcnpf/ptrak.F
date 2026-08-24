!#ident ptrak.F from deck vp4a                                                  !/*vp4a        1*/
      subroutine ptrak(m)                                                       !/*vp4a        2*/
!        initialize, filter, and write the particle track events.               !/*vp4a        3*/
      use global_data                                                           !/*vpx         1*/
!                                                                               !/*vpx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*vpx         3*/
      implicit integer(kindi) (i-n)                                             !/*vpx         4*/
!                                                                               !/*vpx         5*/
      save hs,ht,ih,il,in,ip,iq,iv,iz,nn                                        !/*vp4a        5*/
      real vp(10),rp(20),rq(6)                                                  !/*vp4c2       1*/
      dimension id(2,5),ih(24),il(6),in(5),ip(nptr+1),iq(3,5),iv(5),            !/*vp4c2       2*/
     & iw(6),iz(25)                                                             !/*vp4c2       3*/
      character hs*17,ht(2)*6,hq*74,hf*14                                       !/*vp4c2       4*/
      data id/9,9,10,11,12,13,10,11,14,15/,                                     !/*vp4a       10*/
     & ih/1,2,3,4,5,6,7,8,9,10,17,18,19,23,24,25,                               !/*vp4a       11*/
     & -2,-3,-7,-8,-10,-11,-12,-13/,il/8,7,6,11,3,5/,in/5*0/,                   !/*vp4a       12*/
     & ip/nptr*0,0/,iq/1,14*0/,iv/0,0,2,9,10/,iw/1,2,3,4,5,6/,                  !/*vp4c2       5*/
     & iz/1,0,0,1,0,0,1,1,1,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0/,                   !/*vp4a       14*/
     & ht/'ascii','binary'/                                                     !/*vp4a       15*/
!                                                                               !/*vp4a       16*/
      go to(10,190,190,190,190,190,440,480,710)m+1                              !/*vp4a       17*/
!                                                                               !/*vp4a       18*/
! >>>>>  m=0 -- check the ptrac input and open the files.                       !/*vp4a       19*/
   10 n=0                                                                       !/*vp4a       20*/
      lp=abs(iptra(11))                                                         !/*vp4a       21*/
      if(lp.ne.0)n=nint(ptr(lp))                                                !/*vp4a       22*/
      do 20 i=1,n                                                               !/*vp4a       23*/
      if(kpt(nint(ptr(lp+i))).ne.0)go to 20                                     !/*vp4a       24*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*vp4a       25*/
     & '51hptrac type entry is inconsistent with problem mode.')                !/*vp4a       26*/
      go to 30                                                                  !/*vp4a       27*/
   20 continue                                                                  !/*vp4a       28*/
   30 n=0                                                                       !/*vp4a       29*/
      lp=abs(iptra(10))                                                         !/*vp4a       30*/
      lv=abs(iptra(12))                                                         !/*vp4a       31*/
      if(lp.ne.0)n=nint(ptr(lp))                                                !/*vp4a       32*/
!                                                                               !/*vp4a       33*/
!        find and convert all entries of the tally parameter.                   !/*vp4a       34*/
      do 60 i=1,n                                                               !/*vp4a       35*/
      it=nint(ptr(lp+i))                                                        !/*vp4a       36*/
      do 40 j=1,ntal                                                            !/*vp4a       37*/
   40 if(abs(it).eq.jptal(ljpt+1,j))go to 50                                    !/*vp4a       38*/
      call erprnt(1,1,1,abs(it),0,0,0,0,                                        !/*vp4a       39*/
     & '12hptrac tally ,i5,22h is not a valid tally.')                          !/*vp4a       40*/
      go to 60                                                                  !/*vp4a       41*/
   50 if(it.lt.0.or.lv.eq.0)j=-j                                                !/*vp4a       42*/
      ptr(lp+i)=j                                                               !/*vp4a       43*/
      if(lv.eq.0)go to 60                                                       !/*vp4a       44*/
      if(j.lt.0.and.ptr(lv+i).lt.0.)call erprnt(1,1,1,abs(it),0,0,0,0,          !/*vp4a       45*/
     & '12hptrac tally ,i5,27h has a negative multiplier.')                     !/*vp4a       46*/
   60 continue                                                                  !/*vp4a       47*/
!                                                                               !/*vp4a       48*/
!        open the scratch and output files, and write the header.               !/*vp4a       49*/
      open(iupc,status='scratch',form='unformatted')                            !/*vp4a       50*/
      call unique(ptrac,jtty)                                                   !/*vp4a       51*/
      if(iptra(4).eq.2)go to 70                                                 !/*vp4a       52*/
      open(iupw,file=ptrac,status='new')                                        !/*vp4a       53*/
      write(iupw,'(i5)')-1                                                      !/*vp4a       54*/
      write(iupw,'(a8,a5,a28,a19)')kod,ver,loddat,idtm                          !/*vpx         6*/
      write(iupw,'(a80)')aid                                                    !/*vp4a       56*/
      go to 80                                                                  !/*vp4a       57*/
   70 open(iupw,file=ptrac,form='unformatted',status='new')                     !/*vp4a       58*/
      write(iupw)-1                                                             !/*vp4a       59*/
      write(iupw)kod,ver,loddat,idtm                                            !/*vp4a       60*/
      write(iupw)aid                                                            !/*vp4a       61*/
!                                                                               !/*vp4a       62*/
!        include the ptrac input.                                               !/*vp4a       63*/
   80 vp(1)=nptr                                                                !/*vp4a       64*/
      j=1                                                                       !/*vp4a       65*/
      do 120 i=1,nptr                                                           !/*vp4a       66*/
      l=iptra(i)                                                                !/*vp4a       67*/
      lp=abs(l)                                                                 !/*vp4a       68*/
      n=0                                                                       !/*vp4a       69*/
      if(l.gt.0)n=1                                                             !/*vp4a       70*/
      if(l.lt.0)n=nint(ptr(lp))                                                 !/*vp4a       71*/
      j=j+1                                                                     !/*vp4a       72*/
      vp(j)=n                                                                   !/*vp4a       73*/
      if(j.lt.10)go to 100                                                      !/*vp4a       74*/
      if(iptra(4).eq.1)write(iupw,90)vp                                         !/*vp4a       75*/
   90 format(1x,1p10e12.4)                                                      !/*vp4a       76*/
      if(iptra(4).eq.2)write(iupw)vp                                            !/*vp4a       77*/
      j=0                                                                       !/*vp4a       78*/
  100 do 110 k=1,n                                                              !/*vp4a       79*/
      j=j+1                                                                     !/*vp4a       80*/
      vp(j)=l                                                                   !/*vp4a       81*/
      if(l.lt.0)vp(j)=ptr(lp+k)                                                 !/*vp4a       82*/
      if(j.lt.10)go to 110                                                      !/*vp4a       83*/
      if(iptra(4).eq.1)write(iupw,90)vp                                         !/*vp4a       84*/
      if(iptra(4).eq.2)write(iupw)vp                                            !/*vp4a       85*/
      j=0                                                                       !/*vp4a       86*/
  110 continue                                                                  !/*vp4a       87*/
  120 continue                                                                  !/*vp4a       88*/
      if(j.eq.0)go to 140                                                       !/*vp4a       89*/
      do 130 i=j+1,10                                                           !/*vp4a       90*/
  130 vp(i)=0.                                                                  !/*vp4a       91*/
      if(iptra(4).eq.1)write(iupw,90)vp                                         !/*vp4a       92*/
      if(iptra(4).eq.2)write(iupw)vp                                            !/*vp4a       93*/
!                                                                               !/*vp4a       94*/
!        set the lengths of the nps and event lines.                            !/*vp4a       95*/
  140 nn=2                                                                      !/*vp4a       96*/
      do 150 i=3,5                                                              !/*vp4a       97*/
      if(iptra(iv(i)).eq.0)go to 150                                            !/*vp4a       98*/
      nn=nn+1                                                                   !/*vp4a       99*/
      in(nn)=iv(i)                                                              !/*vp4a      100*/
      iw(nn)=i                                                                  !/*vp4a      101*/
  150 continue                                                                  !/*vp4a      102*/
      write(hs,'(4h(1x,,i1,12hi10,1pe13.5))')nn                                 !/*vp4a      103*/
      iw(nn+1)=6                                                                !/*vp4a      104*/
      l=nn                                                                      !/*vp4a      105*/
      if(iptra(10).ne.0)l=nn+1                                                  !/*vp4a      106*/
      n=0                                                                       !/*vp4a      107*/
      do 160 i=1,nipt                                                           !/*vpx         7*/
      if(kpt(i).ne.0)k=i                                                        !/*vp4a      109*/
  160 n=n+kpt(i)                                                                !/*vp4a      110*/
      if(n.eq.1)ip(11)=1                                                        !/*vp4a      111*/
      if(n.ne.1)k=0                                                             !/*vp4a      112*/
      i1=6+iptra(13)                                                            !/*vp4a      113*/
      i2=3**iptra(13)                                                           !/*vp4a      114*/
      ip(13)=i1+i2                                                              !/*vp4a      115*/
      do 170 i=1,5                                                              !/*vp4a      116*/
      iq(1,i)=i1-ip(11)-iq(1,i)                                                 !/*vp4a      117*/
      iq(2,i)=i2-iq(2,i)                                                        !/*vp4a      118*/
  170 iq(3,i)=iq(1,i)+iq(2,i)                                                   !/*vp4a      119*/
      if(iptra(4).eq.2)go to 180                                                !/*vp4a      120*/
      write(iupw,'(1x,20i5)')l,(iq(1,j),iq(2,j),j=1,5),k,4,(0,j=1,7)            !/*vp4a      121*/
      write(iupw,'(1x,30i4)')(iw(j),j=1,l),(7,8,(j,j=id(1,n),id(2,n)),          !/*vp4a      122*/
     & (j,j=16+ip(11),17+iptra(13)),(j,j=20,19+i2),n=1,5)                       !/*vp4a      123*/
      return                                                                    !/*vp4a      124*/
  180 write(iupw)l,(iq(1,j),iq(2,j),j=1,5),k,4,(0,j=1,7)                        !/*vp4a      125*/
      write(iupw)(iw(j),j=1,l),(7,8,(j,j=id(1,n),id(2,n)),                      !/*vp4b        1*/
     & (j,j=16+ip(11),17+iptra(13)),(j,j=20,19+i2),n=1,5)                       !/*vp4a      127*/
      return                                                                    !/*vp4a      128*/
!                                                                               !/*vp4a      129*/
! >>>>>  m=1 through 5 -- filter and write the ptrac events.                    !/*vp4a      130*/
!        m=1 source; m=2 bank; m=3 surface; m=4 collision; m=5 term.            !/*vp4a      131*/
  190 im=1                                                                      !/*vp4a      132*/
      if(m.eq.5)ip(5)=ip(5)+1                                                   !/*vp4a      133*/
!                                                                               !/*vp4a      134*/
!        apply the various event filters - bank events are special.             !/*vp4a      135*/
      do 290 i=1,6                                                              !/*vp4a      136*/
      lp=abs(iptra(il(i)))                                                      !/*vp4a      137*/
      if(lp.eq.0)go to 290                                                      !/*vp4a      138*/
!                                                                               !/*vp4a      139*/
!        arranged for speed, i hope - il() contains order.                      !/*vp4a      140*/
      go to(270,260,260,210,200,240)i                                           !/*vp4a      141*/
  200 v=m                                                                       !/*vp4a      142*/
      go to 220                                                                 !/*vp4a      143*/
  210 v=ipt                                                                     !/*vp4a      144*/
  220 do 230 j=1,nint(ptr(lp))                                                  !/*vp4a      145*/
  230 if(v.eq.ptr(lp+j))go to 290                                               !/*vp4a      146*/
      return                                                                    !/*vp4a      147*/
  240 do 250 j=1,nint(ptr(lp)),3                                                !/*vp4a      148*/
      k=ih(nint(ptr(lp+j+2)))                                                   !/*vp4a      149*/
      if(k.gt.0)go to 245                                                       !/*vp4a      150*/
      v=jpblcm(abs(k))                                                          !/*vp4a      151*/
      if(k.eq.-2)v=ncl(lncl+nint(v))                                            !/*vp4a      152*/
      if(k.eq.-3)v=nsf(lnsf+nint(v))                                            !/*vp4a      153*/
      if(k.eq.-3)v=v+kfq*0.1                                                    !/*vp4c2       6*/
      go to 250                                                                 !/*vp4a      154*/
  245 v=gpblcm(k)                                                               !/*vp4a      155*/
  250 if(v.lt.ptr(lp+j).or.v.gt.ptr(lp+j+1))go to 280                           !/*vp4a      156*/
      go to 290                                                                 !/*vp4a      157*/
  260 if(ip(il(i)).lt.lp)go to 290                                              !/*vp4a      158*/
      go to 280                                                                 !/*vp4a      159*/
  270 v=npstc                                                                   !/*vp4a      160*/
      if(v.lt.ptr(lp+1).or.v.gt.ptr(lp+nint(ptr(lp))))go to 280                 !/*vp4a      161*/
      go to 290                                                                 !/*vp4a      162*/
  280 if(m.ne.2.or.il(i).ne.5)return                                            !/*vp4a      163*/
      im=-1                                                                     !/*vp4a      164*/
  290 continue                                                                  !/*vp4a      165*/
!                                                                               !/*vp4a      166*/
!        eliminate useless bank events, increment the various counters.         !/*vp4a      167*/
      if(iptra(3).eq.0.and.m.eq.2.and.ip(3).eq.2)go to 300                      !/*vp4a      168*/
      ip(6)=ip(6)+1                                                             !/*vp4a      169*/
      ip(7)=ip(7)+1                                                             !/*vp4a      170*/
      ip(1)=ip(1)+1                                                             !/*vp4a      171*/
!                                                                               !/*vp4a      172*/
!        write the appropriate event data to the buffer.                        !/*vp4a      173*/
  300 lp=lpts+ip(13)*ip(1)                                                      !/*vp4a      174*/
      pts(lp+1)=9000.                                                           !/*vp4a      175*/
      pts(lp+2)=node                                                            !/*vp4a      176*/
      l=0                                                                       !/*vp4a      177*/
      go to (310,320,330,340,350)m                                              !/*vp4a      178*/
  310 pts(lp+3)=nsr                                                             !/*vp4a      179*/
      go to 370                                                                 !/*vp4a      180*/
  320 pts(lp+3)=0.                                                              !/*vp4a      181*/
      pts(lp+4)=0.                                                              !/*vp4a      182*/
      i=abs(iex)                                                                !/*vp4a      183*/
      l=nint(tpp(1))                                                            !/*vp4c2       7*/
      if(iz(l).eq.0)go to 360                                                   !/*vp4c2       8*/
      pts(lp+3)=nxs(lnxs+2,iex)                                                 !/*vp4c2       9*/
      if(nxs(lnxs+2,iex).lt.1000)pts(lp+3)=1000.*nxs(lnxs+2,iex)                !/*vp4c2      10*/
      pts(lp+4)=ntyn                                                            !/*vp4c2      11*/
      go to 360                                                                 !/*vp4a      191*/
  330 pts(lp+3)=nsf(lnsf+jsu)+kfq*0.1                                           !/*vp4c2      12*/
      pts(lp+4)=acos(angl())*180./pie                                           !/*vp4a      193*/
      go to 360                                                                 !/*vp4a      194*/
  340 pts(lp+3)=nxs(lnxs+2,iex)                                                 !/*vp4a      195*/
      if(nxs(lnxs+2,iex).lt.1000)pts(lp+3)=1000.*nxs(lnxs+2,iex)                !/*vpx         8*/
      pts(lp+4)=mtp                                                             !/*vp4c2      13*/
      go to 360                                                                 !/*vp4a      198*/
  350 pts(lp+3)=nter                                                            !/*vp4a      199*/
      pts(lp+4)=ip(5)                                                           !/*vp4a      200*/
  360 lp=lp+1                                                                   !/*vp4a      201*/
  370 pts(lp+4)=ipt                                                             !/*vp4a      202*/
      if(ip(11).eq.0)lp=lp+1                                                    !/*vp4a      203*/
      pts(lp+4)=ncl(lncl+icl)                                                   !/*vp4a      204*/
      pts(lp+5)=mat(lmat+icl)                                                   !/*vp4a      205*/
      pts(lp+6)=ncp                                                             !/*vp4a      206*/
      if(iptra(13).eq.2)lp=lp+1                                                 !/*vp4a      207*/
      do 380 i=1,3**iptra(13)                                                   !/*vp4a      208*/
  380 pts(lp+5+i)=gpblcm(i)                                                     !/*vp4a      209*/
      if(lev.eq.0)go to 400                                                     !/*vp4a      210*/
      do 390 i=1,3*iptra(13)                                                    !/*vp4a      211*/
  390 pts(lp+5+i)=udt(i,0)                                                      !/*vp4a      212*/
  400 pts(lpts+ip(13)*(ip(1)-1)+1)=float(im*(m*1000+l))                         !/*vp4a      213*/
      ip(3)=m                                                                   !/*vp4a      214*/
      if(ip(1).le.iptra(1))return                                               !/*vp4a      215*/
!                                                                               !/*vp4a      216*/
!        when full, move the buffer to the scratch file.                        !/*vp4a      217*/
      if(ip(4).eq.0)ip(4)=nint(pts(lpts+1))                                     !/*vp4a      218*/
      rp(1)=pts(lpts+1)                                                         !/*vp4a      219*/
      do 420 i=1,iptra(1)                                                       !/*vp4a      220*/
      lp=lpts+ip(13)*i                                                          !/*vp4a      221*/
      n=iq(3,nint(abs(rp(1)))/1000)                                             !/*vp4a      222*/
      do 410 j=1,n                                                              !/*vp4a      223*/
  410 rp(j)=pts(lp+j)                                                           !/*vp4a      224*/
  420 write (iupc) rp(1:n)                                                      !/*vpx         9*/
!                                                                               !/*vp4a      226*/
!        move the last event to the front of the buffer.                        !/*vp4a      227*/
      pts(lpts+1)=rp(1)                                                         !/*vp4a      228*/
      do 430 j=1,ip(13)                                                         !/*vp4a      229*/
  430 pts(lpts+ip(13)+j)=pts(lp+ip(13)+j)                                       !/*vp4a      230*/
      ip(1)=1                                                                   !/*vp4a      231*/
      return                                                                    !/*vp4a      232*/
!                                                                               !/*vp4a      233*/
! >>>>>  m=6 -- set the cell and surface flags.                                 !/*vp4a      234*/
  440 k=ncl(lncl+icl)                                                           !/*vp4a      235*/
      do 470 i=3,4                                                              !/*vp4a      236*/
      l=iv(i)                                                                   !/*vp4a      237*/
      lp=abs(iptra(l))                                                          !/*vp4a      238*/
      if(lp.eq.0.or.ip(l).ne.0)go to 470                                        !/*vp4a      239*/
      do 450 j=1,nint(ptr(lp))                                                  !/*vp4a      240*/
      if(i.ne.4.and.nint(ptr(lp+j)).eq.k)go to 460                              !/*vp4c2      14*/
  450 if(i.eq.4.and.ptr(lp+j).eq.k+kfq*0.1)go to 460                            !/*vp4c2      15*/
      go to 470                                                                 !/*vp4a      242*/
  460 ip(l)=k                                                                   !/*vp4a      243*/
!        put surface facet in last slot.                                        !/*vp4c2      16*/
      ip(nptr+1)=kfq                                                            !/*vp4c2      17*/
  470 k=nsf(lnsf+jsu)                                                           !/*vp4a      244*/
      return                                                                    !/*vp4a      245*/
!                                                                               !/*vp4a      246*/
! >>>>>  m=7 -- filter the history and move events to the output file.          !/*vp4a      247*/
  480 if(ip(7).eq.0)go to 690                                                   !/*vp4a      248*/
      if(abs(iptr).eq.1)go to 570                                               !/*vp4a      249*/
!                                                                               !/*vp4a      250*/
!        check the cell and surface filters.                                    !/*vp4a      251*/
      if(iptra(2).eq.0)go to 490                                                !/*vp4a      252*/
      if(ip(2).eq.0)go to 680                                                   !/*vp4a      253*/
      ip(2)=ncl(lncl+ip(2))                                                     !/*vp4a      254*/
  490 if(iptra(9).eq.0)go to 500                                                !/*vp4a      255*/
      if(ip(9).eq.0)go to 680                                                   !/*vp4a      256*/
      ip(9)=nsf(lnsf+ip(9))                                                     !/*vp4a      257*/
      ip(nptr+1)=kfq                                                            !/*vp4c2      18*/
  500 if(iptra(10).eq.0)go to 570                                               !/*vp4a      258*/
!                                                                               !/*vp4a      259*/
!        check the tally filters.                                               !/*vp4a      260*/
      lp=abs(iptra(10))                                                         !/*vp4a      261*/
      lv=abs(iptra(12))                                                         !/*vp4a      262*/
      do 550 i=1,nint(ptr(lp))                                                  !/*vp4a      263*/
      it=nint(ptr(lp+i))                                                        !/*vp4a      264*/
      k=abs(it)                                                                 !/*vp4a      265*/
      t=tal(ktal+jptal(ljpt+7,k))                                               !/*vp4a      266*/
      if(t.eq.0.)go to 550                                                      !/*vp4a      267*/
      i2=iptal(lipt+4,2,k)                                                      !/*vp4a      268*/
      i3=iptal(lipt+4,3,k)                                                      !/*vp4a      269*/
      j2=jptal(ljpt+2,k)                                                        !/*vp4a      270*/
      j4=jptal(ljpt+4,k)                                                        !/*vp4a      271*/
      if(i2.ne.0)t=t/tds(i2+i3*(jtf(ljtf+1,k)-1)+jtf(ljtf+4,k))                 !/*vp4a      272*/
      if((j2.eq.6.or.j2.eq.7).and.j4.eq.1)t=t*1.60219e-22                       !/*vpx        10*/
      if(it.gt.0)go to 530                                                      !/*vp4a      274*/
!                                                                               !/*vp4a      275*/
!        find the tfc bin value and the appropriate multiplier.                 !/*vp4a      276*/
      do 510 l=1,20                                                             !/*vp4a      277*/
  510 if(npc(l).eq.0)go to 520                                                  !/*vp4a      278*/
  520 l=max(1,l-1)                                                              !/*vp4a      279*/
      tv=tfc(ltfc+1,l,k)*10.                                                    !/*vp4a      280*/
      if(tv.eq.0.)go to 550                                                     !/*vp4a      281*/
      if(lv.ne.0)tv=tfc(ltfc+1,l,k)*ptr(lv+i)                                   !/*vp4a      282*/
      go to 540                                                                 !/*vp4a      283*/
  530 tv=ptr(lv+i)                                                              !/*vp4a      284*/
  540 if(t.ge.tv)go to 560                                                      !/*vp4a      285*/
  550 continue                                                                  !/*vp4a      286*/
      go to 680                                                                 !/*vp4a      287*/
  560 ip(10)=jptal(ljpt+1,k)                                                    !/*vp4a      288*/
!                                                                               !/*vp4a      289*/
!        write the nps line to the output file.                                 !/*vp4a      290*/
  570 ip(8)=ip(8)+1                                                             !/*vp4a      291*/
      if(ip(4).eq.0)ip(4)=nint(pts(lpts+1))                                     !/*vp4a      292*/
      iw(1)=npstc                                                               !/*vp4a      293*/
      iw(2)=ip(4)                                                               !/*vp4a      294*/
!        check for surface facet for write.                                     !/*vp4c2      19*/
      mq=0                                                                      !/*vp4c2      20*/
      do 580 i=3,nn                                                             !/*vp4a      295*/
      rq(i)=ip(in(i))                                                           !/*vp4c2      21*/
      if(in(i).ne.9)go to 580                                                   !/*vp4c2      22*/
      if(ip(nptr+1).eq.0)go to 580                                              !/*vp4c2      23*/
      mq=i                                                                      !/*vp4c2      24*/
      tq=ip(9)+ip(nptr+1)*0.1                                                   !/*vp4c2      25*/
      rq(i)=tq                                                                  !/*vp4c2      26*/
  580 iw(i)=ip(in(i))                                                           !/*vp4a      296*/
!                                                                               !/*vp4c2      27*/
!        special nps line write with surface facet.                             !/*vp4c2      28*/
      if(mq.eq.0.or.iptra(4).eq.2)go to 589                                     !/*vp4c2      29*/
      hq=' '                                                                    !/*vp4c2      30*/
      do 581 j=1,nn                                                             !/*vp4c2      31*/
      hf=' '                                                                    !/*vp4c2      32*/
      if(j.ne.mq)write(hf,'(i10)')iw(j)                                         !/*vp4c2      33*/
      if(j.eq.mq)write(hf,'(f10.1)')tq                                          !/*vp4c2      34*/
      hq(1+(j-1)*10:j*10)=hf(1:10)                                              !/*vp4c2      35*/
  581 continue                                                                  !/*vp4c2      36*/
      hf=' '                                                                    !/*vp4c2      37*/
      if(iptra(10).ne.0)write(hf,'(1pe13.5)')t                                  !/*vp4c2      38*/
      if(iptra(10).ne.0)hq(1+nn*10:nn*10+13)=hf(1:13)                           !/*vp4c2      39*/
      write(iupw,'(a74)')hq                                                     !/*vp4c2      40*/
      go to 600                                                                 !/*vp4c2      41*/
  589 if(iptra(4).eq.2)go to 590                                                !/*vp4c2      42*/
      if(iptra(10).eq.0)write(iupw,hs)(iw(j),j=1,nn)                            !/*vp4a      298*/
      if(iptra(10).ne.0)write(iupw,hs)(iw(j),j=1,nn),t                          !/*vp4a      299*/
      go to 600                                                                 !/*vp4a      300*/
  590 if(iptra(10).eq.0.and.nn.ge.3)                                            !/*vp4c2      43*/
     & write(iupw)iw(1),iw(2),(rq(j),j=3,nn)                                    !/*vp4c2      44*/
      if(iptra(10).ne.0.and.nn.ge.3)                                            !/*vp4c2      45*/
     & write(iupw)iw(1),iw(2),(rq(j),j=3,nn),real(t)                            !/*vp4c2      46*/
      if(iptra(10).eq.0.and.nn.lt.3)                                            !/*vp4c2      47*/
     & write(iupw)(iw(j),j=1,nn)                                                !/*vp4c2      48*/
      if(iptra(10).ne.0.and.nn.lt.3)                                            !/*vp4c2      49*/
     & write(iupw)(iw(j),j=1,nn),real(t)                                        !/*vp4c2      50*/
!                                                                               !/*vp4a      303*/
!        move the buffer to the scratch or output file.                         !/*vp4a      304*/
  600 iu=iupc                                                                   !/*vp4a      305*/
      if(ip(7).gt.iptra(1))go to 620                                            !/*vp4a      306*/
      iu=iupw                                                                   !/*vp4a      307*/
      if(iptra(4).eq.2)go to 620                                                !/*vp4a      308*/
      do 610 i=1,ip(1)                                                          !/*vp4a      309*/
      lp=lpts+ip(13)*i                                                          !/*vp4a      310*/
      k=nint(abs(pts(lp-ip(13)+1)))/1000                                        !/*vp4a      311*/
!        include macrobody facet of surface.                                    !/*vp4c2      51*/
      if(int(pts(lp+3)).eq.pts(lp+3))go to 605                                  !/*vp4c2      52*/
      nq=iq(1,k)                                                                !/*vp4c2      53*/
      if(nq.gt.10)nq=10                                                         !/*vp4c2      54*/
      write(iu,'(1x,2i10,f10.1,7i10)')int(pts(lp+1)),int(pts(lp+2)),            !/*vp4c2      55*/
     & pts(lp+3),(int(pts(lp+j)),j=4,nq)                                        !/*vp4c2      56*/
      if(iq(1,k).gt.nq)                                                         !/*vp4c2      57*/
     & write(iu,'(1x,10i10)')(int(pts(lp+j)),j=11,iq(1,k))                      !/*vp4c2      58*/
      go to 610                                                                 !/*vp4c2      59*/
  605 write(iu,'(1x,10i10)')(int(pts(lp+j)),j=1,iq(1,k))                        !/*vp4c2      60*/
  610 write(iu,'(1x,10e13.5)')(pts(lp+j),j=iq(1,k)+1,iq(3,k))                   !/*vp4a      313*/
      go to 690                                                                 !/*vp4a      314*/
  620 rp(1)=pts(lpts+1)                                                         !/*vp4a      315*/
      do 640 i=1,ip(1)                                                          !/*vp4a      316*/
      lp=lpts+ip(13)*i                                                          !/*vp4a      317*/
      n=iq(3,nint(abs(rp(1)))/1000)                                             !/*vp4a      318*/
      do 630 j=1,n                                                              !/*vp4a      319*/
  630 rp(j)=pts(lp+j)                                                           !/*vp4a      320*/
  640 write (iu) rp(1:n)                                                        !/*vpx        11*/
      if(iu.eq.iupw)go to 690                                                   !/*vp4a      322*/
!                                                                               !/*vp4a      323*/
!        if necessary, move the scratch file to the output file.                !/*vp4a      324*/
      rewind(iupc)                                                              !/*vp4a      325*/
      rp(1)=ip(4)                                                               !/*vp4a      326*/
      if(iptra(4).eq.2)go to 660                                                !/*vp4a      327*/
      do 650 i=1,ip(7)                                                          !/*vp4a      328*/
      k=nint(abs(rp(1)))/1000                                                   !/*vp4a      329*/
      read (iupc) rp(1:iq(3,k))                                                 !/*vpx        12*/
!        include macrobody facet of surface.                                    !/*vp4c2      61*/
      if(int(rp(3)).eq.rp(3))go to 645                                          !/*vp4c2      62*/
      nq=iq(1,k)                                                                !/*vp4c2      63*/
      if(nq.gt.10)nq=10                                                         !/*vp4c2      64*/
      write(iupw,'(1x,2i10,f10.1,7i10)')int(rp(1)),int(rp(2)),                  !/*vp4c2      65*/
     & rp(3),(int(rp(j)),j=4,nq)                                                !/*vp4c2      66*/
      if(iq(1,k).gt.nq)                                                         !/*vp4c2      67*/
     & write(iupw,'(1x,10i10)')(int(rp(j)),j=11,iq(1,k))                        !/*vp4c2      68*/
      go to 650                                                                 !/*vp4c2      69*/
  645 write(iupw,'(1x,10i10)')(int(rp(j)),j=1,iq(1,k))                          !/*vp4c2      70*/
  650 write(iupw,'(1x,10e13.5)')(rp(j),j=iq(1,k)+1,iq(3,k))                     !/*vp4a      332*/
      go to 690                                                                 !/*vp4a      333*/
  660 do 670 i=1,ip(7)                                                          !/*vp4a      334*/
      n=iq(3,nint(abs(rp(1)))/1000)                                             !/*vp4a      335*/
      read (iupc) rp(1:n)                                                       !/*vpx        13*/
  670 write (iupw) rp(1:n)                                                      !/*vpx        14*/
      go to 690                                                                 !/*vp4a      338*/
!                                                                               !/*vp4a      339*/
!        prepare for the next history.                                          !/*vp4a      340*/
  680 ip(6)=ip(6)-ip(7)                                                         !/*vp4a      341*/
  690 rewind(iupc)                                                              !/*vp4a      342*/
      do 700 i=1,5                                                              !/*vp4a      343*/
  700 ip(i)=0                                                                   !/*vp4a      344*/
      ip(7)=0                                                                   !/*vp4a      345*/
      ip(9)=0                                                                   !/*vp4a      346*/
      if(iptr.lt.0.and.ip(6).ge.iptra(6))nst=nst+256                            !/*vp4a      347*/
      return                                                                    !/*vp4a      348*/
!                                                                               !/*vp4a      349*/
! >>>>>  m=8 -- close the files and write a summary statement.                  !/*vp4a      350*/
  710 close(iupc)                                                               !/*vp4a      351*/
      close(iupw)                                                               !/*vp4a      352*/
      write(jtty,720)ht(iptra(4)),ptrac,ip(6),ip(8)                             !/*vp4a      353*/
      write(iuo,720)ht(iptra(4)),ptrac,ip(6),ip(8)                              !/*vp4a      354*/
  720 format(1x,a6,6h file ,a8,14h written with ,i7,13h events from ,           !/*vp4a      355*/
     & i7,11h histories.)                                                       !/*vp4a      356*/
      return                                                                    !/*vp4a      357*/
      end                                                                       !/*vp4a      358*/
