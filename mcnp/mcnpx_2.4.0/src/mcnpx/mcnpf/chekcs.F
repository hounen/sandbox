!#ident chekcs.F from deck ch                                                   !/*ch          1*/
      subroutine chekcs                                                         !/*ch          2*/
!        simplify and check the cell descriptions.                              !/*ch          3*/
      use global_data                                                           !/*chx         1*/
      use imcn_data                                                             !/*chx         2*/
!                                                                               !/*chx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*chx         4*/
      implicit integer(kindi) (i-n)                                             !/*chx         5*/
!                                                                               !/*chx         6*/
      dimension iu(0:mxlv),il(0:mxlv),mu(0:mxlv),jm(0:mxlv),nu(0:mxlv)          !/*ch          6*/
      character ha*1,hb*2,hc*2                                                  !/*ch4c        1*/
      data ha/'<'/                                                              !/*ch4b        2*/
!                                                                               !/*ch          7*/
!        simplify and expand the cell descriptions.                             !/*ch          8*/
      do 260 ic=1,mxa                                                           !/*ch          9*/
      m1=abs(lca(llca+ic))                                                      !/*ch         10*/
   10 m2=abs(lca(llca+ic+1))-1                                                  !/*ch         11*/
!                                                                               !/*ch         12*/
!        find and check the next #n descriptor.                                 !/*ch         13*/
      do 30 j=m1,m2                                                             !/*ch         14*/
      j1=lja(llja+j+1)                                                          !/*ch         15*/
      if(lja(llja+j).ne.1000004.or.j1.eq.1000001)go to 30                       !/*ch         16*/
      if(j1.lt.1000000.and.j.ne.m2.and.j1.ne.ncl(lncl+ic))go to 20              !/*ch         17*/
      call erprnt(1,1,1,ncl(lncl+ic),0,0,0,0,                                   !/*ch         18*/
     & '41h# used incorrectly in description of cell,i5')                       !/*ch         19*/
      go to 250                                                                 !/*ch         20*/
   20 n=namchg(1,j1)                                                            !/*ch         21*/
      if(n.ne.0)go to 40                                                        !/*ch         22*/
      call erprnt(1,1,2,j1,ncl(lncl+ic),0,0,0,                                  !/*ch         23*/
     & '4hcell,i5,23h in description of cell,i5,16h is not defined.')           !/*ch         24*/
      go to 250                                                                 !/*ch         25*/
   30 continue                                                                  !/*ch         26*/
      go to 80                                                                  !/*ch         27*/
!                                                                               !/*ch         28*/
!        expand the #n descriptor.                                              !/*ch         29*/
   40 lh=abs(lca(llca+n+1))-abs(lca(llca+n))                                    !/*ch         30*/
      do 50 iz=1,nlja-j                                                         !/*ch         31*/
      i=nlja-j+1-iz                                                             !/*ch         32*/
      idna(lida+j+lh+2+i)=idna(lida+j+1+i)                                      !/*ch4c        2*/
   50 lja(llja+j+lh+2+i)=lja(llja+j+1+i)                                        !/*ch         33*/
      lja(llja+j+1)=1000001                                                     !/*ch         34*/
      lja(llja+j+lh+2)=1000002                                                  !/*ch         35*/
      idna(lida+j+1)=0                                                          !/*ch4c        3*/
      idna(lida+j+lh+2)=0                                                       !/*ch4c        4*/
      nlja=nlja+lh+1                                                            !/*ch         36*/
      do 60 i=ic,mxa                                                            !/*ch         37*/
   60 lca(llca+i+1)=lca(llca+i+1)+sign(lh+1,lca(llca+i+1))                      !/*ch         38*/
      do 70 i=1,lh                                                              !/*ch         39*/
      idna(lida+j+1+i)=idna(lida+abs(lca(llca+n))+i-1)                          !/*ch4c        5*/
   70 lja(llja+j+1+i)=lja(llja+abs(lca(llca+n))+i-1)                            !/*ch         40*/
      go to 10                                                                  !/*ch         41*/
!                                                                               !/*ch         42*/
!        find the next # complementation operator.                              !/*ch         43*/
   80 do 100 j=m1,m2                                                            !/*ch         44*/
      if(lja(llja+j).ne.1000004)go to 100                                       !/*ch         45*/
      np=0                                                                      !/*ch         46*/
      do 90 k=j,m2                                                              !/*ch         47*/
      if(lja(llja+k+1).eq.1000001)np=np+1                                       !/*ch         48*/
      if(lja(llja+k+1).eq.1000002)np=np-1                                       !/*ch         49*/
      if(np.eq.0)go to 110                                                      !/*ch         50*/
   90 if(lja(llja+k+1).eq.1000004)go to 100                                     !/*ch         51*/
      go to 250                                                                 !/*ch         52*/
  100 continue                                                                  !/*ch         53*/
      go to 180                                                                 !/*ch         54*/
!                                                                               !/*ch         55*/
!        multiply the complementation operator out.                             !/*ch         56*/
  110 do 120 i=1,nlja-j+1                                                       !/*ch         57*/
      idna(lida+j+i-1)=idna(lida+j+i)                                           !/*ch4c        6*/
  120 lja(llja+j+i-1)=lja(llja+j+i)                                             !/*ch         58*/
      nlja=nlja-1                                                               !/*ch         59*/
      do 130 i=ic,mxa                                                           !/*ch         60*/
  130 lca(llca+i+1)=lca(llca+i+1)-sign(1,lca(llca+i+1))                         !/*ch         61*/
      np=0                                                                      !/*ch         62*/
  140 if(lja(llja+j).eq.1000001)np=np+1                                         !/*ch         63*/
      if(lja(llja+j).eq.1000002)np=np-1                                         !/*ch         64*/
      if(np.eq.0)go to 10                                                       !/*ch         65*/
      if(lja(llja+j).lt.1000000)lja(llja+j)=-lja(llja+j)                        !/*ch         66*/
      if(lja(llja+j).eq.1000001.or.lja(llja+j).ne.1000003.and.                  !/*ch         67*/
     & lja(llja+j+1).gt.1000001)go to 170                                       !/*ch         68*/
      do 150 iz=1,nlja-j+1                                                      !/*ch         69*/
      i=nlja-j+2-iz                                                             !/*ch         70*/
      idna(lida+j+i+1)=idna(lida+j+i)                                           !/*ch4c        7*/
  150 lja(llja+j+1+i)=lja(llja+j+i)                                             !/*ch         71*/
      do 160 i=ic,mxa                                                           !/*ch         72*/
  160 lca(llca+i+1)=lca(llca+i+1)+sign(1,lca(llca+i+1))                         !/*ch         73*/
      idna(lida+j+1)=0                                                          !/*ch4c        8*/
      if(lja(llja+j).ne.1000003)lja(llja+j+1)=1000003                           !/*ch         74*/
      if(lja(llja+j).eq.1000003)lja(llja+j+1)=1000001                           !/*ch         75*/
      if(lja(llja+j).eq.1000003)lja(llja+j)=1000002                             !/*ch         76*/
      nlja=nlja+1                                                               !/*ch         77*/
      m2=m2+1                                                                   !/*ch         78*/
      j=j+1                                                                     !/*ch         79*/
  170 j=j+1                                                                     !/*ch         80*/
      if(j.le.m2)go to 140                                                      !/*ch         81*/
      go to 250                                                                 !/*ch         82*/
!                                                                               !/*ch         83*/
!        remove any redundant parentheses from the cell description.            !/*ch         84*/
  180 do 200 ip=m1,m2                                                           !/*ch         85*/
      if(lja(llja+ip).ne.1000001)go to 200                                      !/*ch         86*/
      ng=0                                                                      !/*ch         87*/
      np=0                                                                      !/*ch         88*/
      do 190 l=ip,m2                                                            !/*ch         89*/
      if(lja(llja+l).eq.1000001)np=np+1                                         !/*ch         90*/
      if(lja(llja+l).eq.1000003.and.np.eq.1)ng=1                                !/*ch         91*/
      if(lja(llja+l).eq.1000002)np=np-1                                         !/*ch         92*/
      if(np.eq.0.and.(ng.eq.0.or.ip.eq.m1.and.l.eq.m2))go to 210                !/*ch         93*/
  190 if(np.eq.0)go to 200                                                      !/*ch         94*/
      go to 250                                                                 !/*ch         95*/
  200 continue                                                                  !/*ch         96*/
      go to 250                                                                 !/*ch         97*/
  210 do 220 i=1,nlja-ip+1                                                      !/*ch         98*/
      idna(lida+ip+i-1)=idna(lida+ip+i)                                         !/*ch4c        9*/
  220 lja(llja+ip+i-1)=lja(llja+ip+i)                                           !/*ch         99*/
      do 230 i=1,nlja-l+1                                                       !/*ch        100*/
      idna(lida+l+i-2)=idna(lida+l+i-1)                                         !/*ch4c       10*/
  230 lja(llja+l+i-2)=lja(llja+l+i-1)                                           !/*ch        101*/
      nlja=nlja-2                                                               !/*ch        102*/
      do 240 i=ic,mxa                                                           !/*ch        103*/
  240 lca(llca+i+1)=lca(llca+i+1)-sign(2,lca(llca+i+1))                         !/*ch        104*/
      go to 10                                                                  !/*ch        105*/
  250 if(m2-m1.ge.mlgc)call erprnt(1,1,3,ncl(lncl+ic),m2-m1+1,mlgc,0,0,         !/*ch        106*/
     & '19hdescription of cell,i5,5h uses,i4,9h words.  ,i5,8h is max.')        !/*ch        107*/
  260 continue                                                                  !/*ch        108*/
!                                                                               !/*ch        109*/
!        check the definitions of the surfaces of all the cells.                !/*ch        110*/
      do 290 ic=1,mxa                                                           !/*ch        111*/
      do 280 la=abs(lca(llca+ic)),abs(lca(llca+ic+1))-1                         !/*ch        112*/
      if(lja(llja+la).gt.1000000)go to 280                                      !/*ch        113*/
      do 270 jb=1,mxj                                                           !/*ch        114*/
  270 if(abs(lja(llja+la)).eq.nsf(lnsf+jb))go to 280                            !/*ch        115*/
      call erprnt(1,1,2,lja(llja+la),ncl(lncl+ic),0,0,0,                        !/*ch        116*/
     & '7hsurface,i7,8h of cell,i5,16h is not defined.')                        !/*ch        117*/
  280 continue                                                                  !/*ch        118*/
  290 continue                                                                  !/*ch        119*/
!                                                                               !/*ch        120*/
!        change the contents of lja from names to indexes.                      !/*ch        121*/
      do 300 j=1,nlja                                                           !/*ch        122*/
      j1=lja(llja+j)                                                            !/*ch        123*/
      if(j1.ge.1000000)go to 300                                                !/*ch4c       11*/
      j2=namchg(2,abs(j1))                                                      !/*ch4c       12*/
      lja(llja+j)=sign(j2+abs(idna(lida+j)),j1)                                 !/*ch4c       13*/
  300 continue                                                                  !/*ch4c       14*/
!                                                                               !/*ch        125*/
!        prepare to delete all identical surfaces.  the surface is              !/*ch4c       15*/
!        not physically deleted, but is removed from cell cards and             !/*ch4c       16*/
!        tallies so it is not used anywhere.  also deleted if the               !/*ch4c       17*/
!        same except for sign, then sign is swiched on cell cards.              !/*ch4c       18*/
!                                                                               !/*ch4c       19*/
!        set up arrays involving identical surfaces.                            !/*ch4c       20*/
      mc=0                                                                      !/*ch4c       21*/
      in=0                                                                      !/*ch4c       22*/
      do 895 ja=1,mxj-1                                                         !/*ch4c       23*/
      if(ksm(lksm+ja).lt.0)go to 895                                            !/*ch4c       24*/
      if(idnt(lidt+ja).ne.0)go to 895                                           !/*ch4c       25*/
      na=lsc(llsc+ja+1)-lsc(llsc+ja)                                            !/*ch4c       26*/
      do 870 j=ja+1,mxj                                                         !/*ch4c       27*/
      if(ksm(lksm+j).lt.0)go to 870                                             !/*ch4c       28*/
      if(idnt(lidt+j).ne.0)go to 870                                            !/*ch4c       29*/
      is=1                                                                      !/*ch4c       30*/
      if(kst(lkst+ja).gt.4.or.kst(lkst+j).gt.4)go to 820                        !/*ch4c       31*/
!                                                                               !/*ch4c       32*/
!        check for identical planes.                                            !/*ch4c       33*/
      if(kst(lkst+ja).eq.1.and.kst(lkst+j).eq.1)go to 810                       !/*ch4c       34*/
      if(kst(lkst+ja).eq.1.or.kst(lkst+j).eq.1)go to 800                        !/*ch4c       35*/
      if(kst(lkst+ja).ne.kst(lkst+j))go to 870                                  !/*ch4c       36*/
      if(abs(scf(lsc(llsc+ja)+1)-scf(lsc(llsc+j)+1)).gt.                        !/*ch4c       37*/
     & 1e-12*abs(scf(lsc(llsc+ja)+1)+scf(lsc(llsc+j)+1)))go to 870              !/*ch4c       38*/
      go to 840                                                                 !/*ch4c       39*/
  800 j1=ja                                                                     !/*ch4c       40*/
      j2=j                                                                      !/*ch4c       41*/
      if(kst(lkst+ja).ne.1)j1=j                                                 !/*ch4c       42*/
      if(kst(lkst+ja).ne.1)j2=ja                                                !/*ch4c       43*/
      j3=kst(lkst+j2)-1                                                         !/*ch4c       44*/
      if(scf(lsc(llsc+j1)+j3).eq.0.)go to 870                                   !/*ch4c       45*/
      do 805 k=1,3                                                              !/*ch4c       46*/
  805 if(k.ne.j3.and.abs(scf(lsc(llsc+j1)+k)/scf(lsc(llsc+j1)+j3)).gt.          !/*ch4c       47*/
     & 1.e-12)go to 870                                                         !/*ch4c       48*/
      d=scf(lsc(llsc+j1)+4)/scf(lsc(llsc+j1)+j3)                                !/*ch4c       49*/
      if(abs(scf(lsc(llsc+j2)+1)-d).gt.                                         !/*ch4c       50*/
     & 1e-12*abs(scf(lsc(llsc+j2)+1)+d))go to 870                               !/*ch4c       51*/
      if(scf(lsc(llsc+j1)+j3).lt.0.)is=-1                                       !/*ch4c       52*/
      go to 840                                                                 !/*ch4c       53*/
  810 j3=1                                                                      !/*ch4c       54*/
      g=abs(scf(lsc(llsc+ja)+1))                                                !/*ch4c       55*/
      if(abs(scf(lsc(llsc+ja)+2)).gt.g)j3=2                                     !/*ch4c       56*/
      if(abs(scf(lsc(llsc+ja)+2)).gt.g)g=abs(scf(lsc(llsc+ja)+2))               !/*ch4c       57*/
      if(abs(scf(lsc(llsc+ja)+3)).gt.g)j3=3                                     !/*ch4c       58*/
      if(abs(scf(lsc(llsc+ja)+3)).gt.g)g=abs(scf(lsc(llsc+ja)+3))               !/*ch4c       59*/
      if(g.eq.0.)go to 870                                                      !/*ch4c       60*/
      g=scf(lsc(llsc+j)+j3)/scf(lsc(llsc+ja)+j3)                                !/*ch4c       61*/
      do 815 k=1,4                                                              !/*ch4c       62*/
      if(k.eq.j3)go to 815                                                      !/*ch4c       63*/
      d=g*scf(lsc(llsc+ja)+k)                                                   !/*ch4c       64*/
      if(abs(scf(lsc(llsc+j)+k)-d).gt.                                          !/*ch4c       65*/
     & 1e-12*abs(scf(lsc(llsc+j)+k)+d))go to 870                                !/*ch4c       66*/
  815 continue                                                                  !/*ch4c       67*/
      if(g.lt.0.)is=-1                                                          !/*ch4c       68*/
      go to 840                                                                 !/*ch4c       69*/
  820 if(kst(lkst+ja).ne.kst(lkst+j))go to 870                                  !/*ch4c       70*/
!                                                                               !/*ch4c       71*/
!        check surfaces other than planes.                                      !/*ch4c       72*/
      if(kst(lkst+ja).le.21.or.kst(lkst+ja).ge.24)go to 830                     !/*ch4c       73*/
      do 825 i=1,na                                                             !/*ch4c       74*/
      if(scf(lsc(llsc+ja)+i).eq.0.)go to 825                                    !/*ch4c       75*/
      if(scf(lsc(llsc+ja)+i)*scf(lsc(llsc+j)+i).lt.0.)is=-1                     !/*ch4c       76*/
      go to 830                                                                 !/*ch4c       77*/
  825 continue                                                                  !/*ch4c       78*/
  830 do 835 i=1,na                                                             !/*ch4c       79*/
  835 if(abs(scf(lsc(llsc+ja)+i)*is-scf(lsc(llsc+j)+i)).gt.                     !/*ch4c       80*/
     & 1e-12*abs(scf(lsc(llsc+ja)+i)*is+scf(lsc(llsc+j)+i)))go to 870           !/*ch4c       81*/
  840 hb=' '                                                                    !/*ch4c       82*/
      ka=nsf(lnsf+ja)                                                           !/*ch4c       83*/
      mq=kfq                                                                    !/*ch4c       84*/
      if(kfq.ne.0)write(hb,'(1h.,i1)')kfq                                       !/*ch4c       85*/
      hc=' '                                                                    !/*ch4c       86*/
      k=nsf(lnsf+j)                                                             !/*ch4c       87*/
      if(kfq.ne.0)write(hc,'(1h.,i1)')kfq                                       !/*ch4c       88*/
      write(iuo,850)ka,hb,k,hc,k,hc                                             !/*ch4c       89*/
  850 format(/8h surface,i7,a2,12h and surface,i7,a2,                           !/*ch4c       90*/
     & 14h are the same.,i7,a2,17h will be deleted.)                            !/*ch4c       91*/
      if(idnt(lidt+ja).ne.0)go to 860                                           !/*ch4c       92*/
      in=in+1                                                                   !/*ch4c       93*/
      idnt(lidt+ja)=ja                                                          !/*ch4c       94*/
      if(idne(lide+1).eq.0)idne(lide+2)=2                                       !/*ch4c       95*/
      idne(lide+1)=idne(lide+1)+1                                               !/*ch4c       96*/
      idne(lide+idne(lide+2)+1)=1                                               !/*ch4c       97*/
      idns(lids+ja)=idne(lide+2)+1                                              !/*ch4c       98*/
      idne(lide+2)=idne(lide+2)+2                                               !/*ch4c       99*/
      idne(lide+idne(lide+2))=ja                                                !/*ch4c      100*/
  860 idnt(lidt+j)=idnt(lidt+ja)*is                                             !/*ch4c      103*/
      jq=idns(lids+ja)                                                          !/*ch4c      104*/
      idns(lids+j)=jq                                                           !/*ch4c      105*/
      idne(lide+jq)=idne(lide+jq)+1                                             !/*ch4c      106*/
      idne(lide+2)=idne(lide+2)+1                                               !/*ch4c      107*/
      idne(lide+idne(lide+2))=j                                                 !/*ch4c      108*/
      if(ksu(lksu+ja).gt.0.or.ksu(lksu+ja).ne.ksu(lksu+j))call erprnt           !/*ch4c2       1*/
     & (2,1,0,0,0,0,0,0,'40hboundary condition on identical surface.')          !/*ch4c2       2*/
      mc=mc+1                                                                   !/*ch4c      111*/
!                                                                               !/*ch4c      112*/
!        change deleted surface on cell cards.                                  !/*ch4c      113*/
      do 865 k=1,mlja                                                           !/*ch4c      114*/
      if(abs(lja(llja+k)).ne.j)go to 865                                        !/*ch4c      115*/
      lja(llja+k)=sign(ja,is*lja(llja+k))                                       !/*ch4c      116*/
      idna(lida+k)=mq                                                           !/*ch4c      117*/
  865 continue                                                                  !/*ch4c      118*/
  870 continue                                                                  !/*ch4c      119*/
      if(idnt(lidt+ja).eq.0)go to 895                                           !/*ch4c      120*/
!                                                                               !/*ch4c      121*/
!        make master identical surface a non-facet if possible.                 !/*ch4c      122*/
      if(ksm(lksm+ja).eq.0)go to 895                                            !/*ch4c      123*/
      do 875 jq=2,idne(lide+idns(lids+ja))                                      !/*ch4c      124*/
      j1=idne(lide+idns(lids+ja)+jq)                                            !/*ch4c      125*/
  875 if(ksm(lksm+j1).eq.0)go to 880                                            !/*ch4c      126*/
      go to 895                                                                 !/*ch4c      127*/
  880 is=1                                                                      !/*ch4c      128*/
      if(idnt(lidt+j1).lt.0)is=-1                                               !/*ch4c      129*/
      iq=j1*is                                                                  !/*ch4c      130*/
      idnt(lidt+j1)=j1                                                          !/*ch4c      131*/
      idnt(lidt+ja)=j1*is                                                       !/*ch4c      132*/
      idne(lide+idns(lids+ja)+jq)=ja                                            !/*ch4c      133*/
      idne(lide+idns(lids+ja)+1)=j1                                             !/*ch4c      134*/
      do 885 k=1,mlja                                                           !/*ch4c      135*/
      if(abs(lja(llja+k)).ne.ja)go to 885                                       !/*ch4c      136*/
      lja(llja+k)=sign(j1,is*lja(llja+k))                                       !/*ch4c      137*/
      idna(lida+k)=0                                                            !/*ch4c      138*/
  885 continue                                                                  !/*ch4c      139*/
      if(idne(lide+idns(lids+ja)).le.2)go to 895                                !/*ch4c      140*/
      do 890 j2=2,idne(lide+idns(lids+ja))                                      !/*ch4c      141*/
      j3=idne(lide+idns(lids+ja)+j2)                                            !/*ch4c      142*/
      if(ja.eq.j3)go to 890                                                     !/*ch4c      143*/
      j4=idnt(lidt+j3)                                                          !/*ch4c      144*/
      if(j4.gt.0)idnt(lidt+j3)=iq                                               !/*ch4c      145*/
      if(j4.lt.0)idnt(lidt+j3)=-iq                                              !/*ch4c      146*/
  890 continue                                                                  !/*ch4c      147*/
  895 continue                                                                  !/*ch4c      148*/
      if(mc.ne.0)call erprnt(1,2,1,mc,0,0,0,0,                                  !/*ch        166*/
     & 'i4,52h surfaces were deleted for being the same as others.')            !/*ch        167*/
!                                                                               !/*ch        168*/
!        mark duplicate mentions of a surface as ambiguity surfaces.            !/*ch        169*/
      do 400 i=1,nlja+1                                                         !/*ch        170*/
  400 lcaj(llcj+i)=llaj+1                                                       !/*ch        171*/
      do 440 ic=1,mxa                                                           !/*ch        172*/
      do 430 j=abs(lca(llca+ic))+1,abs(lca(llca+ic+1))-1                        !/*ch        173*/
      do 410 jb=abs(lca(llca+ic))+1,j                                           !/*ch        174*/
  410 if(abs(lja(llja+jb-1)).eq.abs(lja(llja+j)))go to 420                      !/*ch        175*/
      go to 430                                                                 !/*ch        176*/
  420 lcaj(llcj+j)=-llaj-1                                                      !/*ch        177*/
  430 continue                                                                  !/*ch        178*/
  440 continue                                                                  !/*ch        179*/
!                                                                               !/*ch        180*/
!        mark logical operators.  replicate lcaj for multitasking.              !/*ch        181*/
      do 450 i=1,nlja                                                           !/*ch        182*/
  450 if(lja(llja+i).gt.1000000)lcaj(llcj+i)=-llaj-1                            !/*ch        183*/
      do 460 i=1,mtasks-1                                                       !/*ch        184*/
      do 460 j=1,nlja+1                                                         !/*ch        185*/
  460 lcaj(llcj+(mlja+1)*i+j)=lcaj(llcj+(mlja+1)*(i-1)+j)+                      !/*ch        186*/
     & sign(mlaj+mxa,lcaj(llcj+(mlja+1)*(i-1)+j))                               !/*ch        187*/
!                                                                               !/*ch        188*/
!        flag cells that have one or more sq, gq, or tori or are void.          !/*ch        189*/
      do 480 i=1,mxa                                                            !/*chx        50*/
      if(mat(lmat+i).eq.0)mbd(lmbd+i)=1                                         !/*ch        191*/
      do 470 j1=abs(lca(llca+i)),abs(lca(llca+i+1))-1                           !/*ch        192*/
      if(lcaj(llcj+j1).le.0.or.lja(llja+j1).gt.1000000)go to 470                !/*ch        193*/
      if(kst(lkst+abs(lja(llja+j1))).gt.21)mbd(lmbd+i)=1                        !/*ch4a       21*/
  470 continue                                                                  !/*ch        195*/
  480 continue                                                                  !/*ch        196*/
!                                                                               !/*ch        197*/
!        check whether there are too many levels of universes.                  !/*ch        198*/
      if(junf.eq.0)return                                                       !/*ch        199*/
      do 490 ic=1,mxa                                                           !/*ch        200*/
      if(mfl(lmfl+1,ic).ne.0.and.mfl(lmfl+1,ic).eq.abs(jun(ljun+ic)))           !/*ch        201*/
     & call erprnt(1,1,1,ncl(lncl+ic),0,0,0,0,                                  !/*ch        202*/
     & '4hcell,i6,33h is filled with its own universe.')                        !/*ch        203*/
      if(lat(llat+1,ic).eq.0.and.mfl(lmfl+1,ic).lt.0)                           !/*ch4b        3*/
     & call erprnt(1,1,1,ncl(lncl+ic),0,0,0,0,                                  !/*ch4b        4*/
     & '16hnon-lattice cell,i6,16h has fill array.')                            !/*ch4b        5*/
  490 if(mfl(lmfl+1,ic).eq.0)nlv(lnlv+ic)=1                                     !/*ch        204*/
  500 do 530 ic=1,mxa                                                           !/*ch        205*/
      if(nlv(lnlv+ic).ne.0)go to 530                                            !/*ch        206*/
      l=0                                                                       !/*ch        207*/
      lp=-mfl(lmfl+1,ic)                                                        !/*ch        208*/
      na=1                                                                      !/*ch        209*/
      if(lp.gt.0)na=laf(lp+1,2)*laf(lp+2,2)*laf(lp+3,2)                         !/*ch        210*/
      do 520 j=3,na+2                                                           !/*ch        211*/
      lu=-lp                                                                    !/*ch        212*/
      if(lp.gt.0)lu=laf(lp+1,j)                                                 !/*ch        213*/
      if(lu.eq.0.or.lu.eq.abs(jun(ljun+ic)))go to 520                           !/*ch        214*/
      do 510 i=1,mxa                                                            !/*ch        215*/
      if(abs(jun(ljun+i)).ne.lu)go to 510                                       !/*ch        216*/
      if(nlv(lnlv+i).eq.0)go to 530                                             !/*ch        217*/
      l=max(l,nlv(lnlv+i)+1)                                                    !/*ch        218*/
  510 continue                                                                  !/*ch        219*/
      if(l.eq.0)call erprnt(1,1,2,lu,ncl(lncl+ic),0,0,0,                        !/*ch4a       22*/
     & '8huniverse,i6,17h which fills cell,i6,14h has no cells.')               !/*ch        221*/
  520 continue                                                                  !/*ch        222*/
      nlv(lnlv+ic)=max(l,1)                                                     !/*ch        223*/
      nlev=max(nlev,l)                                                          !/*ch        224*/
      go to 500                                                                 !/*ch        225*/
  530 continue                                                                  !/*ch        226*/
      if(nlev.gt.mxlv)call erprnt(1,1,2,nlev,mxlv,0,0,0,                        !/*ch        227*/
     & 'i2,21h levels of universes.,i4,12h is maximum.')                        !/*ch        228*/
      if(nlev.gt.mxlv)return                                                    !/*ch        229*/
!                                                                               !/*ch        230*/
!        check that no surface appears more than once in any chain.             !/*ch        231*/
      nt=0                                                                      !/*ch4b        6*/
      lv=0                                                                      !/*ch        232*/
      iu(0)=0                                                                   !/*ch        233*/
  540 il(lv)=1                                                                  !/*ch        234*/
  550 if(jun(ljun+il(lv)).ne.iu(lv))go to 640                                   !/*ch        235*/
      do 580 l=0,lv-1                                                           !/*ch        236*/
      do 570 k=abs(lca(llca+il(lv))),abs(lca(llca+il(lv)+1))-1                  !/*ch        237*/
      jk=abs(lja(llja+k))                                                       !/*ch4b        7*/
      if(jk.gt.1000000)go to 570                                                !/*ch4b        8*/
      ks=1                                                                      !/*ch4c      149*/
      if(kst(lkst+jk).le.4)ks=2                                                 !/*ch4c      150*/
      do 565 j=abs(lca(llca+il(l))),abs(lca(llca+il(l)+1))-1                    !/*ch4b        9*/
      jj=abs(lja(llja+j))                                                       !/*ch4b       10*/
      if(jk.ne.jj)go to 565                                                     !/*ch4b       11*/
      do 555 jt=1,nt                                                            !/*ch4b       12*/
  555 if(nint(tpp(jt)).eq.jj)go to 565                                          !/*ch4b       13*/
      ka=nsf(lnsf+jj)                                                           !/*ch4c      151*/
      hb=' '                                                                    !/*ch4c      152*/
      if(kfq.ne.0)write(hb,'(1h.,i1)')kfq                                       !/*ch4c      153*/
      call erprnt(1,ks,1,ka,0,0,0,0,                                            !/*ch4c      154*/
     & '7hsurface,i7,37h'//hb//' appears more than once in a chain.')           !/*ch4c      155*/
      write(iuo,560)ka,hb,ncl(lncl+il(lv)),ncl(lncl+il(l)),                     !/*ch4c      156*/
     & ncl(lncl+il(lv)),('<',ncl(lncl+il(lv-i)),i=1,lv)                         !/*ch4b       17*/
  560 format(/8h surface,i7,a2,12h is in cells,i6,4h and,i6,                    !/*ch4c      157*/
     & 9h in chain/ i6,15(2x,a1,i5))                                            !/*ch4c      158*/
      nt=nt+1                                                                   !/*ch4b       20*/
      if(nt.gt.20)go to 670                                                     !/*ch4b       21*/
      tpp(nt)=jj                                                                !/*ch4b       22*/
  565 continue                                                                  !/*ch4b       23*/
  570 continue                                                                  !/*ch        241*/
  580 continue                                                                  !/*ch        242*/
      mu(lv)=mfl(lmfl+1,il(lv))                                                 !/*ch        243*/
      if(mu(lv).eq.0)go to 640                                                  !/*ch        244*/
      iu(lv+1)=mu(lv)                                                           !/*ch        245*/
      if(mu(lv).gt.0)go to 610                                                  !/*ch        246*/
      jm(lv)=3                                                                  !/*ch        247*/
      nu(lv)=laf(-mu(lv)+1,2)*laf(-mu(lv)+2,2)*laf(-mu(lv)+3,2)                 !/*ch        248*/
  590 iu(lv+1)=laf(-mu(lv)+1,jm(lv))                                            !/*ch        249*/
      if(iu(lv+1).eq.0.or.iu(lv+1).eq.iu(lv))go to 630                          !/*ch        250*/
      do 600 j=3,jm(lv)-1                                                       !/*ch        251*/
  600 if(iu(lv+1).eq.laf(-mu(lv)+1,j))go to 630                                 !/*ch        252*/
  610 lv=lv+1                                                                   !/*ch        253*/
      go to 540                                                                 !/*ch        254*/
  620 lv=lv-1                                                                   !/*ch        255*/
      if(mu(lv).ge.0)go to 640                                                  !/*ch        256*/
  630 jm(lv)=jm(lv)+1                                                           !/*ch        257*/
!         save time: do not check > 100000 times for this warning.              !/*chx        51*/
      if(jm(lv).le.nu(lv)+2.and.jm(lv).le.100000)go to 590                      !/*chx        52*/
  640 il(lv)=il(lv)+1                                                           !/*ch        259*/
      if(il(lv).le.mxa)go to 550                                                !/*ch        260*/
      if(lv.ne.0)go to 620                                                      !/*ch        261*/
!                                                                               !/*ch4b       24*/
!        set flags (ksc) for parallel and possibly coincident surfaces.         !/*ch4b       25*/
!        ksc(lksc+j)=0 nonplanar; =2/3/4 for px/py/pz; >4 for p.                !/*ch4b       26*/
  670 nt=4                                                                      !/*ch4b       27*/
      coincd=.0001                                                              !/*ch4b       28*/
      if(dbcn(9).ne.0.)coincd=dbcn(9)                                           !/*ch4b       29*/
      do 690 js=1,mxj                                                           !/*ch4b       30*/
      if(kst(lkst+js).gt.4)go to 690                                            !/*ch4b       31*/
      if(kst(lkst+js).gt.1)ksc(lksc+js)=kst(lkst+js)                            !/*ch4b       32*/
      if(ksc(lksc+js).gt.1)go to 690                                            !/*ch4b       33*/
      nt=nt+1                                                                   !/*ch4b       34*/
      ksc(lksc+js)=nt                                                           !/*ch4b       35*/
      is=lsc(llsc+js)+1                                                         !/*ch4b       36*/
      ts=scf(is)**2+scf(is+1)**2+scf(is+2)**2                                   !/*ch4b       37*/
      if(scf(is+1)**2+scf(is+2)**2.lt.1.e-10*ts)ksc(lksc+js)=2                  !/*ch4b       38*/
      if(scf(is)**2+scf(is+2)**2.lt.1.e-10*ts)ksc(lksc+js)=3                    !/*ch4b       39*/
      if(scf(is)**2+scf(is+1)**2.lt.1.e-10*ts)ksc(lksc+js)=4                    !/*ch4b       40*/
      do 680 jk=js+1,mxj                                                        !/*ch4b       41*/
      if(kst(lkst+jk).gt.1)go to 680                                            !/*ch4b       42*/
      if(ksc(lksc+jk).ne.0)go to 680                                            !/*ch4b       43*/
      ik=lsc(llsc+jk)+1                                                         !/*ch4b       44*/
      tk=scf(ik)**2+scf(ik+1)**2+scf(ik+2)**2                                   !/*ch4b       45*/
      if((scf(is)*scf(ik)+scf(is+1)*scf(ik+1)+scf(is+2)*scf(ik+2))**2           !/*ch4b       46*/
     & .gt..9998*ts*tk)ksc(lksc+jk)=ksc(lksc+js)                                !/*ch4b       47*/
  680 continue                                                                  !/*ch4b       48*/
  690 continue                                                                  !/*ch4b       49*/
      return                                                                    !/*ch        265*/
      end                                                                       !/*ch        266*/
