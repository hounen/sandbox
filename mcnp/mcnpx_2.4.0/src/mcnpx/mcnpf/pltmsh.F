!#ident pltmsh.F from deck pum4c2                                               !/*pum4c2      1*/
      subroutine pltmsh(nc,lp)                                                  !/*pum4c2      3*/
#ifdef PLOT                                                                     /*!pumx        1*/
!        plot superimposed mesh.  put points defining mesh in zst array.        !/*pum4c2      4*/
!        superimposed mesh comes from wwinp file.                               !/*pum4c2      5*/
!        increment nc by the number of arcs generated                           !/*pum4c2      6*/
!        increment lp by the number of entries added to zst.                    !/*pum4c2      7*/
      use global_data                                                           !/*pumx        2*/
      use plot_data                                                             !/*pumx        3*/
!                                                                               !/*pumx        4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pumx        5*/
      implicit integer(kindi) (i-n)                                             !/*pumx        6*/
!                                                                               !/*pumx        7*/
      dimension aa(4,4),bb(4,4),cc(4,4),cf(3),co(6,7),km(7),pv(4,4),            !/*pum4c2     10*/
     & qg(4),qp(4),qq(3,3,7),qr(2)                                              !/*pum4c2     11*/
!                                                                               !/*pum4c2     12*/
!        save and reset kst for use in regula.                                  !/*pum4c2     13*/
      js=mxj                                                                    !/*pum4c2     14*/
      ks=kst(lkst+js)                                                           !/*pum4c2     15*/
      kst(lkst+js)=1                                                            !/*pum4c2     16*/
      la=lp                                                                     !/*pum4c2     17*/
      kx=0                                                                      !/*pum4c2     18*/
!                                                                               !/*pum4c2     19*/
!        loop over and plot each mesh surface.  kk=1/2/3=x/y/z=r/z/t            !/*pum4c2     20*/
      ll=0                                                                      !/*pum4c2     21*/
      do 620 kk=1,3                                                             !/*pum4c2     22*/
      if(kk.gt.1)ll=ll+3*(nint(wwm(kk+5))+1)                                    !/*pum4c2     23*/
!                                                                               !/*pum4c2     24*/
!        loop over coarse mesh.                                                 !/*pum4c2     25*/
      do 610 ih=0,nint(wwm(kk+6))                                               !/*pum4c2     26*/
      im=lwgm+ll+3*ih                                                           !/*pum4c2     27*/
      nf=1                                                                      !/*pum4c2     28*/
      if(ih.gt.0)nf=nint(wgm(im+1)-wgm(im-2))                                   !/*pum4c2     29*/
      rr=wgm(im+3)                                                              !/*pum4c2     30*/
!                                                                               !/*pum4c2     31*/
!        loop over fine mesh to get the mesh coordinate, wr.                    !/*pum4c2     32*/
      do 600 jf=1,nf                                                            !/*pum4c2     33*/
      if(jf.eq.nf)then                                                          !/*pum4c2     34*/
         wr=wgm(im+2)                                                           !/*pum4c2     35*/
         go to 20                                                               !/*pum4c2     36*/
      endif                                                                     !/*pum4c2     37*/
      if(jf.gt.1)then                                                           !/*pum4c2     38*/
         dx=dx*rr                                                               !/*pum4c2     39*/
         wr=wr+dx                                                               !/*pum4c2     40*/
         go to 20                                                               !/*pum4c2     41*/
      endif                                                                     !/*pum4c2     42*/
      dp=wgm(im+2)-wgm(im-1)                                                    !/*pum4c2     43*/
      if(abs(1.-rr).lt..0001)then                                               !/*pum4c2     44*/
         rr=1.                                                                  !/*pum4c2     45*/
         dx=dp/nf                                                               !/*pum4c2     46*/
         wr=wgm(im-1)+dx                                                        !/*pum4c2     47*/
         go to 20                                                               !/*pum4c2     48*/
      elseif(rr.lt.1.)then                                                      !/*pum4c2     49*/
         dx=dp*(1.-rr)/(1.-rr**nf)                                              !/*pum4c2     50*/
         wr=wgm(im-1)+dx                                                        !/*pum4c2     51*/
         go to 20                                                               !/*pum4c2     52*/
      elseif(rr.gt.1.)then                                                      !/*pum4c2     53*/
         rr=1./rr                                                               !/*pum4c2     54*/
         dx=-dp*(1.-rr)/(1.-rr**nf)                                             !/*pum4c2     55*/
         wr=wgm(im+2)+dx                                                        !/*pum4c2     56*/
         go to 20                                                               !/*pum4c2     57*/
      endif                                                                     !/*pum4c2     58*/
   20 tpp(1)=-1.                                                                !/*pum4c2     59*/
      if(nwgeom.ne.1)go to 200                                                  !/*pum4c2     60*/
!                                                                               !/*pum4c2     61*/
!**********************  rectangular geometry  *************************        !/*pum4c2     62*/
!                                                                      *        !/*pum4c2     63*/
!        set up bounding surface descriptions first time through.               !/*pum4c2     64*/
      if(kx.gt.0)go to 80                                                       !/*pum4c2     65*/
!                                                                               !/*pum4c2     66*/
!        get the amatrix, aa, describing each bounding mesh surface             !/*pum4c2     67*/
!        in xyz geometry.  set pxx=plmx for use in regula.                      !/*pum4c2     68*/
      do 70 ib=1,2                                                              !/*pum4c2     69*/
      lb=0                                                                      !/*pum4c2     70*/
      do 60 k=1,3                                                               !/*pum4c2     71*/
      kb=k+3*ib-3                                                               !/*pum4c2     72*/
      do 30 i=1,4                                                               !/*pum4c2     73*/
      do 30 j=1,4                                                               !/*pum4c2     74*/
      aa(i,j)=0.                                                                !/*pum4c2     75*/
   30 pxx(i,j)=plmx(i,j)                                                        !/*pum4c2     76*/
      if(k.gt.1)lb=lb+3*(nint(wwm(k+5))+1)                                      !/*pum4c2     77*/
      l=(ib-1)*nint(wwm(k+6))                                                   !/*pum4c2     78*/
      aa(1,k+1)=.5                                                              !/*pum4c2     79*/
      aa(k+1,1)=.5                                                              !/*pum4c2     80*/
      aa(1,1)=-wgm(lwgm+lb+2+3*l)                                               !/*pum4c2     81*/
!                                                                               !/*pum4c2     82*/
!        get parametric form, co, of planar bounding surfaces.                  !/*pum4c2     83*/
!           s=co(1)+co(2)*p   t=co(4)+co(5)*p                                   !/*pum4c2     84*/
      call regula(js,aa,2)                                                      !/*pum4c2     85*/
      km(kb)=ncs(lncs+js)                                                       !/*pum4c2     86*/
      if(km(kb).eq.0)go to 60                                                   !/*pum4c2     87*/
      do 40 i=1,3                                                               !/*pum4c2     88*/
      do 40 j=1,3                                                               !/*pum4c2     89*/
   40 qq(i,j,kb)=qmx(lqmx+i,j,1,js)                                             !/*pum4c2     90*/
      do 50 i=1,6                                                               !/*pum4c2     91*/
   50 co(i,kb)=coe(lcoe+i,1,js)                                                 !/*pum4c2     92*/
   60 continue                                                                  !/*pum4c2     93*/
   70 continue                                                                  !/*pum4c2     94*/
      kx=1                                                                      !/*pum4c2     95*/
!                                                                               !/*pum4c2     96*/
!        get amatrix, aa, (xyz coordinate system) of plot curve.                !/*pum4c2     97*/
   80 do 90 i=1,4                                                               !/*pum4c2     98*/
      do 90 j=1,4                                                               !/*pum4c2     99*/
   90 aa(i,j)=0.                                                                !/*pum4c2    100*/
      aa(1,1)=-wr                                                               !/*pum4c2    101*/
      aa(1,kk+1)=.5                                                             !/*pum4c2    102*/
      aa(kk+1,1)=.5                                                             !/*pum4c2    103*/
!                                                                               !/*pum4c2    104*/
!        get qmatrix, qq(i,j,7), (s,t coordinate system) and                    !/*pum4c2    105*/
!        co(i,7) = parametric form of plot curve.                               !/*pum4c2    106*/
      call regula(js,aa,2)                                                      !/*pum4c2    107*/
      if(ncs(lncs+js).eq.0)go to 600                                            !/*pum4c2    108*/
      do 100 i=1,3                                                              !/*pum4c2    109*/
      do 100 j=1,3                                                              !/*pum4c2    110*/
  100 qq(i,j,7)=qmx(lqmx+i,j,1,js)                                              !/*pum4c2    111*/
      do 110 i=1,6                                                              !/*pum4c2    112*/
  110 co(i,7)=coe(lcoe+i,1,js)                                                  !/*pum4c2    113*/
!                                                                               !/*pum4c2    114*/
!        loop over bounding planar surfaces.                                    !/*pum4c2    115*/
      do 130 kb=1,6                                                             !/*pum4c2    116*/
      if(kb.eq.kk.or.kb.eq.kk+3.or.km(kb).eq.0)go to 130                        !/*pum4c2    117*/
      b=co(5,kb)*co(2,7)-co(2,kb)*co(5,7)                                       !/*pum4c2    118*/
      if(b.eq.0.)go to 130                                                      !/*pum4c2    119*/
      p=(co(5,kb)*(co(1,kb)-co(1,7))+co(2,kb)*(co(4,7)-co(4,kb)))/b             !/*pum4c2    120*/
!                                                                               !/*pum4c2    121*/
!        if skew plot, see if point is in remaining dimension.                  !/*pum4c2    122*/
      k1=9-kk-kb                                                                !/*pum4c2    123*/
      if(km(k1).eq.0)go to 120                                                  !/*pum4c2    124*/
      b=co(5,k1)*co(2,7)-co(2,k1)*co(5,7)                                       !/*pum4c2    125*/
      if(b.eq.0.)go to 120                                                      !/*pum4c2    126*/
      p1=(co(5,k1)*(co(1,k1)-co(1,7))+co(2,k1)*(co(4,7)-co(4,k1)))/b            !/*pum4c2    127*/
      k2=k1+3                                                                   !/*pum4c2    128*/
      if(k1.gt.3)k2=k1-3                                                        !/*pum4c2    129*/
      b=co(5,k2)*co(2,7)-co(2,k2)*co(5,7)                                       !/*pum4c2    130*/
      if(b.eq.0.)go to 120                                                      !/*pum4c2    131*/
      p2=(co(5,k2)*(co(1,k2)-co(1,7))+co(2,k2)*(co(4,7)-co(4,k2)))/b            !/*pum4c2    132*/
      if((p-p1)*(p-p2).gt.0.)go to 130                                          !/*pum4c2    133*/
!                                                                               !/*pum4c2    134*/
!        add intersection to plot arc.                                          !/*pum4c2    135*/
  120 call pltadd(p,1,js,ss,tt,ih,la,lp,nc)                                     !/*pum4c2    136*/
  130 continue                                                                  !/*pum4c2    137*/
      go to 600                                                                 !/*pum4c2    138*/
!                                                                               !/*pum4c2    139*/
!**********************  cylindrical geometry  *************************        !/*pum4c2    140*/
!                                                                      *        !/*pum4c2    141*/
!        set up bounding surface descriptions first time through.               !/*pum4c2    142*/
  200 if(kx.gt.0)go to 300                                                      !/*pum4c2    143*/
!                                                                               !/*pum4c2    144*/
!        get pp matrix describing mesh cylindrical coordinate system.           !/*pum4c2    145*/
!        pv is the inverse of pp matrix and is needed to get from the           !/*pum4c2    146*/
!        cylindrical mesh rzt system to the xyz system.                         !/*pum4c2    147*/
!           pp=   1.0    0.0     0.0     0.0                                    !/*pum4c2    148*/
!                 wwm(4) wwm(20) wwm(16) -wwm(24)                               !/*pum4c2    149*/
!                 wwm(5) wwm(21) wwm(17) -wwm(25)                               !/*pum4c2    150*/
!                 wwm(6) wwm(22) wwm(18) -wwm(26)                               !/*pum4c2    151*/
      do 210 i=1,3                                                              !/*pum4c2    152*/
      pv(1,1+i)=0.                                                              !/*pum4c2    153*/
      pv(2,1+i)=wwm(19+i)                                                       !/*pum4c2    154*/
      pv(3,1+i)=wwm(15+i)                                                       !/*pum4c2    155*/
  210 pv(4,1+i)=-wwm(23+i)                                                      !/*pum4c2    156*/
      pv(1,1)=1.                                                                !/*pum4c2    157*/
      pv(2,1)=-dotpro(wwm(4),wwm(20))                                           !/*pum4c2    158*/
      pv(3,1)=-dotpro(wwm(4),wwm(16))                                           !/*pum4c2    159*/
      pv(4,1)=dotpro(wwm(4),wwm(24))                                            !/*pum4c2    160*/
!                                                                               !/*pum4c2    161*/
!        get the amatrix, aa, describing each bounding mesh surface             !/*pum4c2    162*/
!        in xyz geometry.  set pxx=plmx for use in regula.                      !/*pum4c2    163*/
      do 260 kb=1,3                                                             !/*pum4c2    164*/
      do 220 i=1,4                                                              !/*pum4c2    165*/
      do 220 j=1,4                                                              !/*pum4c2    166*/
      cc(i,j)=0.                                                                !/*pum4c2    167*/
  220 pxx(i,j)=plmx(i,j)                                                        !/*pum4c2    168*/
      if(kb.eq.1)cc(1,1)=-wwm(19)**2                                            !/*pum4c2    169*/
      if(kb.eq.1)cc(3,3)=1.                                                     !/*pum4c2    170*/
      if(kb.eq.1)cc(4,4)=1.                                                     !/*pum4c2    171*/
      if(kb.gt.1)cc(1,2)=.5                                                     !/*pum4c2    172*/
      if(kb.gt.1)cc(2,1)=.5                                                     !/*pum4c2    173*/
      if(kb.eq.3)cc(1,1)=-wwm(23)                                               !/*pum4c2    174*/
      call matmpy(4,4,4,cc,4,pv,4,bb,4,0)                                       !/*pum4c2    175*/
      call matmpy(4,4,4,pv,4,bb,4,aa,4,1)                                       !/*pum4c2    176*/
!                                                                               !/*pum4c2    177*/
!        get parametric form, co, of the kx boundary surfaces.                  !/*pum4c2    178*/
!           km=1 plane:    s=co(1)+co(2)*p                                      !/*pum4c2    179*/
!                          t=co(4)+co(5)*p                                      !/*pum4c2    180*/
!           km=3 ellipse:  s=c(1)+c(2)*sin(p)+c(3)*cos(p)                       !/*pum4c2    181*/
!                          t=c(4)+c(5)*sin(p)+c(6)*cos(p)                       !/*pum4c2    182*/
!        kx = number of boundary surfaces.                                      !/*pum4c2    183*/
!           kx=1  bounding cylinder normal to plot plane;                       !/*pum4c2    184*/
!           kx=3  mesh cylinder is ellipse in plot plane;                       !/*pum4c2    185*/
!           kx=4  mesh cylinder is 2 parallel lines in plot frame.              !/*pum4c2    186*/
      call regula(js,aa,2)                                                      !/*pum4c2    187*/
      if(kb.eq.1.and.ncs(lncs+js).eq.0)go to 900                                !/*pum4c2    188*/
      if(kb.eq.1)kx=ncs(lncs+js)+2                                              !/*pum4c2    189*/
      if(ncs(lncs+js).eq.0)kx=1                                                 !/*pum4c2    190*/
      ka=kb                                                                     !/*pum4c2    191*/
      do 250 mb=1,2                                                             !/*pum4c2    192*/
      if(mb.eq.2)ka=4                                                           !/*pum4c2    193*/
      do 230 i=1,3                                                              !/*pum4c2    194*/
      do 230 j=1,3                                                              !/*pum4c2    195*/
  230 qq(i,j,ka)=qmx(lqmx+i,j,mb,js)                                            !/*pum4c2    196*/
      do 240 i=1,6                                                              !/*pum4c2    197*/
  240 co(i,ka)=coe(lcoe+i,mb,js)                                                !/*pum4c2    198*/
      km(ka)=kfm(lkfm+js)                                                       !/*pum4c2    199*/
  250 continue                                                                  !/*pum4c2    200*/
  260 continue                                                                  !/*pum4c2    201*/
!                                                                               !/*pum4c2    202*/
!        get the amatrix (xyz) for the mesh curve to be plotted,                !/*pum4c2    203*/
!        namely mesh surface kk at wr.                                          !/*pum4c2    204*/
  300 do 310 i=1,4                                                              !/*pum4c2    205*/
      do 310 j=1,4                                                              !/*pum4c2    206*/
  310 cc(i,j)=0.                                                                !/*pum4c2    207*/
      if(kk.eq.1)then                                                           !/*pum4c2    208*/
         if(ih.eq.0)go to 610                                                   !/*pum4c2    209*/
         cc(1,1)=-wr**2                                                         !/*pum4c2    210*/
         cc(3,3)=1.                                                             !/*pum4c2    211*/
         cc(4,4)=1.                                                             !/*pum4c2    212*/
      elseif(kk.eq.2)then                                                       !/*pum4c2    213*/
         if(kx.eq.1)go to 620                                                   !/*pum4c2    214*/
         cc(1,1)=-wr                                                            !/*pum4c2    215*/
         cc(1,2)=.5                                                             !/*pum4c2    216*/
         cc(2,1)=.5                                                             !/*pum4c2    217*/
      elseif(kk.eq.3)then                                                       !/*pum4c2    218*/
         if(km(1).eq.1)go to 620                                                !/*pum4c2    219*/
         if(ih.eq.0)go to 610                                                   !/*pum4c2    220*/
         cc(1,3)=-sin(wr)                                                       !/*pumc        1*/
         cc(3,1)=cc(1,3)                                                        !/*pum4c2    222*/
         cc(1,4)=cos(wr)                                                        !/*pum4c2    223*/
         cc(4,1)=cc(1,4)                                                        !/*pum4c2    224*/
      endif                                                                     !/*pum4c2    225*/
      call matmpy(4,4,4,cc,4,pv,4,bb,4,0)                                       !/*pum4c2    226*/
      call matmpy(4,4,4,pv,4,bb,4,aa,4,1)                                       !/*pum4c2    227*/
!                                                                               !/*pum4c2    228*/
!        set up curve to be plotted:                                            !/*pum4c2    229*/
!           qq(i,j,7) = s,t coordinate system qmatrix                           !/*pum4c2    230*/
!           co(i,7) = parametric form                                           !/*pum4c2    231*/
!           km(7) = 1/3 = line/ellipse = parametric surface type.               !/*pum4c2    232*/
      call regula(js,aa,2)                                                      !/*pum4c2    233*/
      if(ncs(lncs+js).eq.0)go to 600                                            !/*pum4c2    234*/
      do 320 i=1,3                                                              !/*pum4c2    235*/
      do 320 j=1,3                                                              !/*pum4c2    236*/
  320 qq(i,j,7)=qmx(lqmx+i,j,1,js)                                              !/*pum4c2    237*/
      do 330 i=1,6                                                              !/*pum4c2    238*/
  330 co(i,7)=coe(lcoe+i,1,js)                                                  !/*pum4c2    239*/
      km(7)=kfm(lkfm+js)                                                        !/*pum4c2    240*/
!                                                                               !/*pum4c2    241*/
!        plot curve is cylinder with 2 parallel lines.                          !/*pum4c2    242*/
      if(kk.gt.1)go to 420                                                      !/*pum4c2    243*/
      if(ncs(lncs+js).ne.2)go to 380                                            !/*pum4c2    244*/
      do 370 iq=1,2                                                             !/*pum4c2    245*/
      tpp(1)=-1.                                                                !/*pum4c2    246*/
      do 360 kb=2,3                                                             !/*pum4c2    247*/
      b=co(5,kb)*coe(lcoe+2,iq,js)-co(2,kb)*coe(lcoe+5,iq,js)                   !/*pum4c2    248*/
      if(b.eq.0.)go to 360                                                      !/*pum4c2    249*/
      p=(co(5,kb)*(co(1,kb)-coe(lcoe+1,iq,js))+                                 !/*pum4c2    250*/
     & co(2,kb)*(coe(lcoe+4,iq,js)-co(4,kb)))/b                                 !/*pum4c2    251*/
      call pltadd(p,iq,js,ss,tt,ih,la,lp,nc)                                    !/*pum4c2    252*/
  360 continue                                                                  !/*pum4c2    253*/
  370 continue                                                                  !/*pum4c2    254*/
      go to 600                                                                 !/*pum4c2    255*/
!                                                                               !/*pum4c2    256*/
!        if the plot curve, km(7)=3, is an ellipse, then                        !/*pum4c2    257*/
!        set up 3, 0, 3-2*pie, and where it crosses the window boundary.        !/*pum4c2    258*/
!        if nxp = 0 then ellipse is totally outside plot frame.                 !/*pum4c2    259*/
  380 nxp=0                                                                     !/*pum4c2    260*/
      call ptost(3.+zero,1,js,ss,tt)                                            !/*pum4c2    261*/
      if(abs(ss).gt.1..or.abs(tt).gt.1.)go to 390                               !/*pum4c2    262*/
      nxp=2                                                                     !/*pum4c2    263*/
      crs(lcrs+1)=3.-2.*pie                                                     !/*pum4c2    264*/
      crs(lcrs+2)=3.                                                            !/*pum4c2    265*/
      call ptost(zero,1,js,ss,tt)                                               !/*pum4c2    266*/
      if(abs(ss).gt.1..or.abs(tt).gt.1.)go to 390                               !/*pum4c2    267*/
      nxp=3                                                                     !/*pum4c2    268*/
      crs(lcrs+3)=0.                                                            !/*pum4c2    269*/
  390 do 410 i=1,2                                                              !/*pum4c2    270*/
      do 410 m=-1,1,2                                                           !/*pum4c2    271*/
      call sttop(i,zero+m,1,js,n,qg(1),qg(2),qp(1),qp(2))                       !/*pum4c2    272*/
      do 400 j=1,n                                                              !/*pum4c2    273*/
      if(abs(qg(j)).gt.1.)go to 400                                             !/*pum4c2    274*/
      nxp=nxp+1                                                                 !/*pum4c2    275*/
      crs(lcrs+nxp)=qp(j)                                                       !/*pum4c2    276*/
  400 continue                                                                  !/*pum4c2    277*/
  410 continue                                                                  !/*pum4c2    278*/
      if(nxp.eq.0)go to 600                                                     !/*pum4c2    279*/
!                                                                               !/*pum4c2    280*/
!        loop over bounding surfaces.                                           !/*pum4c2    281*/
  420 do 510 kb=1,kx                                                            !/*pum4c2    282*/
      if(kb.eq.kk)go to 510                                                     !/*pum4c2    283*/
!                                                                               !/*pum4c2    284*/
!        do the case of two intersecting straight lines.                        !/*pum4c2    285*/
      if(max(km(7),km(kb)).gt.1)go to 430                                       !/*pum4c2    286*/
      b=co(5,kb)*co(2,7)-co(2,kb)*co(5,7)                                       !/*pum4c2    287*/
      if(b.eq.0.)go to 510                                                      !/*pum4c2    288*/
      p=(co(5,kb)*(co(1,kb)-co(1,7))+co(2,kb)*(co(4,7)-co(4,kb)))/b             !/*pum4c2    289*/
      call pltadd(p,1,js,ss,tt,ih,la,lp,nc)                                     !/*pum4c2    290*/
      go to 510                                                                 !/*pum4c2    291*/
!                                                                               !/*pum4c2    292*/
!        get intersection of line m and quadratic j.                            !/*pum4c2    293*/
  430 j=kb                                                                      !/*pum4c2    294*/
      if(kk.eq.1)j=7                                                            !/*pum4c2    295*/
      m=kb+7-j                                                                  !/*pum4c2    296*/
      cf(1)=qq(2,2,j)*co(2,m)**2+2.*qq(2,3,j)*co(2,m)*co(5,m)+                  !/*pum4c2    297*/
     & qq(3,3,j)*co(5,m)**2                                                     !/*pum4c2    298*/
      cf(2)=2.*(qq(2,2,j)*co(1,m)*co(2,m)+qq(2,3,j)*(co(4,m)*co(2,m)+           !/*pum4c2    299*/
     & co(1,m)*co(5,m))+qq(3,3,j)*co(4,m)*co(5,m)+qq(1,2,j)*co(2,m)+            !/*pum4c2    300*/
     & qq(1,3,j)*co(5,m))                                                       !/*pum4c2    301*/
      cf(3)=qq(2,2,j)*co(1,m)**2+2.*qq(2,3,j)*co(1,m)*co(4,m)+                  !/*pum4c2    302*/
     & qq(3,3,j)*co(4,m)**2+2.*qq(1,2,j)*co(1,m)+2.*qq(1,3,j)*co(4,m)+          !/*pum4c2    303*/
     & qq(1,1,j)                                                                !/*pum4c2    304*/
      call quad(cf,n,qr(1),qr(2))                                               !/*pum4c2    305*/
!                                                                               !/*pum4c2    306*/
!        if plot surface is a plane, draw line.                                 !/*pum4c2    307*/
      if(kk.eq.1)go to 470                                                      !/*pum4c2    308*/
      if(n.ne.2)go to 600                                                       !/*pum4c2    309*/
      tpp(1)=-1.                                                                !/*pum4c2    310*/
!        if plot surface is theta plane in cylindrical geometry:                !/*pum4c2    311*/
      if(kk.ne.3)go to 450                                                      !/*pum4c2    312*/
      qr(2)=max(qr(1),qr(2))                                                    !/*pum4c2    313*/
      qr(1)=.5*(qr(1)+qr(2))                                                    !/*pum4c2    314*/
      if(kx.eq.1)go to 450                                                      !/*pum4c2    315*/
      do 440 i=1,2                                                              !/*pum4c2    316*/
      qp(i)=qr(i)                                                               !/*pum4c2    317*/
      b=co(5,i+1)*co(2,7)-co(2,i+1)*co(5,7)                                     !/*pum4c2    318*/
  440 if(b.ne.0.)qp(i)=(co(5,i+1)*(co(1,i+1)-co(1,7))+                          !/*pum4c2    319*/
     & co(2,i+1)*(co(4,7)-co(4,i+1)))/b                                         !/*pum4c2    320*/
      qr(1)=max(qr(1),min(qp(1),qp(2)))                                         !/*pum4c2    321*/
      qr(2)=min(qr(2),max(qp(1),qp(2)))                                         !/*pum4c2    322*/
      if(qr(1).ge.qr(2))go to 600                                               !/*pum4c2    323*/
!        for all planes intersecting a cylinder (ellipse):                      !/*pum4c2    324*/
  450 do 460 i=1,n                                                              !/*pum4c2    325*/
  460 call pltadd(qr(i),1,js,ss,tt,ih,la,lp,nc)                                 !/*pum4c2    326*/
      go to 600                                                                 !/*pum4c2    327*/
!                                                                               !/*pum4c2    328*/
!        plot surface is cylinder.  store points in parametric form.            !/*pum4c2    329*/
  470 do 500 i=1,n                                                              !/*pum4c2    330*/
      s=co(1,m)+co(2,m)*qr(i)                                                   !/*pum4c2    331*/
      t=co(4,m)+co(5,m)*qr(i)                                                   !/*pum4c2    332*/
      if(abs(s).gt.1..or.abs(t).gt.1.)go to 500                                 !/*pum4c2    333*/
      call sttop(1,s,1,js,m1,qg(1),qg(2),qp(1),qp(2))                           !/*pum4c2    334*/
      call sttop(2,t,1,js,m2,qg(3),qg(4),qp(3),qp(4))                           !/*pum4c2    335*/
      do 480 j1=1,m1                                                            !/*pum4c2    336*/
      do 480 j2=1,m2                                                            !/*pum4c2    337*/
  480 if((s-qg(j2+2))**2+(t-qg(j1))**2.lt.res**2)go to 490                      !/*pum4c2    338*/
      go to 500                                                                 !/*pum4c2    339*/
  490 nxp=nxp+1                                                                 !/*pum4c2    340*/
      crs(lcrs+nxp)=.5*(qp(j1)+qp(j2+2))                                        !/*pum4c2    341*/
      call ptost(crs(lcrs+nxp),1,js,ss,tt)                                      !/*pum4c2    342*/
  500 continue                                                                  !/*pum4c2    343*/
  510 continue                                                                  !/*pum4c2    344*/
!                                                                               !/*pum4c2    345*/
!        sort the intersections and eliminate duplicates.                       !/*pum4c2    346*/
      if(kk.ne.1)go to 600                                                      !/*pum4c2    347*/
      np=nxp                                                                    !/*pum4c2    348*/
      do 550 i=1,nxp-1                                                          !/*pum4c2    349*/
      if(i.ge.np)go to 560                                                      !/*pum4c2    350*/
      do 540 j=i+1,nxp                                                          !/*pum4c2    351*/
  520 if(j.gt.np)go to 550                                                      !/*pum4c2    352*/
      if(abs(crs(lcrs+i)-crs(lcrs+j)).gt.1.e-4*(abs(crs(lcrs+i))+               !/*pum4c2    353*/
     & abs(crs(lcrs+j))))go to 530                                              !/*pum4c2    354*/
      crs(lcrs+j)=crs(lcrs+np)                                                  !/*pum4c2    355*/
      crs(lcrs+np)=crs(lcrs+i)                                                  !/*pum4c2    356*/
      np=np-1                                                                   !/*pum4c2    357*/
      go to 520                                                                 !/*pum4c2    358*/
  530 if(crs(lcrs+i).lt.crs(lcrs+j))go to 540                                   !/*pum4c2    359*/
      a=crs(lcrs+i)                                                             !/*pum4c2    360*/
      crs(lcrs+i)=crs(lcrs+j)                                                   !/*pum4c2    361*/
      crs(lcrs+j)=a                                                             !/*pum4c2    362*/
  540 continue                                                                  !/*pum4c2    363*/
  550 continue                                                                  !/*pum4c2    364*/
  560 nxp=np                                                                    !/*pum4c2    365*/
!                                                                               !/*pum4c2    366*/
!        check if arc midpoints are between cylinder end planes.                !/*pum4c2    367*/
      do 580 i=1,nxp-1                                                          !/*pum4c2    368*/
      np=1                                                                      !/*pum4c2    369*/
      p=.5*(crs(lcrs+i)+crs(lcrs+i+1))                                          !/*pum4c2    370*/
      call ptost(p,1,js,ss,tt)                                                  !/*pum4c2    371*/
      if(2.*qq(1,2,2)*ss+2.*qq(1,3,2)*tt+qq(1,1,2).lt.0..or.                    !/*pum4c2    372*/
     & 2.*qq(1,2,3)*ss+2.*qq(1,3,3)*tt+qq(1,1,3).gt.0.)np=0                     !/*pum4c2    373*/
      if(np.eq.0)tpp(1)=-1.                                                     !/*pum4c2    374*/
      if(np.eq.0)go to 580                                                      !/*pum4c2    375*/
      dp=crs(lcrs+i+1)-crs(lcrs+i)                                              !/*pum4c2    376*/
      p=crs(lcrs+i)                                                             !/*pum4c2    377*/
      call pltadd(p,1,js,ss,tt,ih,la,lp,nc)                                     !/*pum4c2    378*/
      a=max(qq(2,2,1),qq(3,3,1))                                                !/*pum4c2    379*/
      if(a.gt.0.)a=min(qq(2,2,1),qq(3,3,1))/a                                   !/*pum4c2    380*/
      b=180.*abs(dp)/pie                                                        !/*pum4c2    381*/
      np=max(1,int(a*b))                                                        !/*pum4c2    382*/
      dp=dp/np                                                                  !/*pum4c2    383*/
      do 570 j=1,np                                                             !/*pum4c2    384*/
      p=p+dp                                                                    !/*pum4c2    385*/
      call pltadd(p,1,js,ss,tt,ih,la,lp,nc)                                     !/*pum4c2    386*/
  570 continue                                                                  !/*pum4c2    387*/
  580 continue                                                                  !/*pum4c2    388*/
!                                                                               !/*pum4c2    389*/
  600 continue                                                                  !/*pum4c2    390*/
  610 continue                                                                  !/*pum4c2    391*/
  620 continue                                                                  !/*pum4c2    392*/
!                                                                               !/*pum4c2    393*/
!        restore surface type.                                                  !/*pum4c2    394*/
  900 kst(lkst+js)=ks                                                           !/*pum4c2    395*/
#endif                                                                          /*!pumx        8*/
      return                                                                    !/*pum4c2    396*/
      end                                                                       !/*pum4c2    397*/
