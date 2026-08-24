!#ident viewz.F from deck vw                                                    !/*vw          1*/
      subroutine viewz                                                          !/*vw          3*/
#ifdef PLOT                                                                     /*!vwx         1*/
!        compute the arcs and cell number locations to be plotted.              !/*vw          4*/
!        the unusual structure of this subroutine is due to the fact            !/*vw          5*/
!        that it is recursive.  it calls itself, in effect, in 2 places.        !/*vw          6*/
      use global_data                                                           !/*vwx         2*/
      use plot_data                                                             !/*vwx         3*/
!                                                                               !/*vwx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*vwx         5*/
      implicit integer(kindi) (i-n)                                             !/*vwx         6*/
!                                                                               !/*vwx         7*/
      dimension bb(4,4),jn(6),st(3,2)                                           !/*vwx         8*/
      c(i)=coe(lcoe+i,1,jsu)                                                    !/*vw         11*/
!                                                                               !/*vw         12*/
!        define an arbitrary direction for possible use in angl.                !/*vw         13*/
      uuu=.6218                                                                 !/*vw         14*/
      vvv=.5739                                                                 !/*vw         15*/
      www=sqrt(1.-uuu**2-vvv**2)                                                !/*vw         16*/
      nc=0                                                                      !/*vw         17*/
      lp=lzst+12                                                                !/*vw         18*/
      ju=0                                                                      !/*vw         19*/
      lc=0                                                                      !/*vw         20*/
      if(junf.eq.0)go to 20                                                     !/*vw         21*/
      do 10 i=1,mxa                                                             !/*vw         22*/
      if(jun(ljun+i).eq.0.and.lat(llat+1,i).ne.0)lc=i                           !/*vw         23*/
   10 if(jun(ljun+i).eq.0)go to 20                                              !/*vw         24*/
   20 lev=0                                                                     !/*vw         25*/
      if(mod(meshpl,2).eq.0)go to 420                                           !/*vw4c2       1*/
!                                                                               !/*vw         26*/
!        make pxx be plmx transformed for all levels from zero to lev-1.        !/*vw         27*/
   30 do 40 i=1,4                                                               !/*vw         28*/
      do 40 j=1,4                                                               !/*vw         29*/
   40 pxx(i,j)=plmx(i,j)                                                        !/*vw         30*/
      m=0                                                                       !/*vw         31*/
      do 90 l=0,lev                                                             !/*vw         32*/
      if(l.eq.0)go to 50                                                        !/*vw         33*/
      j=-mfl(lmfl+1,int(udt(7,l-1)))                                            !/*vw         34*/
      if(j.lt.0)m=mfl(lmfl+3,int(udt(7,l-1)))                                   !/*vw         35*/
      if(j.gt.0)m=laf(j+3,int(3+udt(8,l-1)-laf(j+1,1)+laf(j+1,2)*               !/*vw         36*/
     & (udt(9,l-1)-laf(j+2,1)+laf(j+2,2)*(udt(10,l-1)-laf(j+3,1)))))            !/*vw         37*/
   50 do 60 i=1,12                                                              !/*vw         38*/
   60 tpp(i)=trf(ltrf+i+1,m)                                                    !/*vw         39*/
      if(l.eq.lev)go to 80                                                      !/*vw         40*/
      if(lat(llat+1,int(udt(7,l))).eq.0)go to 80                                !/*vw         41*/
      k=lat(llat+2,int(udt(7,l)))                                               !/*vw         42*/
      do 70 i=1,3                                                               !/*vw         43*/
   70 tpp(i)=tpp(i)-udt(8,l)*vcl(lvcl+i,1,k)-                                   !/*vw         44*/
     & udt(9,l)*vcl(lvcl+i,2,k)-udt(10,l)*vcl(lvcl+i,3,k)                       !/*vw         45*/
   80 call matmpy(3,4,4,tpp,3,pxx,4,bb,4,0)                                     !/*vw         46*/
      do 90 i=1,3                                                               !/*vw         47*/
      do 90 j=1,4                                                               !/*vw         48*/
   90 pxx(i+1,j)=bb(i,j)                                                        !/*vw         49*/
!                                                                               !/*vw         50*/
      do 100 j=1,mxj                                                            !/*vw         51*/
  100 ncs(lncs+j)=-1                                                            !/*vw         52*/
      if(lc.ne.0)go to 190                                                      !/*vw         53*/
!                                                                               !/*vw         54*/
!        do all the surfaces in the current non-lattice universe.               !/*vw         55*/
      do 150 jsu=1,mxj                                                          !/*vw         56*/
      if(ncs(lncs+jsu).eq.0)go to 150                                           !/*vw         57*/
!                                                                               !/*vw         58*/
!        make a list of the cells bounded by surface jsu.                       !/*vw         59*/
      ncel=0                                                                    !/*vw         60*/
      do 140 i=1,mxa                                                            !/*vw         61*/
      if(junf.eq.0)go to 110                                                    !/*vw         62*/
      if(abs(jun(ljun+i)).ne.ju)go to 140                                       !/*vw         63*/
  110 do 120 j=abs(lca(llca+i)),abs(lca(llca+i+1))-1                            !/*vw         64*/
  120 if(abs(lja(llja+j)).eq.jsu)go to 130                                      !/*vw         65*/
      go to 140                                                                 !/*vw         66*/
  130 ncel=ncel+1                                                               !/*vw         67*/
      lcl(llcl+ncel)=i                                                          !/*vw         68*/
  140 continue                                                                  !/*vw         69*/
      if(ncel.eq.0)go to 150                                                    !/*vw         70*/
!                                                                               !/*vw         71*/
!        put points that define the arcs of surface jsu into zst.               !/*vw         72*/
      if(ncs(lncs+jsu).lt.0)call regula(jsu,amx(lamx+1,1,jsu),0)                !/*vw         73*/
      call pltsrf(nc,lp,zero,zero,0)                                            !/*vw         74*/
  150 continue                                                                  !/*vw         75*/
!                                                                               !/*vw         76*/
!        do cells in the current universe that are filled.                      !/*vw         77*/
      if(junf.eq.0)go to 420                                                    !/*vw         78*/
      kc=0                                                                      !/*vw         79*/
  160 kc=kc+1                                                                   !/*vw         80*/
      if(abs(jun(ljun+kc)).ne.ju.or.mfl(lmfl+1,kc).eq.0)go to 180               !/*vw         81*/
      udt(7,lev)=kc                                                             !/*vw         82*/
      ju=mfl(lmfl+1,kc)                                                         !/*vw         83*/
      lc=mfl(lmfl+2,kc)                                                         !/*vw         84*/
      lev=lev+1                                                                 !/*vw         85*/
      go to 30                                                                  !/*vw         86*/
  170 lev=lev-1                                                                 !/*vw         87*/
      kc=udt(7,lev)                                                             !/*vw         88*/
      ju=abs(jun(ljun+kc))                                                      !/*vw         89*/
  180 if(kc.lt.mxa)go to 160                                                    !/*vw         90*/
      go to 410                                                                 !/*vw         91*/
!                                                                               !/*vw         92*/
!        do the surfaces in the current lattice universe.                       !/*vw         93*/
!        hexahedron and base of hexagonal prism.                                !/*vw         94*/
  190 ncel=0                                                                    !/*vw         95*/
      lcl(llcl+1)=lc                                                            !/*vw         96*/
      do 200 i=1,6                                                              !/*vw         97*/
  200 udt(i,lev)=0.                                                             !/*vw         98*/
      do 230 ks=3*lat(llat+1,lc)-2,(abs(lca(llca+lc+1))-lca(llca+lc))/2         !/*vw         99*/
      jsu=abs(lja(llja+lca(llca+lc)+2*ks-1))                                    !/*vw        100*/
      j2=abs(lja(llja+lca(llca+lc)+2*ks-2))                                     !/*vw        101*/
      ku=min(3,ks)                                                              !/*vw        102*/
      call regula(jsu,amx(lamx+1,1,jsu),1)                                      !/*vw        103*/
      if(ncs(lncs+jsu).ne.0)go to 210                                           !/*vw        104*/
      xxx=pxx(2,1)                                                              !/*vw        105*/
      yyy=pxx(3,1)                                                              !/*vw        106*/
      zzz=pxx(4,1)                                                              !/*vw        107*/
      call findel(lc)                                                           !/*vw        108*/
      udt(2*ku-1,lev)=ijk(ku)                                                   !/*vwx         9*/
      udt(2*ku,lev)=udt(2*ku-1,lev)                                             !/*vw        110*/
      go to 230                                                                 !/*vw        111*/
  210 call regula(j2,amx(lamx+1,1,j2),1)                                        !/*vw        112*/
      d1=coe(lcoe+1,1,j2)-c(1)                                                  !/*vw        113*/
      d2=coe(lcoe+4,1,j2)-c(4)                                                  !/*vw        114*/
      a=(abs(d1)+abs(d2))/(d1**2+d2**2)                                         !/*vw        115*/
      ii=nint(a+(c(1)*d1+c(4)*d2)/(d1**2+d2**2)+.5)                             !/*vw4b        1*/
      nn=nint(2.*a+.5)                                                          !/*vw4b        2*/
      coe(lcoe+1,1,jsu)=c(1)-ii*d1                                              !/*vw        118*/
      coe(lcoe+4,1,jsu)=c(4)-ii*d2                                              !/*vw        119*/
      udt(2*ku-1,lev)=huge                                                      !/*vw        120*/
      udt(2*ku,lev)=-huge                                                       !/*vw        121*/
      do 220 j=1,nn                                                             !/*vw        122*/
      coe(lcoe+1,1,jsu)=c(1)+d1                                                 !/*vw        123*/
      coe(lcoe+4,1,jsu)=c(4)+d2                                                 !/*vw        124*/
      n=nc                                                                      !/*vw        125*/
      call pltsrf(nc,lp,zero,zero,0)                                            !/*vw        126*/
      if(n.eq.nc)go to 220                                                      !/*vw        127*/
      udt(2*ku-1,lev)=min(udt(2*ku-1,lev),j-ii-one)                             !/*vw        128*/
      udt(2*ku,lev)=j-ii                                                        !/*vw        129*/
  220 continue                                                                  !/*vw        130*/
      coe(lcoe+1,1,jsu)=coe(lcoe+1,1,j2)-d1                                     !/*vw4b        3*/
      coe(lcoe+4,1,jsu)=coe(lcoe+4,1,j2)-d2                                     !/*vw4b        4*/
      if(udt(2*ku,lev).gt.udt(2*ku-1,lev))go to 230                             !/*vw        131*/
      udt(2*ku-1,lev)=-ii                                                       !/*vw        132*/
      udt(2*ku,lev)=nn-ii-1                                                     !/*vw        133*/
  230 continue                                                                  !/*vw        134*/
!                                                                               !/*vw        135*/
!        sides of hexagonal prism lattice.                                      !/*vw        136*/
      if(lat(llat+1,lc).eq.1)go to 350                                          !/*vw        137*/
      ncel=1                                                                    !/*vw        138*/
      do 240 k=1,6                                                              !/*vw        139*/
      jn(k)=abs(lja(llja+lca(llca+lc)+k-1))                                     !/*vw        140*/
      call regula(jn(k),amx(lamx+1,1,jn(k)),1)                                  !/*vw        141*/
  240 if(ncs(lncs+jn(k)).eq.0)go to 350                                         !/*vw4c        1*/
      xxx=pxx(2,1)                                                              !/*vw        143*/
      yyy=pxx(3,1)                                                              !/*vw        144*/
      zzz=pxx(4,1)                                                              !/*vw        145*/
      call findel(lc)                                                           !/*vw        146*/
      do 250 i=1,3                                                              !/*vw        147*/
      j=jn(mod(-2+4*i,7))                                                       !/*vw        148*/
      k=jn(mod(4*i,7))                                                          !/*vw        149*/
      a=coe(lcoe+5,1,j)*coe(lcoe+2,1,k)-coe(lcoe+2,1,j)*coe(lcoe+5,1,k)         !/*vw        150*/
      if(a.eq.0.)go to 350                                                      !/*vw4c        2*/
      a=(coe(lcoe+5,1,j)*(coe(lcoe+1,1,j)-coe(lcoe+1,1,k))+                     !/*vw        152*/
     & coe(lcoe+2,1,j)*(coe(lcoe+4,1,k)-coe(lcoe+4,1,j)))/a                     !/*vw        153*/
      st(i,1)=coe(lcoe+1,1,k)+coe(lcoe+2,1,k)*a                                 !/*vw        154*/
  250 st(i,2)=coe(lcoe+4,1,k)+coe(lcoe+5,1,k)*a                                 !/*vw        155*/
      d1=st(2,1)-st(1,1)                                                        !/*vw        156*/
      d2=st(2,2)-st(1,2)                                                        !/*vw        157*/
      e1=st(3,1)-st(1,1)                                                        !/*vw        158*/
      e2=st(3,2)-st(1,2)                                                        !/*vw        159*/
      a=(d1*e1+d2*e2)/(d1**2+d2**2)                                             !/*vw        160*/
      f1=e1-d1*a                                                                !/*vw        161*/
      f2=e2-d2*a                                                                !/*vw        162*/
      a=(e1*d1+e2*d2)/(e1**2+e2**2)                                             !/*vw        163*/
      g1=d1-e1*a                                                                !/*vw        164*/
      g2=d2-e2*a                                                                !/*vw        165*/
      u=huge                                                                    !/*vw        166*/
      v=-huge                                                                   !/*vw        167*/
      do 260 i=-1,1,2                                                           !/*vw        168*/
      do 260 j=-1,1,2                                                           !/*vw        169*/
      a=(i-st(1,1))*g1+(j-st(1,2))*g2                                           !/*vw        170*/
      u=min(u,a)                                                                !/*vw        171*/
  260 v=max(v,a)                                                                !/*vw        172*/
      a=1./(g1**2+g2**2)                                                        !/*vw        173*/
      mm=(v-u)*a+3.                                                             !/*vw        174*/
      ii=-u*a+2.                                                                !/*vw        175*/
      u=huge                                                                    !/*vw        176*/
      v=-huge                                                                   !/*vw        177*/
      do 270 i=-1,1,2                                                           !/*vw        178*/
      do 270 j=-1,1,2                                                           !/*vw        179*/
      a=(i-st(1,1))*f1+(j-st(1,2))*f2                                           !/*vw        180*/
      u=min(u,a)                                                                !/*vw        181*/
  270 v=max(v,a)                                                                !/*vw        182*/
      a=1./(f1**2+f2**2)                                                        !/*vw        183*/
      nn=(v-u)*a+3.                                                             !/*vw        184*/
      jj=-u*a+2.                                                                !/*vw        185*/
      do 280 i=1,6                                                              !/*vw        186*/
      coe(lcoe+1,1,jn(i))=coe(lcoe+1,1,jn(i))-ii*d1-jj*e1                       !/*vw        187*/
  280 coe(lcoe+4,1,jn(i))=coe(lcoe+4,1,jn(i))-ii*d2-jj*e2                       !/*vw        188*/
      udt(1,lev)=huge                                                           !/*vw        189*/
      udt(2,lev)=-huge                                                          !/*vw        190*/
      udt(3,lev)=huge                                                           !/*vw        191*/
      udt(4,lev)=-huge                                                          !/*vw        192*/
      lx=abs(lca(llca+lc+1))-lca(llca+lc)-6                                     !/*vw        193*/
      do 330 j=1,nn                                                             !/*vw        194*/
      do 290 k=1,6                                                              !/*vw        195*/
      coe(lcoe+1,1,jn(k))=coe(lcoe+1,1,jn(k))+e1                                !/*vw        196*/
  290 coe(lcoe+4,1,jn(k))=coe(lcoe+4,1,jn(k))+e2                                !/*vw        197*/
      do 310 i=1,mm                                                             !/*vw        198*/
      do 300 k=1,6                                                              !/*vw        199*/
      coe(lcoe+1,1,jn(k))=coe(lcoe+1,1,jn(k))+d1                                !/*vw        200*/
  300 coe(lcoe+4,1,jn(k))=coe(lcoe+4,1,jn(k))+d2                                !/*vw        201*/
      do 310 k=1,5,2                                                            !/*vw        202*/
      jsu=jn(k)                                                                 !/*vw        203*/
      n=nc                                                                      !/*vw        204*/
      call pltsrf(nc,lp,(i-ii)*d1+(j-jj)*e1,(i-ii)*d2+(j-jj)*e2,lx)             !/*vw        205*/
      if(n.eq.nc)go to 310                                                      !/*vw        206*/
      udt(1,lev)=min(udt(1,lev),i-ii-one)                                       !/*vw        207*/
      udt(2,lev)=max(udt(2,lev),i-ii-zero)                                      !/*vw        208*/
      udt(3,lev)=min(udt(3,lev),j-jj-one)                                       !/*vw        209*/
      udt(4,lev)=max(udt(4,lev),j-jj-zero)                                      !/*vw        210*/
  310 continue                                                                  !/*vw        211*/
      do 320 k=1,6                                                              !/*vw        212*/
      coe(lcoe+1,1,jn(k))=coe(lcoe+1,1,jn(k))-mm*d1                             !/*vw        213*/
  320 coe(lcoe+4,1,jn(k))=coe(lcoe+4,1,jn(k))-mm*d2                             !/*vw        214*/
  330 continue                                                                  !/*vw        215*/
      do 335 k=1,6                                                              !/*vw4b        5*/
      jn(k)=abs(lja(llja+lca(llca+lc)+k-1))                                     !/*vw4b        6*/
  335 call regula(jn(k),amx(lamx+1,1,jn(k)),1)                                  !/*vw4b        7*/
      if(udt(2,lev).gt.udt(1,lev))go to 340                                     !/*vw        216*/
      udt(1,lev)=-ii                                                            !/*vw        217*/
      udt(2,lev)=nn-ii-1                                                        !/*vw        218*/
  340 if(udt(4,lev).gt.udt(3,lev))go to 350                                     !/*vw        219*/
      udt(3,lev)=-jj                                                            !/*vw        220*/
      udt(4,lev)=mm-jj-1                                                        !/*vw        221*/
!                                                                               !/*vw        222*/
!        do elements in the current lattice that are filled.                    !/*vw        223*/
  350 j=-mfl(lmfl+1,lc)                                                         !/*vw        224*/
      if(j.eq.0)go to 410                                                       !/*vw        225*/
      if(j.lt.0)go to 360                                                       !/*vw        226*/
      udt(1,lev)=max(int(udt(1,lev)),laf(j+1,1))                                !/*vw        227*/
      udt(2,lev)=min(int(udt(2,lev)),laf(j+1,1)+laf(j+1,2)-1)                   !/*vw        228*/
      udt(3,lev)=max(int(udt(3,lev)),laf(j+2,1))                                !/*vw        229*/
      udt(4,lev)=min(int(udt(4,lev)),laf(j+2,1)+laf(j+2,2)-1)                   !/*vw        230*/
      udt(5,lev)=max(int(udt(5,lev)),laf(j+3,1))                                !/*vw        231*/
      udt(6,lev)=min(int(udt(6,lev)),laf(j+3,1)+laf(j+3,2)-1)                   !/*vw        232*/
  360 if(min(udt(2,lev)-udt(1,lev),udt(4,lev)-udt(3,lev),                       !/*vw        233*/
     & udt(6,lev)-udt(5,lev)).lt.0.)go to 410                                   !/*vw        234*/
      udt(7,lev)=lc                                                             !/*vw        235*/
      udt(10,lev)=udt(5,lev)-1.                                                 !/*vw        236*/
  370 udt(10,lev)=udt(10,lev)+1.                                                !/*vw        237*/
      udt(9,lev)=udt(3,lev)-1.                                                  !/*vw        238*/
  380 udt(9,lev)=udt(9,lev)+1.                                                  !/*vw        239*/
      udt(8,lev)=udt(1,lev)-1.                                                  !/*vw        240*/
  390 udt(8,lev)=udt(8,lev)+1.                                                  !/*vw        241*/
      ju=mfl(lmfl+1,lc)                                                         !/*vw        242*/
      lc=mfl(lmfl+2,lc)                                                         !/*vw        243*/
      lev=lev+1                                                                 !/*vw        244*/
      if(ju.gt.0)go to 30                                                       !/*vw        245*/
      n=3+udt(8,lev-1)-laf(-ju+1,1)+laf(-ju+1,2)*(udt(9,lev-1)-                 !/*vw        246*/
     & laf(-ju+2,1)+laf(-ju+2,2)*(udt(10,lev-1)-laf(-ju+3,1)))                  !/*vw        247*/
      lc=laf(-ju+2,n)                                                           !/*vw        248*/
      ju=laf(-ju+1,n)                                                           !/*vw        249*/
      if(ju.ne.0.and.ju.ne.abs(jun(ljun+int(udt(7,lev-1)))))go to 30            !/*vw        250*/
  400 lev=lev-1                                                                 !/*vw        251*/
      lc=udt(7,lev)                                                             !/*vw        252*/
      if(udt(8,lev).lt.udt(2,lev))go to 390                                     !/*vw        253*/
      if(udt(9,lev).lt.udt(4,lev))go to 380                                     !/*vw        254*/
      if(udt(10,lev).lt.udt(6,lev))go to 370                                    !/*vw        255*/
!                                                                               !/*vw        256*/
!        return to recursive call.                                              !/*vw        257*/
  410 if(lev.eq.0)go to 420                                                     !/*vw        258*/
      if(lat(llat+1,int(udt(7,lev-1))).ne.0)go to 400                           !/*vw        259*/
      go to 170                                                                 !/*vw        260*/
!                                                                               !/*vw        261*/
!        put traces of dxtran spheres in zst as dashed lines.                   !/*vw        262*/
  420 call pltdxt(nc,lp)                                                        !/*vw        263*/
!                                                                               !/*vw4c2       2*/
!        plot superimposed mesh.                                                !/*vw4c2       3*/
      if(mod(meshpl/2,2).ne.0)call pltmsh(nc,lp)                                !/*vw4c2       4*/
!                                                                               !/*vw        264*/
!        put cell label locations in zst if required.                           !/*vw        265*/
      nl=0                                                                      !/*vw        266*/
      if(sclabl(2).ne.0.)call celnbr(nl,lp)                                     !/*vw        267*/
!                                                                               !/*vw        268*/
!        fill in the header block of zst.                                       !/*vw        269*/
      zst(lzst+1)=nc                                                            !/*vw        270*/
      zst(lzst+2)=lp-lzst-1                                                     !/*vw        271*/
      zst(lzst+3)=extent(1)                                                     !/*vw        272*/
      zst(lzst+4)=extent(2)                                                     !/*vw        273*/
      zst(lzst+5)=sclabl(1)                                                     !/*vw        274*/
      zst(lzst+6)=sclabl(2)                                                     !/*vw        275*/
      zst(lzst+7)=0.                                                            !/*vw        276*/
      zst(lzst+8)=nl                                                            !/*vw        277*/
      zst(lzst+9)=sclabl(3)                                                     !/*vw        278*/
      zst(lzst+10)=sclabl(4)                                                    !/*vw        279*/
      zst(lzst+11)=0.                                                           !/*vw        280*/
#endif                                                                          /*!vwx        10*/
      return                                                                    !/*vw        281*/
      end                                                                       !/*vw        289*/
