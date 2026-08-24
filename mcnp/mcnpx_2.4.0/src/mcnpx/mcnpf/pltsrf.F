!#ident pltsrf.F from deck pf                                                   !/*pf          1*/
      subroutine pltsrf(nc,lp,ss,tt,lx)                                         !/*pf          3*/
#ifdef PLOT                                                                     /*!pfx         1*/
!        put points that define the arcs of surface jsu into zst.               !/*pf          4*/
!        increment nc by the number of arcs generated.                          !/*pf          5*/
!        increment lp by the number of entries added to zst.                    !/*pf          6*/
!        ss and tt are net offsets in hexagonal prism lattices.                 !/*pf          7*/
!        lx is flag for hexagonal prism lattice with base surfaces.             !/*pf          8*/
      use allocate_memory                                                       !/*pfx         2*/
      use global_data                                                           !/*pfx         3*/
      use plot_data                                                             !/*pfx         4*/
!                                                                               !/*pfx         5*/
      implicit real(kindr) (a-h,o-z)                                            !/*pfx         6*/
      implicit integer(kindi) (i-n)                                             !/*pfx         7*/
!                                                                               !/*pfx         8*/
      if(lev.ne.levplt.and.levplt.ge.0)return                                   !/*pf4c        1*/
!                                                                               !/*pf         11*/
!        do the one or two curves of the surface.                               !/*pf         12*/
      ks=kst(lkst+jsu)                                                          !/*pf4a        1*/
      jl=lsc(llsc+jsu)                                                          !/*pf         14*/
      de=0.                                                                     !/*pf         15*/
      if(ks.gt.23)de=scf(jl+4)**2-scf(jl+6)**2                                  !/*pf         16*/
      kd=3                                                                      !/*pf         17*/
      lj=jsu                                                                    !/*pf4b        1*/
      if(junf.eq.0)go to 10                                                     !/*pf         18*/
      if(lat(llat+1,lcl(llcl+1)).ne.0)kd=1                                      !/*pf         19*/
   10 do 170 iqc=1,ncs(lncs+jsu)                                                !/*pf         20*/
!                                                                               !/*pf         21*/
!        find the intersections of the curve with all the other curves.         !/*pf         22*/
      call inter                                                                !/*pf         23*/
!                                                                               !/*pf         24*/
!        determine whether and how each segment should be plotted.              !/*pf         25*/
      ke=0                                                                      !/*pf         26*/
      lsg(llsg+1)=0                                                             !/*pf         27*/
      do 90 js=1,nxp-1                                                          !/*pf         28*/
      call ptost(.51729*crs(lcrs+js)+.48271*crs(lcrs+js+1),iqc,jsu,s,t)         !/*pf         29*/
      kl=0                                                                      !/*pf         30*/
      kr=0                                                                      !/*pf         31*/
      if(abs(s).gt.1..or.abs(t).gt.1.)go to 80                                  !/*pf         32*/
!                                                                               !/*pf         33*/
!        check cells at higher levels.                                          !/*pf         34*/
      if(lev.eq.0)go to 35                                                      !/*pf4a        2*/
      xxx=pxx(2,1)+s*extent(1)*pxx(2,2)+t*extent(2)*pxx(2,3)                    !/*pf         36*/
      yyy=pxx(3,1)+s*extent(1)*pxx(3,2)+t*extent(2)*pxx(3,3)                    !/*pf         37*/
      zzz=pxx(4,1)+s*extent(1)*pxx(4,2)+t*extent(2)*pxx(4,3)                    !/*pf         38*/
      do 30 lz=0,lev-1                                                          !/*pf         39*/
      l=lev-1-lz                                                                !/*pf         40*/
      j=-mfl(lmfl+1,int(udt(7,l)))                                              !/*pf         41*/
      if(j.lt.0)m=mfl(lmfl+3,int(udt(7,l)))                                     !/*pf         42*/
      if(j.gt.0)m=laf(j+3,int(3+udt(8,l)-laf(j+1,1)+laf(j+1,2)*                 !/*pf         43*/
     & (udt(9,l)-laf(j+2,1)+laf(j+2,2)*(udt(10,l)-laf(j+3,1)))))                !/*pf         44*/
      x=xxx                                                                     !/*pf         45*/
      y=yyy                                                                     !/*pf         46*/
      z=zzz                                                                     !/*pf         47*/
      if(l.eq.lev-1)go to 20                                                    !/*pf         48*/
      if(lat(llat+1,int(udt(7,l+1))).eq.0)go to 20                              !/*pf         49*/
      k=lat(llat+2,int(udt(7,l+1)))                                             !/*pf         50*/
      x=x+udt(8,l+1)*vcl(lvcl+1,1,k)+                                           !/*pf         51*/
     & udt(9,l+1)*vcl(lvcl+1,2,k)+udt(10,l+1)*vcl(lvcl+1,3,k)                   !/*pf         52*/
      y=y+udt(8,l+1)*vcl(lvcl+2,1,k)+                                           !/*pf         53*/
     & udt(9,l+1)*vcl(lvcl+2,2,k)+udt(10,l+1)*vcl(lvcl+2,3,k)                   !/*pf         54*/
      z=z+udt(8,l+1)*vcl(lvcl+3,1,k)+                                           !/*pf         55*/
     & udt(9,l+1)*vcl(lvcl+3,2,k)+udt(10,l+1)*vcl(lvcl+3,3,k)                   !/*pf         56*/
   20 xxx=x*trf(ltrf+5,m)+y*trf(ltrf+6,m)+z*trf(ltrf+7,m)+                      !/*pf         57*/
     & trf(ltrf+15,m)                                                           !/*pf         58*/
      yyy=x*trf(ltrf+8,m)+y*trf(ltrf+9,m)+z*trf(ltrf+10,m)+                     !/*pf         59*/
     & trf(ltrf+16,m)                                                           !/*pf         60*/
      zzz=x*trf(ltrf+11,m)+y*trf(ltrf+12,m)+z*trf(ltrf+13,m)+                   !/*pf         61*/
     & trf(ltrf+17,m)                                                           !/*pf         62*/
      if(ksc(lksc+jsu).ne.0)jsu=-lj                                             !/*pf4b        2*/
      call chkcel(nint(udt(7,l)),2,j)                                           !/*pf4b        3*/
      jsu=lj                                                                    !/*pf4b        4*/
   30 if(j.ne.0)go to 80                                                        !/*pf         64*/
   35 if(ncel.ne.0)go to 40                                                     !/*pf4a        3*/
      kl=1                                                                      !/*pf         66*/
      kr=1                                                                      !/*pf         67*/
      go to 80                                                                  !/*pf         68*/
!                                                                               !/*pf         69*/
!        check cells on either side of the surface.                             !/*pf         70*/
   40 xxx=pxx(2,1)+(s-ss)*extent(1)*pxx(2,2)+(t-tt)*extent(2)*pxx(2,3)          !/*pf         71*/
      yyy=pxx(3,1)+(s-ss)*extent(1)*pxx(3,2)+(t-tt)*extent(2)*pxx(3,3)          !/*pf         72*/
      zzz=pxx(4,1)+(s-ss)*extent(1)*pxx(4,2)+(t-tt)*extent(2)*pxx(4,3)          !/*pf         73*/
      if(lx.ne.0)lca(llca+lcl(llcl+1)+1)=lca(llca+lcl(llcl+1)+1)-               !/*pf         74*/
     & sign(2,lca(llca+lcl(llcl+1)+1))                                          !/*pf         75*/
      if(de.ge.0.)go to 50                                                      !/*pf         76*/
      tpp(1)=(xxx-scf(jl+1))**2                                                 !/*pf         77*/
      tpp(2)=(yyy-scf(jl+2))**2                                                 !/*pf         78*/
      tpp(3)=(zzz-scf(jl+3))**2                                                 !/*pf         79*/
      tpp(ks-23)=tpp(ks-23)*scf(jl+7)                                           !/*pf         80*/
      if((tpp(1)+tpp(2)+tpp(3)+de)*scf(jl+4).lt.0.)go to 80                     !/*pf         81*/
   50 do 70 ic=1,ncel                                                           !/*pf         82*/
      il=lcl(llcl+ic)                                                           !/*pf         83*/
      call chkcel(il,2,jf)                                                      !/*pf         84*/
      ns=abs(lca(llca+il+1))-abs(lca(llca+il))                                  !/*pf         85*/
      do 60 i=1,ns                                                              !/*pf         86*/
   60 if(abs(lja(llja+abs(lca(llca+il))-1+i)).eq.jsu)lgc(i)=1-lgc(i)            !/*pf         87*/
      j=lgeval(lgc,ns)                                                          !/*pf         88*/
      kr=max(kr,kr+1-jf-j)                                                      !/*pf         89*/
   70 kl=max(kl,kl-1+jf+j)                                                      !/*pf         90*/
      if(lx.ne.0)lca(llca+il+1)=lca(llca+il+1)+sign(2,lca(llca+il+1))           !/*pf         91*/
   80 if(kl*kr.ne.1)ke=js+1                                                     !/*pf         92*/
      lsg(llsg+js+1)=kd                                                         !/*pf         93*/
      if(kl*kr.eq.1)lsg(llsg+js+1)=1                                            !/*pf         94*/
   90 if(kl+kr.eq.0)lsg(llsg+js+1)=0                                            !/*pf         95*/
!                                                                               !/*pf         96*/
!        rearrange ellipse intersections to avoid extra surface label.          !/*pf         97*/
      if(kfm(lkfm+jsu).ne.3)go to 100                                           !/*pf         98*/
      if(ke.eq.0.or.crs(lcrs+1).ne.-pie.or.crs(lcrs+nxp).ne.pie.or.             !/*pf         99*/
     & lsg(llsg+2).ne.1.or.lsg(llsg+nxp).ne.1)go to 100                         !/*pf        100*/
      nxp=ke                                                                    !/*pf        101*/
      crs(lcrs+1)=crs(lcrs+ke)-2.*pie                                           !/*pf        102*/
!                                                                               !/*pf        103*/
!        put the points to be plotted into zst.                                 !/*pf        104*/
  100 do 160 js=1,nxp-1                                                         !/*pf        105*/
      if(lsg(llsg+js+1).eq.0)go to 160                                          !/*pf        106*/
!                                                                               !/*pf        107*/
!        start a new arc if there is a gap or the line style changes.           !/*pf        108*/
      if(lsg(llsg+js+1).eq.lsg(llsg+js))go to 110                               !/*pf        109*/
      nc=nc+1                                                                   !/*pf        110*/
      la=lp                                                                     !/*pf        111*/
      if(size(zst).lt.(la+8)) then                                              !/*pfx         9*/
        zst => chg_mem (zst,la+5008,is)                                         !/*pfx        10*/
        if(is.ne.0)call expire(0,'pltsrf','chg_mem error')                      !/*pfx        11*/
      endif                                                                     !/*pfx        12*/
      zst(la)=0.                                                                !/*pf        113*/
      jq=nsf(lnsf+jsu)                                                          !/*pf4c        2*/
      if(kfq.eq.0)go to 106                                                     !/*pf4c        3*/
      if(idne(lide+idne(lide+2)+4).ne.1)go to 106                               !/*pf4c        4*/
      jq=jq*10+kfq+1000000                                                      !/*pf4c        5*/
  106 if(ncel.ne.0)zst(la)=jq                                                   !/*pf4c        6*/
      if(ncel.ne.0.and.ksu(lksu+jsu).ne.0)zst(la)=-jq                           !/*pf4c        7*/
      zst(la+1)=1                                                               !/*pf        115*/
      zst(la+2)=lsg(llsg+js+1)                                                  !/*pf        116*/
      p1=crs(lcrs+js)                                                           !/*pf        117*/
      call ptost(p1,iqc,jsu,s1,t1)                                              !/*pf        118*/
      zst(lp+3)=s1*extent(1)                                                    !/*pf        119*/
      zst(lp+4)=t1*extent(2)                                                    !/*pf        120*/
      lp=lp+5                                                                   !/*pf        121*/
      go to 120                                                                 !/*pf        122*/
  110 if(kfm(lkfm+jsu).ne.1)go to 120                                           !/*pf        123*/
      lp=lp-2                                                                   !/*pf        124*/
      zst(la+1)=zst(la+1)-1                                                     !/*pf        125*/
!                                                                               !/*pf        126*/
!        calculate enough points to define the segment.                         !/*pf        127*/
  120 p1=crs(lcrs+js)                                                           !/*pf        128*/
      p2=crs(lcrs+js+1)                                                         !/*pf        129*/
      call ptost(p2,iqc,jsu,se,te)                                              !/*pf        130*/
  130 p2=crs(lcrs+js+1)                                                         !/*pf        131*/
      s2=se                                                                     !/*pf        132*/
      t2=te                                                                     !/*pf        133*/
      if(kfm(lkfm+jsu).eq.1)go to 150                                           !/*pf        134*/
  140 call ptost(.5*(p1+p2),iqc,jsu,sm,tm)                                      !/*pf        135*/
      if(((sm-s1)*(t2-t1)-(tm-t1)*(s2-s1))**2.le.res**2*                        !/*pf        136*/
     & ((t2-t1)**2+(s2-s1)**2))go to 150                                        !/*pf        137*/
      p2=.5*(p1+p2)                                                             !/*pf        138*/
      s2=sm                                                                     !/*pf        139*/
      t2=tm                                                                     !/*pf        140*/
      go to 140                                                                 !/*pf        141*/
  150 if(size(zst).lt.(lp+5)) then                                              !/*pfx        13*/
        zst => chg_mem (zst,lp+5005,is)                                         !/*pfx        14*/
        if(is.ne.0)call expire(0,'pltsrf','chg_mem error')                      !/*pfx        15*/
      endif                                                                     !/*pfx        16*/
      zst(lp)=s2*extent(1)                                                      !/*pf        143*/
      zst(lp+1)=t2*extent(2)                                                    !/*pf        144*/
      lp=lp+2                                                                   !/*pf        145*/
      zst(la+1)=zst(la+1)+1                                                     !/*pf        146*/
      p1=p2                                                                     !/*pf        147*/
      s1=s2                                                                     !/*pf        148*/
      t1=t2                                                                     !/*pf        149*/
      if(p2.ne.crs(lcrs+js+1))go to 130                                         !/*pf        150*/
  160 continue                                                                  !/*pf        151*/
  170 continue                                                                  !/*pf        152*/
#endif                                                                          /*!pfx        17*/
      return                                                                    !/*pf        153*/
      end                                                                       !/*pf        154*/
