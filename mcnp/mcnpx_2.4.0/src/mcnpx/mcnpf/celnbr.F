!#ident celnbr.F from deck cl                                                   !/*cl          1*/
      subroutine celnbr(nl,lp)                                                  !/*cl          3*/
#ifdef PLOT                                                                     /*!clx         1*/
!        put cell label values and locations in zst(lp).                        !/*cl          4*/
!        increment lp.  return nl = the number of cell labels.                  !/*cl          5*/
      use allocate_memory                                                       !/*clx         2*/
      use global_data                                                           !/*clx         3*/
      use plot_data                                                             !/*clx         4*/
!                                                                               !/*clx         5*/
      implicit real(kindr) (a-h,o-z)                                            !/*clx         6*/
      implicit integer(kindi) (i-n)                                             !/*clx         7*/
!                                                                               !/*clx         8*/
      character ha*10                                                           !/*cl          8*/
      logical lf                                                                !/*cl          9*/
      lf(i1,j1)=abs(kcl(lkcl+i+i1,j+j1)).ne.ic                                  !/*cl         10*/
!                                                                               !/*cl         11*/
!        put cell numbers in kcl.                                               !/*cl         12*/
      jsu=0                                                                     !/*cl         13*/
      ic=0                                                                      !/*cl         14*/
      do 80 jy=2,101                                                            !/*cl         15*/
      t=extent(2)*(jy-51.5)*.02                                                 !/*cl         16*/
      x1=plmx(2,1)+t*plmx(2,3)                                                  !/*cl         17*/
      y1=plmx(3,1)+t*plmx(3,3)                                                  !/*cl         18*/
      z1=plmx(4,1)+t*plmx(4,3)                                                  !/*cl         19*/
      do 70 ix=2,101                                                            !/*cl         20*/
      s=extent(1)*(ix-51.5)*.02                                                 !/*cl         21*/
      xxx=x1+s*plmx(2,2)                                                        !/*cl         22*/
      yyy=y1+s*plmx(3,2)                                                        !/*cl         23*/
      zzz=z1+s*plmx(4,2)                                                        !/*cl         24*/
      ju=0                                                                      !/*cl         25*/
      lev=0                                                                     !/*cl         26*/
      if(ic.eq.0)go to 10                                                       !/*cl         27*/
      call chkcel(ic,0,j)                                                       !/*cl         28*/
      if(j.eq.0)go to 60                                                        !/*cl         29*/
   10 do 50 ic=1,mxa                                                            !/*cl         30*/
      if(junf.eq.0)go to 20                                                     !/*cl         31*/
      if(abs(jun(ljun+ic)).ne.ju)go to 50                                       !/*cl         32*/
      if(lat(llat+1,ic).eq.0)go to 20                                           !/*cl         33*/
      call findel(ic)                                                           !/*cl         34*/
      go to 30                                                                  !/*cl         35*/
   20 call chkcel(ic,0,j)                                                       !/*cl         36*/
      if(j.ne.0)go to 50                                                        !/*cl         37*/
      if(junf.eq.0)go to 60                                                     !/*cl         38*/
   30 if(mfl(lmfl+1,ic).eq.0)go to 60                                           !/*cl         39*/
      j=-mfl(lmfl+1,ic)                                                         !/*cl         40*/
      if(j.gt.0)go to 40                                                        !/*cl         41*/
      if(lev.eq.levplt)go to 60                                                 !/*cl4c        1*/
      ju=mfl(lmfl+1,ic)                                                         !/*cl         42*/
      call dunlev(ic)                                                           !/*cl         43*/
      go to 10                                                                  !/*cl         44*/
   40 do 45 i=1,3                                                               !/*cl4a        3*/
   45 if(ijk(i).lt.laf(j+i,1).or.ijk(i).gt.laf(j+i,1)+laf(j+i,2)-1)             !/*clx         9*/
     & go to 55                                                                 !/*cl4a        5*/
      n=3+iii-laf(j+1,1)+laf(j+1,2)*(jjj-laf(j+2,1)+laf(j+2,2)*                 !/*cl4a        6*/
     & (kkk-laf(j+3,1)))                                                        !/*cl         46*/
      if(laf(j+1,n).eq.0)go to 55                                               !/*cl4a        7*/
      if(laf(j+1,n).eq.abs(jun(ljun+ic)))go to 60                               !/*cl         47*/
      if(lev.eq.levplt)go to 60                                                 !/*cl4c        2*/
      ju=laf(j+1,n)                                                             !/*cl         49*/
      call dunlev(ic)                                                           !/*cl         50*/
      go to 10                                                                  !/*cl         51*/
   50 continue                                                                  !/*cl         52*/
   55 ic=0                                                                      !/*cl4a        8*/
      go to 68                                                                  !/*cl4c2       1*/
!        get mesh index if label is wwn with mesh-based weight windows.         !/*cl4c2       2*/
   60 if(nint(sclabl(3)).ne.11)go to 68                                         !/*cl4c2       3*/
      ip=int(sclabl(4))/1000                                                    !/*cl4c2       4*/
      if(wwp(ip,5).ge.0.)go to 68                                               !/*cl4c2       5*/
      kcl(lkcl+ix,jy)=0                                                         !/*cl4c2       6*/
      if(lev.ne.0.or.ic.eq.0)go to 70                                           !/*cl4c2       7*/
      ie=mod(int(sclabl(4)),1000)                                               !/*cl4c2       8*/
      ww=wwval(ip,ic,ie,1,jx)                                                   !/*cl4c2       9*/
      if(jx.eq.0)go to 70                                                       !/*cl4c2      10*/
      kcl(lkcl+ix,jy)=jx                                                        !/*cl4c2      11*/
      if(lev.ne.levplt.and.levplt.ge.0)kcl(lkcl+ix,jy)=-jx                      !/*cl4c2      12*/
      go to 70                                                                  !/*cl4c2      13*/
!        otherwise store cell index in pixel array.                             !/*cl4c2      14*/
   68 kcl(lkcl+ix,jy)=ic                                                        !/*cl4c2      15*/
      if(lev.ne.levplt.and.levplt.ge.0)kcl(lkcl+ix,jy)=-ic                      !/*cl4c        3*/
   70 if(lev.ne.0)ic=0                                                          !/*cl         55*/
   80 continue                                                                  !/*cl         56*/
!                                                                               !/*cl         57*/
!        find the best locations for the cell labels.                           !/*cl         58*/
      mx=max(nwwm,mxa)                                                          !/*cl4c2      16*/
      do 90 i=1,mx                                                              !/*cl4c2      17*/
   90 lcl(llcl+i)=0                                                             !/*cl         60*/
      ks=0                                                                      !/*cl         61*/
      do 520 jq=1,10000                                                         !/*cl         62*/
      ks=mod(ks+1879,10000)                                                     !/*cl         63*/
      jy=ks/100+2                                                               !/*cl         64*/
      ix=ks-(jy-2)*100+2                                                        !/*cl         65*/
!                                                                               !/*cl         66*/
!        put the point on the stack if it is in a new piece of cell.            !/*cl         67*/
      if(kcl(lkcl+ix,jy).le.0)go to 520                                         !/*cl         68*/
      ic=kcl(lkcl+ix,jy)                                                        !/*cl         69*/
      kcl(lkcl+ix,jy)=-ic                                                       !/*cl         70*/
      ns=1                                                                      !/*cl         71*/
      ml=1                                                                      !/*cl         72*/
      jst(ljst+1,1)=ix                                                          !/*cl         73*/
      jst(ljst+2,1)=jy                                                          !/*cl         74*/
!                                                                               !/*cl         75*/
!        get the value and size of the label.                                   !/*cl         76*/
      nn=mod(int(sclabl(4)),1000)                                               !/*cl4c2      18*/
      ip=int(sclabl(4))/1000                                                    !/*cl4c2      19*/
      jv=nint(sclabl(3))                                                        !/*cl4c2      20*/
      iap=ic                                                                    !/*cl4c2      21*/
      call pltdat(2,jv,ip,nn,ie,ar,an,ha)                                       !/*clxk        1*/
      call fshort(ar,ha,nc)                                                     !/*cl4c2      24*/
      nw=nc/2                                                                   !/*cl        132*/
      mw=nw/2                                                                   !/*cl        133*/
!                                                                               !/*cl        134*/
!        find the best point for the label in the piece of cell.                !/*cl        135*/
      gb=-1000.                                                                 !/*cl        136*/
  310 i=jst(ljst+1,ns)                                                          !/*cl        137*/
      j=jst(ljst+2,ns)                                                          !/*cl        138*/
      ns=ns-1                                                                   !/*cl        139*/
!                                                                               !/*cl        140*/
!        put neighbor points on the stack if they are in the same cell.         !/*cl        141*/
      do 330 k=i-1,i+1                                                          !/*cl        142*/
      do 320 l=j-1,j+1                                                          !/*cl        143*/
      if(kcl(lkcl+k,l).ne.ic)go to 320                                          !/*cl        144*/
      kcl(lkcl+k,l)=-ic                                                         !/*cl        145*/
      ns=ns+1                                                                   !/*cl        146*/
      if(ns.ge.10404)go to 520                                                  !/*cl4c2      25*/
      ml=ml+1                                                                   !/*cl        147*/
      jst(ljst+1,ns)=k                                                          !/*cl        148*/
      jst(ljst+2,ns)=l                                                          !/*cl        149*/
  320 continue                                                                  !/*cl        150*/
  330 continue                                                                  !/*cl        151*/
!                                                                               !/*cl        152*/
!        calculate the worth of the current point.                              !/*cl        153*/
      do 340 l=1,nw                                                             !/*cl        154*/
  340 if(lf(l,0).or.lf(-l,0))go to 350                                          !/*cl        155*/
      go to 360                                                                 !/*cl        156*/
  350 k=l-nw                                                                    !/*cl        157*/
      go to 410                                                                 !/*cl        158*/
  360 do 370 k=1,100                                                            !/*cl        159*/
  370 if(lf(0,k).or.lf(0,-k).or.lf(nw+k,0).or.lf(-nw-k,0))go to 380             !/*cl        160*/
  380 if(k+1.lt.gb)go to 510                                                    !/*cl        161*/
      do 390 l=1,(2*(k+1))/3-1                                                  !/*cl        162*/
  390 if(lf(mw+l,l).or.lf(mw+l,-l).or.lf(-mw-l,l).or.lf(-mw-l,-l))              !/*cl        163*/
     & go to 400                                                                !/*cl        164*/
      go to 420                                                                 !/*cl        165*/
  400 k=(l*3)/2                                                                 !/*cl        166*/
  410 if(k+1.lt.gb)go to 510                                                    !/*cl        167*/
  420 l=max(1,k)                                                                !/*cl        168*/
      do 430 k1=l,100                                                           !/*cl        169*/
  430 if(lf(0,k1))go to 440                                                     !/*cl        170*/
  440 do 450 k2=l,100                                                           !/*cl        171*/
  450 if(lf(0,-k2))go to 460                                                    !/*cl        172*/
  460 do 470 k3=l,100                                                           !/*cl        173*/
  470 if(lf(nw+k3,0))go to 480                                                  !/*cl        174*/
  480 do 490 k4=l,100                                                           !/*cl        175*/
  490 if(lf(-nw-k4,0))go to 500                                                 !/*cl        176*/
  500 g=k+.0000001*k1*k2*k3*k4                                                  !/*cl        177*/
!                                                                               !/*cl        178*/
!        if the point is better than the best one so far, remember it.          !/*cl        179*/
      if(g.lt.gb)go to 510                                                      !/*cl        180*/
      gb=g                                                                      !/*cl        181*/
      ib=i                                                                      !/*cl        182*/
      jb=j                                                                      !/*cl        183*/
      l1=k1                                                                     !/*cl        184*/
      l2=k2                                                                     !/*cl        185*/
      l3=k3                                                                     !/*cl        186*/
      l4=k4                                                                     !/*cl        187*/
  510 if(ns.gt.0)go to 310                                                      !/*cl        188*/
!                                                                               !/*cl        189*/
!        put the label value and its location in zst.                           !/*cl        190*/
      if(ml.le.lcl(llcl+ic))go to 520                                           !/*cl        191*/
      lcl(llcl+ic)=min(ml,5)                                                    !/*cl        192*/
      nl=nl+1                                                                   !/*cl        193*/
      if(size(zst).lt.(lp+6)) then                                              !/*clx        10*/
        zst => chg_mem (zst,lp+5006,is)                                         !/*clx        11*/
        if(is.ne.0)call expire(0,'celnbr','chg_mem error')                      !/*clx        12*/
      endif                                                                     !/*clx        13*/
      zst(lp)=ar                                                                !/*cl        195*/
      zst(lp+1)=extent(1)*max(-1.,min(1.-real(sch*sclabl(2))*nc,                !/*cl4a       10*/
     & (ib+.5*max(-1,min(1,l3-l4))-51.5)/50.-real(sch/2.*sclabl(2))*nc))        !/*cl4a       11*/
      zst(lp+2)=extent(2)*max(-1.,min(1.-real(sch*sclabl(2)),                   !/*cl4a       12*/
     & (jb+.5*max(-1,min(1,l1-l2))-51.5)/50.-real(sch/2.*sclabl(2))))           !/*cl4a       13*/
      lp=lp+3                                                                   !/*cl        200*/
  520 continue                                                                  !/*cl        201*/
#endif                                                                          /*!clx        14*/
      return                                                                    !/*cl        202*/
      end                                                                       !/*cl        203*/
