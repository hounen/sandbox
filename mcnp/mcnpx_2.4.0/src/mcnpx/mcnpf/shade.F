!#ident shade.F from deck xr4a                                                  !/*xr4a        1*/
      subroutine shade(np)                                                      !/*xr4a        3*/
#ifdef PLOT                                                                     /*!xrx         1*/
!        shade cells by color using a resolution of 1/np pixels.                !/*xr4a        4*/
      use global_data                                                           !/*xrx         2*/
      use plot_data                                                             !/*xrx         3*/
!                                                                               !/*xrx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*xrx         5*/
      implicit integer(kindi) (i-n)                                             !/*xrx         6*/
!                                                                               !/*xrx         7*/
      real x(4),y(4)                                                            !/*xr4a        9*/
      character ha*10                                                           !/*xrxk        1*/
!                                                                               !/*xrxk        2*/
!        get the maximum/minimum values for the desired shade variable.         !/*xrxk        3*/
      nn=mod(int(sclabl(6)),1000)                                               !/*xrxk        4*/
      ip=int(sclabl(6))/1000                                                    !/*xrxk        5*/
      jv=nint(sclabl(5))                                                        !/*xrxk        6*/
      call pltdat(4,jv,ip,nn,ie,am,an,ha)                                       !/*xrxk        7*/
!                                                                               !/*xrxk        8*/
!        set up to shade by different variables.                                !/*xrxk        9*/
      if(an.eq.huge)an=zero                                                     !/*xrxk       10*/
      if(am.le.an)am=an+one                                                     !/*xrxk       11*/
      if(jv.eq.11)an=tenth*an                                                   !/*xrxk       12*/
      az=an                                                                     !/*xrxk       13*/
      ax=one                                                                    !/*xrxk       14*/
      if(jv.eq.1.or.jv.eq.9.or.(jv.ge.15.and.jv.le.18))then                     !/*xrxk       15*/
         if(am-an.gt.ten)ax=tenth*(am-an)                                       !/*xrxk       16*/
      elseif(am.gt.mcolor*an.and.an.gt.zero)then                                !/*xrxk       17*/
         az=-az                                                                 !/*xrxk       18*/
         am=log(am)                                                             !/*xrxk       19*/
         an=log(an)                                                             !/*xrxk       20*/
      endif                                                                     !/*xrxk       21*/
      ax=ax/(am-an)                                                             !/*xrxk       22*/
!                                                                               !/*xrxk       23*/
      jsu=0                                                                     !/*xr4a       10*/
      ic=0                                                                      !/*xr4a       11*/
      il=0                                                                      !/*xr4c        1*/
      call gsfais(1)                                                            !/*xr4a       12*/
!                                                                               !/*xr4a       13*/
!        move up the vertical window axis with resolution 1/np.                 !/*xr4a       14*/
      do 120 jy=1,np                                                            !/*xr4a       15*/
      t1=extent(2)*(2.*jy/np-1.)                                                !/*xr4a       16*/
      t2=extent(2)*(2.*(jy-1)/np-1.)                                            !/*xr4a       17*/
      t=.501*(t1+t2)                                                            !/*xr4c        2*/
      x1=plmx(2,1)+t*plmx(2,3)                                                  !/*xr4a       19*/
      y1=plmx(3,1)+t*plmx(3,3)                                                  !/*xr4a       20*/
      z1=plmx(4,1)+t*plmx(4,3)                                                  !/*xr4a       21*/
      s=(res-1)*extent(1)                                                       !/*xr4b        1*/
!                                                                               !/*xr4a       23*/
!        move along the horizontal axis by surface crossings.                   !/*xr4a       24*/
   10 xxx=x1+s*plmx(2,2)                                                        !/*xr4a       25*/
      yyy=y1+s*plmx(3,2)                                                        !/*xr4a       26*/
      zzz=z1+s*plmx(4,2)                                                        !/*xr4a       27*/
      uuu=basis(1)                                                              !/*xr4a       28*/
      vvv=basis(2)                                                              !/*xr4a       29*/
      www=basis(3)                                                              !/*xr4a       30*/
      kdb=0                                                                     !/*xr4c        3*/
!                                                                               !/*xrxk       24*/
!        skip cell tracking if this is a weight-window mesh plot.               !/*xrxk       25*/
      if (jv.eq.11.and.wwp(ip,5).lt.0.) then                                    !/*xrxk       26*/
        lev=0                                                                   !/*xrxk       27*/
        lc=0                                                                    !/*xrxk       28*/
        s2=min(s+2.*extent(1)/np,extent(1))                                     !/*xrxk       29*/
        ar=wwval(ip,0,nn,0,jx)                                                  !/*xrxk       30*/
        if(ar.le.zero)ix=0                                                      !/*xrxk       31*/
        go to 98                                                                !/*xrxk       32*/
      endif                                                                     !/*xrxk       33*/
!                                                                               !/*xr4a       31*/
!        find the cell that contains the new point.                             !/*xr4a       32*/
      ju=0                                                                      !/*xr4a       33*/
      lev=0                                                                     !/*xr4a       34*/
      if(ic.eq.0)go to 20                                                       !/*xr4a       35*/
      call chkcel(ic,0,j)                                                       !/*xr4a       36*/
      if(j.eq.0)go to 80                                                        !/*xr4a       37*/
   20 do 70 ic=1,mxa                                                            !/*xr4a       38*/
      if(junf.eq.0)go to 30                                                     !/*xr4a       39*/
      if(abs(jun(ljun+ic)).ne.ju)go to 70                                       !/*xr4a       40*/
      if(lat(llat+1,ic).eq.0)go to 30                                           !/*xr4a       41*/
      call findel(ic)                                                           !/*xr4a       42*/
      go to 40                                                                  !/*xr4a       43*/
   30 call chkcel(ic,0,j)                                                       !/*xr4a       44*/
      if(j.ne.0)go to 70                                                        !/*xr4a       45*/
      if(junf.eq.0)go to 80                                                     !/*xr4a       46*/
   40 if(mfl(lmfl+1,ic).eq.0)go to 80                                           !/*xr4a       47*/
      j=-mfl(lmfl+1,ic)                                                         !/*xr4a       48*/
      if(j.gt.0)go to 50                                                        !/*xr4a       49*/
      ju=mfl(lmfl+1,ic)                                                         !/*xr4a       50*/
      call dunlev(ic)                                                           !/*xr4a       51*/
      go to 20                                                                  !/*xr4a       52*/
   50 lc=1                                                                      !/*xr4a       53*/
      do 60 i=1,3                                                               !/*xr4a       54*/
   60 if(ijk(i).lt.laf(j+i,1).or.ijk(i).gt.laf(j+i,1)+laf(j+i,2)-1)             !/*xrx         8*/
     & go to 90                                                                 !/*xr4a       56*/
      n=3+iii-laf(j+1,1)+laf(j+1,2)*(jjj-laf(j+2,1)+laf(j+2,2)*                 !/*xr4a       57*/
     & (kkk-laf(j+3,1)))                                                        !/*xr4a       58*/
      if(laf(j+1,n).eq.0)go to 90                                               !/*xr4a       59*/
      if(laf(j+1,n).eq.abs(jun(ljun+ic)))go to 80                               !/*xr4a       60*/
      ju=laf(j+1,n)                                                             !/*xr4a       61*/
      call dunlev(ic)                                                           !/*xr4a       62*/
      go to 20                                                                  !/*xr4a       63*/
   70 continue                                                                  !/*xr4a       64*/
      ic=0                                                                      !/*xr4a       65*/
   80 if(ic.eq.0)go to 110                                                      !/*xr4a       66*/
      lc=0                                                                      !/*xr4a       67*/
!                                                                               !/*xr4a       68*/
!        find the next surface crossing.                                        !/*xr4a       69*/
   90 call track(ic)                                                            !/*xr4a       70*/
      if(kdb.eq.0.or.fim(lfim+1,ic).eq.0.)go to 95                              !/*xr4c        4*/
      t=t+.11*(t1-t2)                                                           !/*xr4c        5*/
      if(t.lt.t2+.75*(t1-t2))ic=0                                               !/*xr4c        6*/
      if(ic.eq.0)go to 10                                                       !/*xr4c        7*/
      if(ic.ne.il)call erprnt(1,2,1,ncl(lncl+ic),0,0,0,0,                       !/*xr4c        8*/
     & '22hshading failure.  cell,i5')                                          !/*xr4c        9*/
      il=ic                                                                     !/*xr4c       10*/
   95 s2=extent(1)                                                              !/*xr4c       11*/
      if(dls.lt.huge)s2=min(s+dls,extent(1))                                    !/*xr4a       72*/
      if(lc.eq.1)go to 100                                                      !/*xrxk       34*/
      if(lev.ne.levplt.and.levplt.ge.0)go to 100                                !/*xr4c       12*/
!                                                                               !/*xr4a       74*/
!        set the color index.                                                   !/*xr4a       75*/
      iap=ic                                                                    !/*xrxk       35*/
      call pltdat(-2,jv,ip,nn,ie,ar,a,ha)                                       !/*xrxk       36*/
   98 if(jv.eq.8)then                                                           !/*xrxk       37*/
         if(ar.eq.-1e6)go to 100                                                !/*xrxk       38*/
         ar=abs(ar)                                                             !/*xrxk       39*/
      elseif(jv.eq.11)then                                                      !/*xrxk       40*/
         if(ar.lt.zero)go to 100                                                !/*xrxk       41*/
         if(ar.eq.zero)ar=abs(az)                                               !/*xrxk       42*/
      elseif(jv.eq.12.or.jv.ge.18)then                                          !/*xrxk       43*/
         ar=abs(ar)                                                             !/*xrxk       44*/
      endif                                                                     !/*xrxk       45*/
      if(ar.le.zero)go to 100                                                   !/*xrxk       46*/
      if(az.lt.zero)ar=log(ar)                                                  !/*xrxk       47*/
      n=nint((mcolor-1)*ax*(ar-an))+1                                           !/*xrxk       48*/
      if(n.le.0)go to 100                                                       !/*xrxk       49*/
      if(jv.eq.9)then                                                           !/*xrxk       50*/
         if(mat(lmat+ic).le.mplm)then                                           !/*xrxk       51*/
            if(icolor(mat(lmat+ic)).ne.0)n=icolor(mat(lmat+ic))                 !/*xrxk       52*/
         endif                                                                  !/*xrxk       53*/
      endif                                                                     !/*xrxk       54*/
      if(n.gt.mcolor)n=mod(n-1,mcolor)+1                                        !/*xrxk       55*/
      call gsfaci(kcolor(n))                                                    !/*xr4a       79*/
!                                                                               !/*xr4a       80*/
!        set the fill points and color the rectangular area.                    !/*xr4a       81*/
      x(1)=s                                                                    !/*xr4a       82*/
      x(2)=s2                                                                   !/*xr4a       83*/
      x(3)=x(2)                                                                 !/*xr4a       84*/
      x(4)=x(1)                                                                 !/*xr4a       85*/
      y(1)=t1                                                                   !/*xr4a       86*/
      y(2)=y(1)                                                                 !/*xr4a       87*/
      y(3)=t2                                                                   !/*xr4a       88*/
      y(4)=y(3)                                                                 !/*xr4a       89*/
      call gfa(4,x,y)                                                           !/*xr4a       90*/
!                                                                               !/*xr4a       91*/
!        update the horizontal window position.                                 !/*xr4a       92*/
  100 if(lev.ne.0.or.lc.eq.1)ic=0                                               !/*xr4a       93*/
      if(s2.eq.extent(1))go to 120                                              !/*xr4a       94*/
      s=s2+res*extent(1)/np                                                     !/*xrxk       56*/
      go to 10                                                                  !/*xr4a       96*/
  110 s=s+2.*extent(1)/np                                                       !/*xr4a       97*/
      if(s.lt.extent(1))go to 10                                                !/*xr4a       98*/
  120 continue                                                                  !/*xr4a       99*/
#endif                                                                          /*!xrx         9*/
      return                                                                    !/*xr4a      101*/
      end                                                                       !/*xr4a      102*/
