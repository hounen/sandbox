!#ident intsec.F from deck ic                                                   !/*ic          1*/
      subroutine intsec(jt,jh,lh,ni,ns,ie)                                      !/*ic          2*/
!        find the ni intersections of surface jt with other surfaces.           !/*ic          3*/
!        jh=1,2 for which branch of a torus.  lh=2 if torus, 1 if not.          !/*ic          4*/
!        ns = the facet surface for a polyhedron, zero otherwise.               !/*ic          5*/
!        return with ie=-5 if quart fails.                                      !/*ic          6*/
      use global_data                                                           !/*icx         1*/
      use imcn_data                                                             !/*icx         2*/
!                                                                               !/*icx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*icx         4*/
      implicit integer(kindi) (i-n)                                             !/*icx         5*/
!                                                                               !/*icx         6*/
      dimension r(4),s(4),c(6)                                                  !/*ic          9*/
!                                                                               !/*ic         10*/
      jq=0                                                                      !/*ic         11*/
      ni=0                                                                      !/*ic         12*/
      if(ns.ne.0)go to 50                                                       !/*ic         13*/
!                                                                               !/*ic         14*/
!        get intersections with the axis and with the torus cross axis.         !/*ic         15*/
      if(scfq(lscq+4,jt).ne.0.)sa=-scfq(lscq+5,jt)/scfq(lscq+4,jt)              !/*ic         16*/
      fm=abs(scfq(lscq+1,jt))+abs(scfq(lscq+3,jt))                              !/*ic         17*/
      rb=0.                                                                     !/*ic         18*/
      do 10 i=1,3                                                               !/*ic         19*/
   10 c(i)=scfq(lscq+2+i,jt)                                                    !/*ic         20*/
      do 40 mr=1,2                                                              !/*ic         21*/
      if(mr.eq.1.and.lh.eq.2.and.scfq(lscq+2,jt)*(3-2*jh).lt.0.)go to 30        !/*ic         22*/
      call quad(c,jj,s(1),s(2))                                                 !/*ic         23*/
      do 20 i=1,jj                                                              !/*ic         24*/
      ni=ni+1                                                                   !/*ic         25*/
      iint(liin+ni+1)=jq                                                        !/*ic         26*/
      rsint(lrsn+1,ni+1)=rb                                                     !/*ic         27*/
   20 rsint(lrsn+2,ni+1)=s(i)                                                   !/*ic         28*/
   30 if(lh.ne.2.or.scfq(lscq+2,jt).ge.0.)go to 50                              !/*ic         29*/
      rb=-.5*scfq(lscq+2,jt)                                                    !/*ic         30*/
      c(3)=c(3)-rb**2                                                           !/*ic         31*/
   40 jq=jt                                                                     !/*ic         32*/
!                                                                               !/*ic         33*/
!        look for intersections with the other surfaces.                        !/*ic         34*/
   50 do 160 nb=1,nsv                                                           !/*ic         35*/
      ju=ljsv(llsv+nb)                                                          !/*ic         36*/
      if(ju.eq.jt)go to 160                                                     !/*ic         37*/
!                                                                               !/*ic         38*/
!        calculate one intersection if the cell is a polyhedron.                !/*ic         39*/
      if(ns.eq.0)go to 60                                                       !/*ic         40*/
      t1=scfq(lscq+4,ju)*scfq(lscq+2,jt)-scfq(lscq+4,jt)*scfq(lscq+2,ju)        !/*ic         41*/
      if(t1.eq.0..or.nb.eq.ns)go to 160                                         !/*ic         42*/
      t1=1./t1                                                                  !/*ic         43*/
      ni=ni+1                                                                   !/*ic         44*/
      iint(liin+ni+1)=ju                                                        !/*ic         45*/
      rsint(lrsn+1,ni+1)=(scfq(lscq+5,ju)*scfq(lscq+4,jt)-                      !/*ic         46*/
     & scfq(lscq+5,jt)*scfq(lscq+4,ju))*t1                                      !/*ic         47*/
      rsint(lrsn+2,ni+1)=(scfq(lscq+5,jt)*scfq(lscq+2,ju)-                      !/*ic         48*/
     & scfq(lscq+5,ju)*scfq(lscq+2,jt))*t1                                      !/*ic         49*/
      go to 160                                                                 !/*ic         50*/
!                                                                               !/*ic         51*/
!        calculate up to four intersections if it is a rotational cell.         !/*ic         52*/
   60 if(scfq(lscq+1,jt).eq.0..and.scfq(lscq+1,ju).eq.0.)go to 160              !/*ic         53*/
      j=min(jt,ju)                                                              !/*ic         54*/
      jc=j                                                                      !/*ic         55*/
      jd=max(jt,ju)                                                             !/*ic         56*/
      do 70 i=1,4                                                               !/*ic         57*/
   70 r(i)=-1.                                                                  !/*ic         58*/
!                                                                               !/*ic         59*/
!        set up the equation for the intersections.                             !/*ic         60*/
      t1=scfq(lscq+1,jc)*scfq(lscq+2,jd)-scfq(lscq+1,jd)*scfq(lscq+2,jc)        !/*ic         61*/
      t2=scfq(lscq+1,jc)*scfq(lscq+3,jd)-scfq(lscq+1,jd)*scfq(lscq+3,jc)        !/*ic         62*/
      c(3)=t2                                                                   !/*ic         63*/
      t3=scfq(lscq+1,jc)*scfq(lscq+4,jd)-scfq(lscq+1,jd)*scfq(lscq+4,jc)        !/*ic         64*/
      c(4)=t3                                                                   !/*ic         65*/
      t4=scfq(lscq+1,jc)*scfq(lscq+5,jd)-scfq(lscq+1,jd)*scfq(lscq+5,jc)        !/*ic         66*/
      c(5)=t4                                                                   !/*ic         67*/
      if(t1.eq.0.)go to 110                                                     !/*ic         68*/
      if(scfq(lscq+2,j).eq.0.)j=jc+jd-j                                         !/*ic         69*/
      c(1)=scfq(lscq+1,j)*t2**2                                                 !/*ic         70*/
      c(2)=2.*scfq(lscq+1,j)*t2*t3                                              !/*ic         71*/
      c(3)=scfq(lscq+1,j)*(2.*t2*t4+t3**2)+t1*(scfq(lscq+3,j)*t1-               !/*ic         72*/
     & scfq(lscq+2,j)*t2)                                                       !/*ic         73*/
      c(4)=2.*scfq(lscq+1,j)*t3*t4+t1*(scfq(lscq+4,j)*t1-                       !/*ic         74*/
     & scfq(lscq+2,j)*t3)                                                       !/*ic         75*/
      c(5)=scfq(lscq+1,j)*t4**2+t1*(scfq(lscq+5,j)*t1-scfq(lscq+2,j)*t4)        !/*ic         76*/
      if(c(1).eq.0.)go to 110                                                   !/*ic         77*/
!                                                                               !/*ic         78*/
!        solve a quartic equation for the intersections.                        !/*ic         79*/
      a=1./c(1)                                                                 !/*ic         80*/
      do 80 i=1,5                                                               !/*ic         81*/
   80 c(i)=c(i)*a                                                               !/*ic         82*/
      call quart(4,c,s,j1,1)                                                    !/*ic         83*/
      if(j1.eq.0)go to 160                                                      !/*ic         84*/
!                                                                               !/*ic         85*/
!        zero in on the roots.                                                  !/*ic         86*/
      do 100 m=1,j1                                                             !/*ic         87*/
      z0=1.e-8*(1.+abs(s(m)))                                                   !/*ic         88*/
      do 90 nr=1,20                                                             !/*ic         89*/
      a=c(4)+s(m)*(2.*c(3)+s(m)*(3.*c(2)+4.*s(m)))                              !/*ic         90*/
      if(a.eq.0.)go to 100                                                      !/*ic         91*/
      a=(c(5)+s(m)*(c(4)+s(m)*(c(3)+s(m)*(c(2)+s(m)))))/a                       !/*ic         92*/
      s(m)=s(m)-a                                                               !/*ic         93*/
   90 if(abs(a).le.z0)go to 100                                                 !/*ic         94*/
      ie=-5                                                                     !/*ic         95*/
      return                                                                    !/*ic         96*/
  100 r(m)=-(t4+s(m)*(t3+s(m)*t2))/t1                                           !/*ic         97*/
      go to 130                                                                 !/*ic         98*/
!                                                                               !/*ic         99*/
!        solve a quadratic equation for the intersections.                      !/*ic        100*/
  110 call quad(c(3),j1,s(1),s(2))                                              !/*ic        101*/
      if(j1.eq.0)go to 160                                                      !/*ic        102*/
      if(fm.eq.0.)s(1)=sa                                                       !/*ic        103*/
      if(scfq(lscq+1,j).ne.0..and.scfq(lscq+2,j).ne.0.)j=jc+jd-j                !/*ic        104*/
      if(scfq(lscq+1,j).eq.0..and.scfq(lscq+2,j).eq.0.)j=jc+jd-j                !/*ic        105*/
      c(1)=scfq(lscq+1,j)                                                       !/*ic        106*/
      c(2)=scfq(lscq+2,j)                                                       !/*ic        107*/
      do 120 i=1,j1                                                             !/*ic        108*/
      c(3)=scfq(lscq+5,j)+s(i)*(scfq(lscq+4,j)+s(i)*scfq(lscq+3,j))             !/*ic        109*/
      if(scfq(lscq+1,j)*scfq(lscq+2,j)*t1.eq.0.)call quad(c,j2,                 !/*ic        110*/
     & r(i),r(i+2))                                                             !/*ic        111*/
      if(scfq(lscq+1,j)*scfq(lscq+2,j)*t1.ne.0.)r(i)=-(t3*s(i)+t4)/t1           !/*ic        112*/
  120 s(i+2)=s(i)                                                               !/*ic        113*/
!                                                                               !/*ic        114*/
!        store the intersections, omitting false ones.                          !/*ic        115*/
  130 do 150 i=1,4                                                              !/*ic        116*/
      if(r(i).lt.0.)go to 150                                                   !/*ic        117*/
      do 140 j=jc,jd,jd-jc                                                      !/*ic        118*/
      k=kst(lkst+j)                                                             !/*ic4a        1*/
      if(k.lt.16.or.k.gt.21)go to 140                                           !/*ic        120*/
      l=mod(k-1,3)+1                                                            !/*ic        121*/
      if((bbb(l+1,1)+bbb(l+1,2)*s(i)-scf(lsc(llsc+j)+l+(1-l)*(k/19)))*          !/*ic        122*/
     & scf(lsc(llsc+j)+5-2*(k/19)).lt.0.)go to 150                              !/*ic        123*/
  140 continue                                                                  !/*ic        124*/
      if(lh.eq.2.and.(r(i)+.5*scfq(lscq+2,jt))*(3-2*jh).lt.0.)go to 150         !/*ic        125*/
      ni=ni+1                                                                   !/*ic        126*/
      iint(liin+ni+1)=ju                                                        !/*ic        127*/
      rsint(lrsn+1,ni+1)=r(i)                                                   !/*ic        128*/
      rsint(lrsn+2,ni+1)=s(i)                                                   !/*ic        129*/
  150 continue                                                                  !/*ic        130*/
  160 continue                                                                  !/*ic        131*/
!                                                                               !/*ic        132*/
!        put the intersections in increasing order of s, then r.                !/*ic        133*/
      do 190 i=2,ni                                                             !/*ic        134*/
      do 180 j=i,ni                                                             !/*ic        135*/
      if(rsint(lrsn+2,j+1).gt.rsint(lrsn+2,i).or.rsint(lrsn+2,j+1).eq.          !/*ic        136*/
     & rsint(lrsn+2,i).and.rsint(lrsn+1,j+1).gt.rsint(lrsn+1,i))                !/*ic        137*/
     & go to 180                                                                !/*ic        138*/
      do 170 k=1,2                                                              !/*ic        139*/
      a=rsint(lrsn+k,i)                                                         !/*ic        140*/
      rsint(lrsn+k,i)=rsint(lrsn+k,j+1)                                         !/*ic        141*/
  170 rsint(lrsn+k,j+1)=a                                                       !/*ic        142*/
      k=iint(liin+i)                                                            !/*ic        143*/
      iint(liin+i)=iint(liin+j+1)                                               !/*ic        144*/
      iint(liin+j+1)=k                                                          !/*ic        145*/
  180 continue                                                                  !/*ic        146*/
  190 continue                                                                  !/*ic        147*/
!                                                                               !/*ic        148*/
!        print list of intersections if required.                               !/*ic        149*/
      if(dbcn(7).ne.0..and.ni.ne.0)write(iuo,200)nsf(lnsf+jt),                  !/*ic        150*/
     & (min(1,iint(liin+i))*nsf(lnsf+max(1,iint(liin+i))),                      !/*ic        151*/
     & (rsint(lrsn+j,i),j=1,2),i=2,ni+1)                                        !/*ic        152*/
  200 format(/25h intersections of surface,i7/5x,2hj1,5x,1hr,13x,1hs/           !/*ic        153*/
     & (1x,i6,1p2e14.5))                                                        !/*ic        154*/
      return                                                                    !/*ic        155*/
      end                                                                       !/*ic        156*/
