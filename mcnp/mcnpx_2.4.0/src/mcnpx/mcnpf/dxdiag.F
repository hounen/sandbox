!#ident dxdiag.F from deck dy                                                   !/*dy          1*/
      subroutine dxdiag                                                         !/*dy          2*/
!        print dxtran diagnostics.                                              !/*dy          3*/
      use global_data                                                           !/*dyx         1*/
!                                                                               !/*dyx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*dyx         3*/
      implicit integer(kindi) (i-n)                                             !/*dyx         4*/
!                                                                               !/*dyx         5*/
!                                                                               !/*dy          5*/
!        print separate tables for neutron and photon spheres.                  !/*dy          6*/
      do 140 ip=1,mipt                                                          !/*dy          7*/
      if(ndx(ip).eq.0)go to 140                                                 !/*dy          8*/
      if(dbcn(26).ne.0.)then                                                    !/*dyx         6*/
      write(iuo,11)hnp(ip)(1:leng(hnp(ip)))//'s',nps                            !/*dyx         7*/
   11 format(23h1dxtran diagnostics -- ,a9,7x,5hnps =,i9,                       !/*dyx         8*/
     & 51x,15hprint table 150)                                                  !/*dyx         9*/
      else                                                                      !/*dyx        10*/
      write(iuo,10)hnp(ip),nps                                                  !/*dyx        11*/
   10 format(23h1dxtran diagnostics -- ,a11,7x,5hnps =,i9,                      !/*dyx        12*/
     & 49x,15hprint table 150)                                                  !/*dyx        13*/
      endif                                                                     !/*dyx        14*/
!                                                                               !/*dy         12*/
!        print a table for each dxtran sphere.                                  !/*dy         13*/
      do 130 n=1,ndx(ip)                                                        !/*dy         14*/
      if(ddx(ip,1,n).lt.0.)write(iuo,20)n,(dxx(ip,i,n),i=1,3),                  !/*dy         15*/
     & '      maximum weight'                                                   !/*dy         16*/
      if(ddx(ip,1,n).ge.0.)write(iuo,20)n,(dxx(ip,i,n),i=1,3),                  !/*dy         17*/
     & 'times average weight'                                                   !/*dy         18*/
   20 format(//18h dxtran sphere no.,i2,19h located at x,y,z =,1p3e12.5/        !/*dy         19*/
     & 45x,10hcumulative,8x,6hweight,11x,10hcumulative/                         !/*dy         20*/
     & 45x,11hfraction of,7x,11htransmitted,6x,11hfraction of/                  !/*dy         21*/
     & 2x,a20,5x,13htransmissions,5x,13htransmissions,5x,                       !/*dy         22*/
     & 11hper history,6x,12htotal weight/)                                      !/*dy         23*/
      t1=0.                                                                     !/*dy         24*/
      t2=0.                                                                     !/*dy         25*/
      t3=0.                                                                     !/*dy         26*/
      do 30 j=1,9                                                               !/*dy         27*/
   30 t3=t3+dxd(ldxd+ip,j,n)                                                    !/*dy         28*/
      do 40 j=1,8                                                               !/*dy         29*/
      if(t3.ne.0.)t1=t1+dxd(ldxd+ip,j,n)/t3                                     !/*dy         30*/
      if(dxd(ldxd+ip,20,n).ne.0.)t2=t2+dxd(ldxd+ip,j+9,n)/                      !/*dy         31*/
     & dxd(ldxd+ip,20,n)                                                        !/*dy         32*/
      if(ddx(ip,1,n).ge.0.)t4=talb(j,1)                                         !/*dy         33*/
      if(ddx(ip,1,n).ge.0..and.j.eq.1)t4=ddx(ip,1,n)                            !/*dy         34*/
      if(ddx(ip,1,n).lt.0.)t4=-talb(j,2)*ddx(ip,1,n)                            !/*dy         35*/
   40 write(iuo,50)t4,int(dxd(ldxd+ip,j,n)),t1,fpi*dxd(ldxd+ip,j+9,n),t2        !/*dy         36*/
      if(ddx(ip,1,n).lt.0.)write(iuo,50)huge,int(dxd(ldxd+ip,9,n)),1.,          !/*dy         37*/
     & fpi*dxd(ldxd+ip,18,n),1.                                                 !/*dy         38*/
   50 format(1pe19.4,i15,0pf20.5,1pe20.5,0pf15.5)                               !/*dy         39*/
      if(ddx(ip,1,n).ge.0.)write(iuo,60)int(dxd(ldxd+ip,9,n)),1.,               !/*dy         40*/
     & fpi*dxd(ldxd+ip,18,n),1.                                                 !/*dy         41*/
   60 format(19h  1st 200 histories,i15,0pf20.5,1pe20.5,0pf15.5)                !/*dy         42*/
      t=1.                                                                      !/*dy         43*/
      if(dxd(ldxd+ip,20,n).ne.0.)                                               !/*dy         44*/
     & t=dxd(ldxd+ip,21,n)/(dxd(ldxd+ip,20,n)*fpi)                              !/*dy         45*/
      write(iuo,70)dxd(ldxd+ip,20,n)*fpi,dxd(ldxd+ip,21,n),t,                   !/*dy         46*/
     & int(dxd(ldxd+ip,23,n))                                                   !/*dy         47*/
   70 format(/29h average weight per history =,1pe12.5,7x,                      !/*dy         48*/
     & 16hlargest weight =,e12.5/                                               !/*dy         49*/
     & 18h largest/average =,e12.5,18x,16hnps of largest =,i9)                  !/*dy         50*/
      write(iuo,80)                                                             !/*dy         51*/
   80 format(//22h contributions by cell//8x,4hcell,4x,6hmisses,                !/*dy         52*/
     & 6x,4hhits,4x,18hweight per history,4x,14hweight per hit/)                !/*dy         53*/
      j=0                                                                       !/*dy         54*/
      m=0                                                                       !/*dy         55*/
      t=0.                                                                      !/*dy         56*/
      l=mxa*((ip-1)*ndx(1)+n-1)                                                 !/*dy         57*/
      do 100 i=1,mxa                                                            !/*dy         58*/
      if(dxc(ldxc+3,l+i).eq.0.)go to 100                                        !/*dy         59*/
      j=j+dxc(ldxc+3,l+i)                                                       !/*dy         60*/
      m=m+dxc(ldxc+1,l+i)                                                       !/*dy         61*/
      t=t+dxc(ldxc+2,l+i)                                                       !/*dy         62*/
      write(iuo,90)i,ncl(lncl+i),int(dxc(ldxc+3,l+i)-dxc(ldxc+1,l+i)),          !/*dy         63*/
     & int(dxc(ldxc+1,l+i)),dxc(ldxc+2,l+i)*fpi,                                !/*dy         64*/
     & dxc(ldxc+2,l+i)/max(dxc(ldxc+1,l+i),one)                                 !/*dy         65*/
   90 format(2i6,2i10,1pe18.5,e20.5)                                            !/*dy         66*/
  100 continue                                                                  !/*dy         67*/
      if(m.ne.0)write(iuo,110)j-m,m,t*fpi,t/m                                   !/*dy         68*/
  110 format(7x,5htotal,2i10,1pe18.5,e20.5)                                     !/*dy         69*/
      write(iuo,120)(nziy(i,n,ip),i=1,8)                                        !/*dy         70*/
  120 format(//7h misses/26h   russian roulette on dxc,i19/                     !/*dy         71*/
     & 9h   psc=0.,i36/35h   russian roulette in transmission,i10/              !/*dy         72*/
     & 28h   underflow in transmission,i17/                                     !/*dy         73*/
     & 29h   hit a zero-importance cell,i16/14h   time cutoff,i31/              !/*dy         74*/
     & 16h   energy cutoff,i29/16h   weight cutoff,i29)                         !/*dy         75*/
  130 continue                                                                  !/*dy         76*/
  140 continue                                                                  !/*dy         77*/
      return                                                                    !/*dy         78*/
      end                                                                       !/*dy         79*/
