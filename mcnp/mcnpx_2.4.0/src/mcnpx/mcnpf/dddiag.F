!#ident dddiag.F from deck dg                                                   !/*dg          1*/
      subroutine dddiag(i1,iw)                                                  !/*dg          2*/
!        print detector score diagnostics for detector i1 of ital.              !/*dg4a        1*/
!        identify the detector if iw is not zero.                               !/*dg          4*/
      use global_data                                                           !/*dgx         1*/
!                                                                               !/*dgx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*dgx         3*/
      implicit integer(kindi) (i-n)                                             !/*dgx         4*/
!                                                                               !/*dgx         5*/
!                                                                               !/*dg          6*/
      write(iuo,'(1h )')                                                        !/*dg          7*/
      if(iw.ne.0)call binlin(i1,1,' ')                                          !/*dg          8*/
      do 10 id=1,ndtt                                                           !/*dg          9*/
   10 if(locdt(1,id).eq.ital)go to 20                                           !/*dg         10*/
   20 id=id+i1-1                                                                !/*dg         11*/
      if(ddg(1,id).lt.0.)write(iuo,30)'      maximum score'                     !/*dg4a        2*/
      if(ddg(1,id).ge.0.)write(iuo,30)'times average score'                     !/*dg4a        3*/
   30 format(27h detector score diagnostics,18x,10hcumulative,10x,              !/*dg4a        4*/
     & 5htally,9x,10hcumulative/45x,11hfraction of,9x,3hper,11x,                !/*dg4a        5*/
     & 11hfraction of/3x,a19,5x,13htransmissions,5x,13htransmissions,7x,        !/*dg4a        6*/
     & 7hhistory,7x,11htotal tally)                                             !/*dg4a        7*/
      t1=0.                                                                     !/*dg         18*/
      t2=0.                                                                     !/*dg         19*/
      t3=0.                                                                     !/*dg         20*/
      do 40 j=1,9                                                               !/*dg         21*/
   40 t3=t3+ddn(lddn+j,id)                                                      !/*dg         22*/
      do 50 j=1,8                                                               !/*dg         23*/
      if(t3.ne.0.)t1=t1+ddn(lddn+j,id)/t3                                       !/*dg         24*/
      if(ddn(lddn+20,id).ne.0.)t2=t2+ddn(lddn+j+9,id)/ddn(lddn+20,id)           !/*dg         25*/
      if(ddg(1,id).ge.0.)t4=talb(j,1)                                           !/*dg         26*/
      if(ddg(1,id).ge.0..and.j.eq.1)t4=ddg(1,id)                                !/*dg         27*/
      if(ddg(1,id).lt.0.)t4=-talb(j,2)*ddg(1,id)                                !/*dg         28*/
   50 write(iuo,60)t4,int(ddn(lddn+j,id)),t1,ddn(lddn+j+9,id)*fpi,t2            !/*dg         29*/
      if(ddg(1,id).lt.0.)write(iuo,60)huge,int(ddn(lddn+9,id)),1.,              !/*dg         30*/
     & ddn(lddn+18,id)*fpi,1.                                                   !/*dg         31*/
   60 format(1pe19.5,i15,0pf20.5,1pe19.5,0pf15.5)                               !/*dg         32*/
      if(ddg(1,id).ge.0.)write(iuo,70)int(ddn(lddn+9,id)),1.,                   !/*dg         33*/
     & ddn(lddn+18,id)*fpi,1.                                                   !/*dg         34*/
   70 format(19h  1st 200 histories,i15,f20.5,1pe19.5,0pf15.5)                  !/*dg         35*/
      t=1.                                                                      !/*dg         36*/
      if(ddn(lddn+20,id).ne.0.)t=ddn(lddn+21,id)/(ddn(lddn+20,id)*fpi)          !/*dg         37*/
      write(iuo,80)ddn(lddn+20,id)*fpi,ddn(lddn+21,id),t,                       !/*dg         38*/
     & int(ddn(lddn+23,id))                                                     !/*dg         39*/
   80 format(/28h average tally per history =,1pe12.5,12x,                      !/*dg         40*/
     & 15hlargest score =,e12.5/                                                !/*dg         41*/
     & 34h (largest score)/(average tally) =,e12.5,6x,                          !/*dg         42*/
     & 22hnps of largest score =,i9)                                            !/*dg         43*/
      write(iuo,90)                                                             !/*dg         44*/
   90 format(/28h score contributions by cell/8x,4hcell,4x,6hmisses,            !/*dg4a        8*/
     & 6x,4hhits,4x,17htally per history,4x,14hweight per hit)                  !/*dg4a        9*/
      j=0                                                                       !/*dg         47*/
      n=0.                                                                      !/*dg         48*/
      t=0.                                                                      !/*dg         49*/
      l=mxa*(id-1)                                                              !/*dg         50*/
      do 110 i=1,mxa                                                            !/*dg         51*/
      if(dec(ldec+3,l+i).eq.0.)go to 110                                        !/*dg         52*/
      j=j+dec(ldec+3,l+i)                                                       !/*dg         53*/
      n=n+dec(ldec+1,l+i)                                                       !/*dg         54*/
      t=t+dec(ldec+2,l+i)                                                       !/*dg         55*/
      write(iuo,100)i,ncl(lncl+i),int(dec(ldec+3,l+i)-dec(ldec+1,l+i)),         !/*dg         56*/
     & int(dec(ldec+1,l+i)),dec(ldec+2,l+i)*fpi,                                !/*dg         57*/
     & dec(ldec+2,l+i)/max(dec(ldec+1,l+i),one)                                 !/*dg         58*/
  100 format(2i6,2i10,1pe18.5,e20.5)                                            !/*dg         59*/
  110 continue                                                                  !/*dg         60*/
      if(n.ne.0)write(iuo,120)j-n,n,t*fpi,t/n                                   !/*dg         61*/
  120 format(7x,5htotal,2i10,1pe18.5,e20.5)                                     !/*dg         62*/
      write(iuo,130)(ndpf(lndp+i,id),i=1,6)                                     !/*dg         63*/
  130 format(/13h score misses/25h   russian roulette on pd,i20/                !/*dg4a       10*/
     & 9h   psc=0.,i36/35h   russian roulette in transmission,i10/              !/*dg         65*/
     & 28h   underflow in transmission,i17/                                     !/*dg         66*/
     & 29h   hit a zero-importance cell,i16/16h   energy cutoff,i29)            !/*dg         67*/
      is=0                                                                      !/*dg4b        1*/
      do 140 i=1,6                                                              !/*dg4b        2*/
  140 is=is+ndpf(lndp+i,id)                                                     !/*dg4b        3*/
      if(is.ne.j-n)write(iuo,150)j-n-is                                         !/*dg4b        4*/
  150 format(35h   other: outside tally bin bounds.,i10)                        !/*dg4b        5*/
      return                                                                    !/*dg         68*/
      end                                                                       !/*dg         69*/
