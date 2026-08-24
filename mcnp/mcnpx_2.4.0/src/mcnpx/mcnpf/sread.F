!#ident sread.F from deck sr                                                    !/*sr          1*/
      subroutine sread(nr,iz,hk,hm,hd,hc,lp)                                    !/*sr          2*/
!        read the cross-section table described by hc.                          !/*sr          3*/
      use global_data                                                           !/*srx         1*/
!                                                                               !/*srx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*srx         3*/
      implicit integer(kindi) (i-n)                                             !/*srx         4*/
!                                                                               !/*srx         5*/
      character hf*64,hz*10,hk*70,hm*10,hd*10,ht*10,hc*181                      !/*sr          5*/
      dimension ly(5),iz(0:16),aw(0:16)                                         !/*sr          6*/
      real(kindr), allocatable :: x(:)                                          !/*srxc        1*/
!                                                                               !/*sr          7*/
!        set up the file and record characteristics.                            !/*sr          8*/
      hz=' '                                                                    !/*sr          9*/
      iu=0                                                                      !/*sr         10*/
      do 10 i=1,9                                                               !/*sr         11*/
      call nxtsym(hc,' ',iu+1,it,iu,1)                                          !/*sr4a        1*/
      if(iu.eq.0)go to 20                                                       !/*sr         13*/
      if(i.eq.3)hf=hc(it:iu)                                                    !/*sr         14*/
      if(i.ge.5)ht=hc(it:iu)                                                    !/*sr         15*/
   10 if(i.ge.5)read(ht,'(bn,i10)')ly(i-4)                                      !/*sr         16*/
   20 call zaid(2,ht,ixl(lixl+1,iex))                                           !/*sr         17*/
#ifdef PCDOS                                                                    /*!sr4a        2*/
      if(lockl)call pttyin                                                      !/*sr4a        3*/
#endif                                                                          /*!cpp       187*/
      if(irup.ne.0)call qttyin(0,'26hreading nuclide '//ht)                     !/*sr4a        4*/
!                                                                               !/*sr         18*/
      if(ly(1).eq.2)go to 80                                                    !/*sr4b        1*/
!                                                                               !/*sr         20*/
! >>>>>  ly(1)=1 -- standard portable formatted file.                           !/*sr         21*/
   30 if(ly(2).ge.nr)go to 40                                                   !/*sr         22*/
      nr=1                                                                      !/*sr         23*/
      rewind iux                                                                !/*sr         24*/
   40 do 50 i=1,ly(2)-nr                                                        !/*sr         25*/
   50 read(iux,'(a10)')hz                                                       !/*sr4a        5*/
      read(iux,'(a10,2e12.0,1x,a10)',err=200)hz,aw(0),tz,hd                     !/*sr         27*/
      call nxtsym(hz,' ',1,i,j,2)                                               !/*sr4a        6*/
      if(hz.ne.ht)go to 200                                                     !/*sr         28*/
      read(iux,60,err=200)hk,hm,(iz(i),aw(i),i=1,16),(nxs(lnxs+i,iex),          !/*sr         29*/
     & i=1,16),(jxs(ljxs+i,iex),i=1,32)                                         !/*sr         30*/
   60 format(a70,a10/4(i7,f11.0)/4(i7,f11.0)/4(i7,f11.0)/4(i7,f11.0)/           !/*sr         31*/
     & 8i9/8i9/8i9/8i9/8i9/8i9)                                                 !/*sr         32*/
      if(nty(lnty+iex).ne.10)read(iux,70,err=200)(xss(lp+i),i=1,ly(3))          !/*srx         6*/
      if(nty(lnty+iex).eq.10)read(iux,70,err=200)(exs(lp+i),i=1,ly(3))          !/*srx         7*/
   70 format(4e20.0)                                                            !/*sr         35*/
      nr=ly(2)+12+(ly(3)+3)/4                                                   !/*sr         36*/
      go to 180                                                                 !/*sr         37*/
!                                                                               !/*sr         38*/
! >>>>>  ly(1)=2 -- standard unformatted file.                                  !/*sr         39*/
   80 read(iux,rec=ly(2),err=200)hz,aw(0),tz,hd,hk,hm,(iz(i),aw(i),             !/*sr         40*/
     & i=1,16),(nxs(lnxs+i,iex),i=1,16),(jxs(ljxs+i,iex),i=1,32)                !/*sr         41*/
      call nxtsym(hz,' ',1,i,j,2)                                               !/*sr4a        7*/
      if(hz.ne.ht)go to 200                                                     !/*sr         42*/
      k=nty(lnty+iex)                                                           !/*sr         43*/
      do 90 i=1,(ly(3)+ly(5)-1)/ly(5)                                           !/*sr         44*/
      j1=lp+1+(i-1)*ly(5)                                                       !/*sr         45*/
      j2=min(lp+ly(3),j1+ly(5)-1)                                               !/*sr         46*/
      allocate (x(j2-j1+1), stat=is)                                            !/*srxc        2*/
      if (is.ne.0) call expire(0,'sread','allocate error')                      !/*srxc        3*/
      read(iux,rec=ly(2)+i,err=200)x                                            !/*srxc        4*/
      if(k.ne.10) then                                                          !/*srxc        5*/
        xss(j1:j2)=x(:)                                                         !/*srxc        6*/
      else                                                                      !/*srxc        7*/
        exs(j1:j2)=x(:)                                                         !/*srxc        8*/
      endif                                                                     !/*srxc        9*/
      deallocate(x)                                                             !/*srxc       10*/
   90 continue                                                                  !/*srxc       11*/
!                                                                               !/*sr4c        1*/
!        check if library and xsdir temperatures different.                     !/*sr4c        2*/
  180 if(nty(lnty+iex).lt.3.and.abs(tbt(ltbt+iex)-tz).gt.                       !/*sr4c        3*/
     & .005*(tbt(ltbt+iex)+tz))call erprnt(1,1,0,0,0,0,0,0,                     !/*sr4c        4*/
     & '51hxsdir and library temperatures disagree: '//ht)                      !/*sr4c        5*/
!                                                                               !/*sr         95*/
!        check that the dates match.                                            !/*sr         96*/
      m=0                                                                       !/*sr4c        6*/
      do 190 i=1,10                                                             !/*sr         98*/
      n=index('0123456789/',hd(i:i))                                            !/*sr         99*/
  190 if(n.ne.0)m=11*m+n                                                        !/*sr        100*/
      if(m.eq.kxd(lkxd+iex).or.kxd(lkxd+iex).eq.0)return                        !/*sr        101*/
!                                                                               !/*sr        102*/
  200 call expire(0,'sread',                                                    !/*sr        103*/
     & 'error in '//ht//' of cross-section file '//hf(1:27))                    !/*sr        104*/
      return                                                                    !/*sr        105*/
      end                                                                       !/*sr        106*/
