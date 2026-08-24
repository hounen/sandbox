!#ident wrwssa.F from deck ws                                                   !/*ws          1*/
      subroutine wrwssa                                                         !/*ws          2*/
!        create wssa, write header, and copy records from scratch file.         !/*ws          3*/
      use global_data                                                           !/*wsx         1*/
!                                                                               !/*wsx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*wsx         3*/
      implicit integer(kindi) (i-n)                                             !/*wsx         4*/
!                                                                               !/*wsx         5*/
      dimension rs(6),rg(11)                                                    !/*ws4b        1*/
      character hq*80,hf*9                                                      !/*ws4c2       1*/
!                                                                               !/*ws          6*/
!        create wssa and write general information and table lengths.           !/*ws          7*/
      call unique(wssa,jtty)                                                    !/*ws          8*/
      open(iusw,file=wssa,status='new',form='unformatted')                      !/*ws          9*/
      write(iusw)kod,ver,loddat,idtm,probid,aid,knod                            !/*ws         10*/
      np=nps                                                                    !/*ws         11*/
      nd=11                                                                     !/*ws4b        2*/
      if(nsph.eq.1)nd=6                                                         !/*ws4b        3*/
      if(nsr.eq.6.and.nrrs.ge.nrss)np=np1                                       !/*ws4c        1*/
      if(nsr.eq.6.and.nrrs.lt.nrss)np=npsr                                      !/*ws4c        2*/
      if(nsr.eq.71)np=pax(1,1,1)                                                !/*ws4c        3*/
      write(iusw)-np,nrsw,nd,njss,nqsw                                          !/*ws         17*/
      kq=0                                                                      !/*ws4c2       2*/
      do 10 i=1,njss                                                            !/*ws4c2       3*/
   10 if(jsq(ljsq+i).ne.0)kq=1                                                  !/*ws4c2       4*/
      write(iusw)nilw,mipt,kq,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0                 !/*ws4c2       5*/
!                                                                               !/*ws         19*/
!        write surface, cell, and summary information.                          !/*ws         20*/
      do 15 j=1,njss                                                            !/*ws4c2       6*/
      jx=namchg(2,abs(jss(ljss+j)))+jsq(ljsq+j)                                 !/*ws4c2       7*/
      n=lsc(llsc+jx+1)-lsc(llsc+jx)                                             !/*ws4c2       8*/
      if(kq.eq.0)write(iusw)jss(ljss+j),kst(lkst+jx),n,                         !/*ws4c2       9*/
     & (scf(lsc(llsc+jx)+i),i=1,n)                                              !/*ws4c2      10*/
   15 if(kq.eq.1)write(iusw)jss(ljss+j),jsq(ljsq+j),kst(lkst+jx),n,             !/*ws4c2      11*/
     & (scf(lsc(llsc+jx)+i),i=1,n)                                              !/*ws         25*/
      do 20 j=njss+1,njss+nilw                                                  !/*ws         26*/
   20 write(iusw)jss(ljss+j),0,1,zero                                           !/*cor4-2    210*/
      write(iusw)zero,((nsl(lnsl+i,j),i=1,2+4*mipt),j=1,njss+nilw)              !/*ws         28*/
!                                                                               !/*ws         29*/
!        copy all track information from the scratch file.                      !/*ws         30*/
!ccc    machinery is needed here to reorder the scratch file in the             !/*ws         31*/
!ccc    multitasking case.                                                      !/*ws         32*/
      rewind iusc                                                               !/*ws         33*/
      read(iusc)id                                                              !/*ws4a        2*/
      read(iusc)id                                                              !/*ws4a        3*/
      if(nsph.eq.1)go to 40                                                     !/*ws4b        4*/
      do 30 i=1,nrsw                                                            !/*ws         37*/
      read(iusc,end=110)rg                                                      !/*ws         38*/
   30 write(iusw)rg                                                             !/*ws         39*/
      go to 60                                                                  !/*ws         40*/
   40 do 50 i=1,nrsw                                                            !/*ws         41*/
      read(iusc,end=110)rs                                                      !/*ws         42*/
   50 write(iusw)rs                                                             !/*ws         43*/
   60 rewind iusw                                                               !/*ws         44*/
      write(jtty,70)wssa,nrsw                                                   !/*ws         45*/
   70 format(21h surface-source file ,a8,13h written with,i8,8h tracks.)        !/*ws         46*/
      write(iuo,80)wssa,np,nrsw                                                 !/*ws         47*/
   80 format(21h surface-source file ,a8,11h with nps =,i7,4h and,i8,           !/*ws         48*/
     & 23h tracks was written for)                                              !/*ws         49*/
!                                                                               !/*ws4c2      12*/
!        write surfaces without facets.                                         !/*ws4c2      13*/
      if(njss.eq.0)go to 92                                                     !/*ws4c2      14*/
      if(kq.eq.0)write(iuo,84)(jss(ljss+i),i=1,njss)                            !/*ws4c2      15*/
   84 format(5x,10hsurface(s),14i7/(15x,14i7))                                  !/*ws4c2      16*/
      if(kq.eq.0)go to 92                                                       !/*ws4c2      17*/
!                                                                               !/*ws4c2      18*/
!        write surfaces with facets.                                            !/*ws4c2      19*/
      do 90 i=1,njss                                                            !/*ws4c2      20*/
      n=mod(i-1,8)+1                                                            !/*ws4c2      21*/
      if(n.eq.1)hq=' '                                                          !/*ws4c2      22*/
      hf=' '                                                                    !/*ws4c2      23*/
      if(jsq(ljsq+i).eq.0)write(hf,'(i7,2h  )')jss(ljss+i)                      !/*ws4c2      24*/
      if(jsq(ljsq+i).ne.0)write(hf,'(i7,1h.,i1)')jss(ljss+i),jsq(ljsq+i)        !/*ws4c2      25*/
      hq(1+(n-1)*9:n*9)=hf(1:9)                                                 !/*ws4c2      26*/
      if(i.le.8.and.(n.eq.8.or.i.eq.njss))write(iuo,88)hq                       !/*ws4c2      27*/
   88 format(5x,10hsurface(s),a80)                                              !/*ws4c2      28*/
      if(i.gt.8.and.(n.eq.8.or.i.eq.njss))write(iuo,'(15x,a80)')hq              !/*ws4c2      29*/
   90 continue                                                                  !/*ws4c2      30*/
!                                                                               !/*ws4c2      31*/
!        write fission volume criticality source.                               !/*ws4c2      32*/
   92 if(nilw.ne.0)write(iuo,100)(jss(ljss+i),i=njss+1,njss+nilw)               !/*ws4c2      33*/
  100 format(5x,7hcell(s),14i7/(7x,14i7))                                       !/*ws4c        5*/
      close(iusc,status='delete')                                               !/*ws         54*/
      return                                                                    !/*ws         55*/
!                                                                               !/*ws         56*/
!        quit on unexpected eof on the scratch file.                            !/*ws         57*/
  110 call expire(0,'wrwssa',                                                   !/*ws         58*/
     & 'unexpected eof on the surface-source scratch file.')                    !/*ws         59*/
      end                                                                       !/*ws         60*/
