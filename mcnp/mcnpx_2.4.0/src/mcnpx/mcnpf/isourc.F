!#ident isourc.F from deck is                                                   !/*is          1*/
      subroutine isourc                                                         !/*is          2*/
!        process the source specifications.                                     !/*is          3*/
      use global_data                                                           !/*isx         1*/
      use imcn_data                                                             !/*isx         2*/
!                                                                               !/*isx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*isx         4*/
      implicit integer(kindi) (i-n)                                             !/*isx         5*/
!                                                                               !/*isx         6*/
      character hq*80,hf*9                                                      !/*is4c2       1*/
!                                                                               !/*is          6*/
#ifdef PCDOS                                                                    /*!is4a        1*/
      if(lockl)call pttyin                                                      !/*is4a        2*/
#endif                                                                          /*!cpp       116*/
      if(irup.ne.0)call qttyin(0,'25hisourc source processing.')                !/*is4a        3*/
      if(nsr.ne.71.or.ksdef.ne.0)call jsourc                                    !/*is4c        1*/
      if(nsr.eq.6.and.nrcd.ne.0)call issrc                                      !/*cor4-2    118*/
      if(nsr.eq.71.and.(nsrc.gt.0.or.ksdef.gt.0.or.jovr(4).ne.0))               !/*is4a        4*/
     & call skcode                                                              !/*is4a        5*/
      rngs=92121400080171.                                                      !/*isxl        1*/
!                                                                               !/*isxc        2*/
!        check for existence of required TR cards.                              !/*isxc        3*/
      call check_tr                                                             !/*isxc        4*/
!                                                                               !/*is4c2       3*/
!        surface source write.                                                  !/*is4c2       4*/
      if(issw.eq.0.or.ink(10).eq.0)return                                       !/*is4c2       5*/
      if(nsph.eq.1.or.njss.eq.0)go to 60                                        !/*is4c2       6*/
      write(iuo,10)wssa                                                         !/*is4c2       7*/
   10 format(///21h surface-source file ,a8,20h will be written for)            !/*is4c2       8*/
      kq=0                                                                      !/*is4c2       9*/
      do 20 i=1,njss                                                            !/*is4c2      10*/
   20 if(jsq(ljsq+i).ne.0)kq=1                                                  !/*is4c2      11*/
!                                                                               !/*is4c2      12*/
!        print surfaces if there are no facets.                                 !/*is4c2      13*/
      if(kq.eq.0)write(iuo,30)(jss(ljss+i),i=1,njss)                            !/*is4c2      14*/
   30 format(5x,10hsurface(s),14i7:/(15x,14i7))                                 !/*is4c2      15*/
      if(kq.eq.0)go to 60                                                       !/*is4c2      16*/
!                                                                               !/*is4c2      17*/
!        print surfaces with facets.                                            !/*is4c2      18*/
      do 50 i=1,njss                                                            !/*is4c2      19*/
      n=mod(i-1,8)+1                                                            !/*is4c2      20*/
      if(n.eq.1)hq=' '                                                          !/*is4c2      21*/
      hf=' '                                                                    !/*is4c2      22*/
      if(jsq(ljsq+i).eq.0)write(hf,'(i7,2h  )')jss(ljss+i)                      !/*is4c2      23*/
      if(jsq(ljsq+i).ne.0)write(hf,'(i7,1h.,i1)')jss(ljss+i),jsq(ljsq+i)        !/*is4c2      24*/
      hq(1+(n-1)*9:n*9)=hf(1:9)                                                 !/*is4c2      25*/
      if(i.le.8.and.(n.eq.8.or.i.eq.njss))write(iuo,40)hq                       !/*is4c2      26*/
   40 format(5x,10hsurface(s),a80)                                              !/*is4c2      27*/
      if(i.gt.8.and.(n.eq.8.or.i.eq.njss))write(iuo,'(15x,a80)')hq              !/*is4c2      28*/
   50 continue                                                                  !/*is4c2      29*/
!                                                                               !/*is4c2      30*/
!        print cells of volume distributed fission source.                      !/*is4c2      31*/
   60 if(nilw.ne.0)write(iuo,70)(jss(ljss+i),i=njss+1,njss+nilw)                !/*is4c2      32*/
   70 format(5x,7hcell(s),14i7:/(7x,14i7))                                      !/*is4c2      33*/
!                                                                               !/*is4c2      34*/
!        print surfaces of spherical surface source.                            !/*is4c2      35*/
      if(nsph.eq.1)write(iuo,80)wssa,jss(ljss+1)                                !/*is4c2      36*/
   80 format(///21h surface-source file ,a8,                                    !/*is4c2      37*/
     & 38h will be written for spherical surface,i7)                            !/*is4c2      38*/
      return                                                                    !/*is         21*/
      end                                                                       !/*is         22*/
