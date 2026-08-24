!#ident ffetch.F from deck ff                                                   !/*ff          1*/
      subroutine ffetch(hf,hr)                                                  !/*ff          2*/
!        fetch file hf by the route hr.                                         !/*ff          3*/
      use global_data                                                           !/*ffx         1*/
!                                                                               !/*ffx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ffx         3*/
      implicit integer(kindi) (i-n)                                             !/*ffx         4*/
!                                                                               !/*ffx         5*/
      character hf*64,hr*70,ht*10                                               !/*ff4c        1*/
!                                                                               !/*ff          6*/
#ifdef PCDOS                                                                    /*!ff4a        1*/
      if(lockl)call pttyin                                                      !/*ff4a        2*/
#endif                                                                          /*!cpp       185*/
      if(irup.ne.0)call qttyin(0,'27hfetching xsec file '//hf(1:8))             !/*ff4a        3*/
#ifdef UNICOS                                                                   /*!ff         18*/
      if(hr.eq.' '.or.hr.eq.'0')return                                          !/*ff         19*/
      ie=ishell('cfs get '//hf(1:leng(hf))//':'//hr)                            !/*ff         20*/
      write(ht,'(i10)')ie                                                       !/*ff         21*/
      if(ie.ne.0)call expire(0,'ffetch',                                        !/*ff         22*/
     & 'error='//ht//'   cfs unable to get '//hf(1:32))                         !/*ff         23*/
#endif                                                                          /*!cpp       186*/
      return                                                                    !/*ff         25*/
      end                                                                       !/*ff         26*/
