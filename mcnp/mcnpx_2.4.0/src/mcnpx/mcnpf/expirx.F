!#ident expirx.F from deck xx                                                   !/*xx          1*/
      subroutine expirx(mm,hs,hm)                                               !/*xx          2*/
!        save expire arguments for call expire in output.                       !/*xx          3*/
      use global_data                                                           !/*xxx         1*/
!                                                                               !/*xxx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*xxx         3*/
      implicit integer(kindi) (i-n)                                             !/*xxx         4*/
!                                                                               !/*xxx         5*/
      character hs*(*),hm*(*)                                                   !/*xx          5*/
!                                                                               !/*xx          6*/
      if(ntasks.le.1.and.ltasks.le.1)call expire(mm,hs,hm)                      !/*xx4c        1*/
#ifdef MULTT                                                                    /*!xxxk        1*/
      call thread_lockon(jlock)                                                 !/*xxxk        2*/
#endif                                                                          /*!xxxk        3*/
      kdbnps=npstc                                                              !/*xx          9*/
      rnrtc=rnrtc0                                                              !/*xx         10*/
      kdb=-30                                                                   !/*xx         11*/
      mmkdb=mm                                                                  !/*xx         12*/
      hsub=hs                                                                   !/*xx         13*/
      hmes=hm                                                                   !/*xx         14*/
#ifdef MULTT                                                                    /*!xxxk        4*/
      call thread_lockoff(jlock)                                                !/*xxxk        5*/
#endif                                                                          /*!xxxk        6*/
      return                                                                    !/*xx         15*/
      end                                                                       !/*xx         16*/
