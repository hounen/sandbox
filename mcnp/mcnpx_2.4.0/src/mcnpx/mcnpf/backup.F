!#ident backup.F from deck bu                                                   !/*bu          1*/
      subroutine backup                                                         !/*bu          2*/
!        back up to history beginning to rerun it. nonmultitasking only.        !/*bu          3*/
      use global_data                                                           !/*bux         1*/
!                                                                               !/*bux         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*bux         3*/
      implicit integer(kindi) (i-n)                                             !/*bux         4*/
!                                                                               !/*bux         5*/
!                                                                               !/*bu          5*/
      rnrtc=rnrtc0                                                              !/*bu          6*/
      krflg=2                                                                   !/*bu          7*/
      ntii=0                                                                    !/*bu          8*/
!                                                                               !/*bu          9*/
!        backup kcode.                                                          !/*bu         10*/
      if(nsr.ne.71)go to 10                                                     !/*bu         11*/
      ist=ist0                                                                  !/*bu         12*/
      ixak=ixak0                                                                !/*bu         13*/
      nsa=nsa0                                                                  !/*bu         14*/
      nss=nss0                                                                  !/*bu         15*/
!                                                                               !/*bu         16*/
!        backspace surface source read file.                                    !/*bu         17*/
   10 if(nsr.ne.6)return                                                        !/*bu         18*/
      backspace iusr                                                            !/*bu         19*/
   20 backspace iusr                                                            !/*bu         20*/
      backspace iusr                                                            !/*bu         21*/
      nrrs=nrrs-1                                                               !/*bu         22*/
      read(iusr)a                                                               !/*bu         23*/
      if(npsr.eq.abs(a))go to 20                                                !/*bu         24*/
      read(iusr)(ssb(i),i=1,nrcd)                                               !/*bu         25*/
      nqss=nqss-1                                                               !/*bu         26*/
      return                                                                    !/*bu         27*/
      end                                                                       !/*bu         28*/
