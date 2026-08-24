!#ident source.F from deck so                                                   !/*so          1*/
      subroutine source                                                         !/*so          2*/
!        dummy subroutine.  aborts job if source subroutine is missing.         !/*so          3*/
!        if nsr=0, subroutine source must be furnished by the user.             !/*so          4*/
!        at entrance, a random set of uuu,vvv,www has been defined.  the        !/*so          5*/
!        following variables must be defined within the subroutine:             !/*so          6*/
!        xxx,yyy,zzz,icl,jsu,erg,wgt,tme and possibly ipt,uuu,vvv,www.          !/*so          7*/
!        subroutine srcdx may also be needed.                                   !/*so          8*/
      use global_data                                                           !/*sox         1*/
!                                                                               !/*sox         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*sox         3*/
      implicit integer(kindi) (i-n)                                             !/*sox         4*/
!                                                                               !/*sox         5*/
!                                                                               !/*so         10*/
      call expirx(0,'source','you need a source subroutine.')                   !/*so         11*/
      return                                                                    !/*so         12*/
      end                                                                       !/*so         13*/
