!#ident mb.h from comdeck mb                                                    !/*mb          1*/
!     common block for message passing subroutines.                             !/*mbx         1*/
!        variables used with pvm calls.                                         !/*mb4c        1*/
      integer*4 idbuf,idtid,iflag,ihtid,imtid,info,iptid,                       !/*mb4c        2*/
     & ispeed,itid,narch,nhost,nitem,nmsg,nsub,ntask                            !/*mb4c        3*/
      common /msgcom/  idbuf,idtid,iflag,ihtid,imtid,info,iptid,                !/*mb4c        4*/
     & ispeed,itid(512),narch,nhost,nitem,nmsg,nsub,ntask                       !/*mb4c        5*/
!        four-byte local integer values.                                        !/*mbx         2*/
      integer*4 iz(-1:9)                                                        !/*mbx         3*/
      data iz/-1,0,1,2,3,4,5,6,7,8,99999/                                       !/*mbx         4*/
      save iz                                                                   !/*mbx         5*/
!                                                                               !/*mb          3*/
! ----------------------------------------------------------------------        !/*mb          4*/
!                                                                               !/*mb          5*/
