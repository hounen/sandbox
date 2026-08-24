!#ident crspro.F from deck cr                                                   !/*cr          1*/
      subroutine crspro(v1,v2,v3)                                               !/*cr          2*/
!        calculate the cross product   v3 = v1 x v2                             !/*cr          3*/
      use global_parameters                                                     !/*crx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*crx         2*/
      implicit integer(kindi) (i-n)                                             !/*crx         3*/
      dimension v1(3),v2(3),v3(3)                                               !/*cr          6*/
!                                                                               !/*cr          7*/
      v3(1)=v1(2)*v2(3)-v1(3)*v2(2)                                             !/*cr          8*/
      v3(2)=v1(3)*v2(1)-v1(1)*v2(3)                                             !/*cr          9*/
      v3(3)=v1(1)*v2(2)-v1(2)*v2(1)                                             !/*cr         10*/
      return                                                                    !/*cr         11*/
      end                                                                       !/*cr         12*/
