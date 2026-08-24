!#ident chrhol.F from deck cu                                                   !/*cu          1*/
      subroutine chrhol(h,m,k)                                                  !/*cu          2*/
!        use a non-standard hardware-dependent a-format read statement          !/*cu          3*/
!        to move up to 80 characters from character entity h into               !/*cu          4*/
!        integer array m which must have room for 80 characters.                !/*cu          5*/
!        if k.ne.0, a $ is inserted after the last non-blank character.         !/*cu          6*/
      use global_parameters                                                     !/*cux         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*cux         2*/
      implicit integer(kindi) (i-n)                                             !/*cux         3*/
      character h*(*),ht*80                                                     !/*cu          7*/
      dimension m(*)                                                            !/*cu          8*/
!                                                                               !/*cu          9*/
      ht=h                                                                      !/*cu         10*/
      if(k.ne.0)ht(min(80,leng(ht)+1):80)='$'                                   !/*cu         11*/
      if(kindi.eq.4)read(ht,'(20a4)')(m(i),i=1,20)                              !/*cux         4*/
      if(kindi.ne.4)read(ht,'(10a8)')(m(i),i=1,10)                              !/*cux         5*/
      return                                                                    !/*cu         18*/
      end                                                                       !/*cu         19*/
