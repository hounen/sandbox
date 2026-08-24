!#ident getidt.F from deck gi                                                   !/*gi          1*/
      subroutine getidt(hi)                                                     !/*gi          2*/
!        put the machine designator, date, and time into hi                     !/*gi          3*/
!        in the form   x mm/dd/yy hh:mm:ss                                      !/*gi          4*/
      character hi*19                                                           !/*gi          5*/
      dimension jd(3),jt(3),it(9)                                               !/*cor4-2    243*/
!                                                                               !/*gi          9*/
      hi=' '                                                                    !/*gi         10*/
      call date_and_time(values=it)                                             !/*gix         1*/
      write(hi,10)it(2),it(3),it(1)-(it(1)/100)*100,it(5),it(6),it(7)           !/*gix         2*/
   10 format(2x,i2.2,1h/,i2.2,1h/,i2.2,1x,i2.2,1h:,i2.2,1h:,i2.2)               !/*gix         3*/
      return                                                                    !/*gi         45*/
      end                                                                       !/*gi         46*/
