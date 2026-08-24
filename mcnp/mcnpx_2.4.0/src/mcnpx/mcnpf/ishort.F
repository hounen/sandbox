!#ident ishort.F from deck ish4c2                                               !/*ish4c2      1*/
      subroutine ishort(n,s,l)                                                  !/*ish4c2      3*/
#ifdef PLOT                                                                     /*!ishx        1*/
!        create a left justified string from an integer.                        !/*ish4c2      4*/
!        valid integer range: -99999999 < n < 999999999                         !/*ish4c2      5*/
      character s*10,s1*9                                                       !/*ish4c2      6*/
      write(s1,'(i9)')n                                                         !/*ish4c2      7*/
      do 10 i=1,9                                                               !/*ish4c2      8*/
   10 if(s1(i:i).ne.' ')go to 20                                                !/*ish4c2      9*/
   20 s=s1(i:9)                                                                 !/*ish4c2     10*/
      l=10-i                                                                    !/*ish4c2     11*/
#endif                                                                          /*!ishx        2*/
      return                                                                    !/*ish4c2     12*/
      end                                                                       !/*ish4c2     13*/
