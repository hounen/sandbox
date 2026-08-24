!#ident getpar.F from deck qr4c2                                                !/*qr4c2       1*/
      subroutine getpar(k1,k2)                                                  !/*qr4c2       2*/
!        retrieve the current particle attributes from particle stack.          !/*qr4c2       3*/
!           k1 = stack index for particle attributes                            !/*qr4c2       4*/
!              = 0 to increment and use npb                                     !/*qr4c2       5*/
!           k2 = udt (repeated structures) stack index                          !/*qr4c2       6*/
!              = 0 to not save repeated structures data.                        !/*qr4c2       7*/
      use global_data                                                           !/*qrx         1*/
!                                                                               !/*qrx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*qrx         3*/
      implicit integer(kindi) (i-n)                                             !/*qrx         4*/
!                                                                               !/*qrx         5*/
      n=k1                                                                      !/*qr4c2       9*/
      if(k1.eq.0)n=npb                                                          !/*qr4c2      10*/
      if(k1.eq.0)npb=npb-1                                                      !/*qr4c2      11*/
!                                                                               !/*qr4c2      12*/
!        retrieve the integer particle attributes.                              !/*qr4c2      13*/
      do 10 i=1,lpblcm                                                          !/*qr4c2      14*/
   10 jpblcm(i)=jpb9cm(n,i)                                                     !/*qr4c2      15*/
!                                                                               !/*qr4c2      16*/
!        retrieve the real particle attributes.                                 !/*qr4c2      17*/
      do 20 i=1,npblcm                                                          !/*qr4c2      18*/
   20 gpblcm(i)=gpb9cm(n,i)                                                     !/*qr4c2      19*/
!                                                                               !/*qr4c2      20*/
!        retrieve the repeated structures level information.                    !/*qr4c2      21*/
      if(k2.eq.0.or.lev.eq.0)return                                             !/*qr4c2      22*/
      do 30 j=0,lev                                                             !/*qr4c2      23*/
      do 30 i=1,10                                                              !/*qr4c2      24*/
   30 udt(i,j)=udtsav(k2,10*j+i)                                                !/*qr4c2      25*/
      return                                                                    !/*qr4c2      26*/
      end                                                                       !/*qr4c2      27*/
