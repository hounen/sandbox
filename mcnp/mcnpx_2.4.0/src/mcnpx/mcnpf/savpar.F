!#ident savpar.F from deck qs4c2                                                !/*qs4c2       1*/
      subroutine savpar(k1,k2)                                                  !/*qs4c2       2*/
!        save the current particle attributes to the particle stack.            !/*qs4c2       3*/
!           k1 = stack index for particle attributes;                           !/*qs4c2       4*/
!              = 0 to increment and use npb;                                    !/*qs4c2       5*/
!           k2 = udt (repeated structures) stack index;                         !/*qs4c2       6*/
!              = 0 to not save repeated structures data.                        !/*qs4c2       7*/
      use global_data                                                           !/*qsx         1*/
!                                                                               !/*qsx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*qsx         3*/
      implicit integer(kindi) (i-n)                                             !/*qsx         4*/
!                                                                               !/*qsx         5*/
      n=k1                                                                      !/*qs4c2       9*/
      if(k1.eq.0)n=npb+1                                                        !/*qs4c2      10*/
      if(n.gt.mpb)go to 40                                                      !/*qs4c2      11*/
      if(k1.eq.0)npb=n                                                          !/*qs4c2      12*/
!                                                                               !/*qs4c2      13*/
!        save the real particle attributes.                                     !/*qs4c2      14*/
      do 10 i=1,npblcm                                                          !/*qs4c2      15*/
   10 gpb9cm(n,i)=gpblcm(i)                                                     !/*qs4c2      16*/
!                                                                               !/*qs4c2      17*/
!        save the integer particle attributes.                                  !/*qs4c2      18*/
      do 20 i=1,lpblcm                                                          !/*qs4c2      19*/
   20 jpb9cm(n,i)=jpblcm(i)                                                     !/*qs4c2      20*/
!                                                                               !/*qs4c2      21*/
!        save the repeated structures level information.                        !/*qs4c2      22*/
      if(k2.eq.0.or.lev.eq.0)return                                             !/*qs4c2      23*/
      do 30 j=0,lev                                                             !/*qs4c2      24*/
      do 30 i=1,10                                                              !/*qs4c2      25*/
   30 udtsav(k2,10*j+i)=udt(i,j)                                                !/*qs4c2      26*/
      return                                                                    !/*qs4c2      27*/
!                                                                               !/*qs4c2      28*/
!        error.                                                                 !/*qs4c2      29*/
   40 call expirx(1,'savpar','overflow particle save stack.')                   !/*qs4c2      30*/
      return                                                                    !/*qs4c2      31*/
      end                                                                       !/*qs4c2      32*/
