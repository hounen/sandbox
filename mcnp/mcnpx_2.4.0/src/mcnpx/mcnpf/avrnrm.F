!#ident avrnrm.F from deck vn4c                                                 !/*vn4c        1*/
      subroutine avrnrm(is)                                                     !/*vn4c        2*/
!        mesh-based weight window generator.                                    !/*vn4c        3*/
!           is=0  renormalize nmc scoring loops at beginning of history.        !/*vn4c        4*/
!           is=1  renormalize nmc scoring loops during history.                 !/*vn4c        5*/
      use global_data                                                           !/*vnx         1*/
!                                                                               !/*vnx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*vnx         3*/
      implicit integer(kindi) (i-n)                                             !/*vnx         4*/
!                                                                               !/*vnx         5*/
      if(is.gt.0)go to 20                                                       !/*vn4c        7*/
!                                                                               !/*vn4c        8*/
!        initialize nmc loop at beginning of history.                           !/*vn4c        9*/
      do 10 i=1,nmc                                                             !/*vn4c       10*/
      ip=nmcp(knmc+1,i)                                                         !/*vn4c       11*/
      k=nmcp(knmc+2,i)                                                          !/*vn4c       12*/
      l=nmcp(knmc+3,i)                                                          !/*vn4c       13*/
      i1=l+nwwma*(k-1+mgww(ip))                                                 !/*vn4c       14*/
      i2=l+nwwma*(ip-1+mgww(mipt+1))                                            !/*vn4c       15*/
      iafg(kifg+i1)=0                                                           !/*vn4c       16*/
   10 iafg(kifg+i2)=0                                                           !/*vn4c       17*/
      nmc=0                                                                     !/*vn4c       18*/
      nmco=0                                                                    !/*vn4c       19*/
      return                                                                    !/*vn4c       20*/
!                                                                               !/*vn4c       21*/
!        in-history nmc re-initialization.                                      !/*vn4c       22*/
   20 do 30 i=nmco+1,nmc                                                        !/*vn4c       23*/
      ip=nmcp(knmc+1,i)                                                         !/*vn4c       24*/
      k=nmcp(knmc+2,i)                                                          !/*vn4c       25*/
      l=nmcp(knmc+3,i)                                                          !/*vn4c       26*/
      i1=l+nwwma*(k-1+mgww(ip))                                                 !/*vn4c       27*/
      i2=l+nwwma*(ip-1+mgww(mipt+1))                                            !/*vn4c       28*/
      iafg(kifg+i1)=0                                                           !/*vn4c       29*/
   30 iafg(kifg+i2)=0                                                           !/*vn4c       30*/
      do 40 i=1,nmco                                                            !/*vn4c       31*/
      ip=nmcp(knmc+1,i)                                                         !/*vn4c       32*/
      k=nmcp(knmc+2,i)                                                          !/*vn4c       33*/
      l=nmcp(knmc+3,i)                                                          !/*vn4c       34*/
      i1=l+nwwma*(k-1+mgww(ip))                                                 !/*vn4c       35*/
      i2=l+nwwma*(ip-1+mgww(mipt+1))                                            !/*vn4c       36*/
      iafg(kifg+i1)=1                                                           !/*vn4c       37*/
   40 iafg(kifg+i2)=1                                                           !/*vn4c       38*/
      nmc=nmco                                                                  !/*vn4c       39*/
      return                                                                    !/*vn4c       40*/
      end                                                                       !/*vn4c       41*/
