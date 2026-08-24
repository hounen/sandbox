!#ident avrclc.F from deck vy4c                                                 !/*vy4c        1*/
      subroutine avrclc(tw)                                                     !/*vy4c        2*/
!        mesh-based weight window generator.                                    !/*vy4c        3*/
!        sum up scored weight (tw) from a scoring particle track.               !/*vy4c        4*/
      use global_data                                                           !/*vyx         1*/
!                                                                               !/*vyx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*vyx         3*/
      implicit integer(kindi) (i-n)                                             !/*vyx         4*/
!                                                                               !/*vyx         5*/
!                                                                               !/*vy4c        6*/
      do 10 i=1,nmc                                                             !/*vy4c        7*/
      ip=nmcp(knmc+1,i)                                                         !/*vy4c        8*/
      l=nmcp(knmc+3,i)                                                          !/*vy4c        9*/
      i1=l+nwwma*(nmcp(knmc+2,i)-1+mgww(ip))                                    !/*vy4c       10*/
      i2=l+nwwma*(ip-1+mgww(mipt+1))                                            !/*vy4c       11*/
      swwfa(ksww+i1)=swwfa(ksww+i1)+tw                                          !/*vy4c       12*/
   10 if(nmcp(knmc+4,i).eq.1)swwfa(ksww+i2)=swwfa(ksww+i2)+tw                   !/*vy4c       13*/
      return                                                                    !/*vy4c       14*/
      end                                                                       !/*vy4c       15*/
