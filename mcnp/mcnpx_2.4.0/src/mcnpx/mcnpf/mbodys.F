!#ident mbodys.F from deck mbs4c2                                               !/*mbs4c2      1*/
      subroutine mbodys(mm,m,mq,a,b,dz,q,qa,qb,sc)                              !/*mbs4c2      2*/
!        vector computations to set an sc vector for rhp.                       !/*mbs4c2      3*/
!        mm controls vector set.                                                !/*mbs4c2      4*/
      use global_parameters                                                     !/*mbsx        1*/
      implicit real(kindr) (a-h,o-z)                                            !/*mbsx        2*/
      implicit integer(kindi) (i-n)                                             !/*mbsx        3*/
      dimension sc(33)                                                          !/*mbs4c2      7*/
!                                                                               !/*mbs4c2      8*/
      j=22                                                                      !/*mbs4c2      9*/
      if(mm.eq.2)j=25                                                           !/*mbs4c2     10*/
      if(m.ne.1)go to 20                                                        !/*mbs4c2     11*/
      if(mq.eq.0)go to 10                                                       !/*mbs4c2     12*/
      sc(j+2)=dz                                                                !/*mbs4c2     13*/
      sc(j+1)=(-b+q)/(2.*a)                                                     !/*mbs4c2     14*/
      sc(j)=-(sc(17)*sc(j+1)+sc(18)*sc(j+2))/sc(16)                             !/*mbs4c2     15*/
      go to 60                                                                  !/*mbs4c2     16*/
   10 sc(j+2)=(-b+q)/(2.*a)                                                     !/*mbs4c2     17*/
      sc(j+1)=qa+qb*sc(j+2)                                                     !/*mbs4c2     18*/
      sc(j)=-(sc(17)*sc(j+1)+sc(18)*sc(j+2))/sc(16)                             !/*mbs4c2     19*/
      go to 60                                                                  !/*mbs4c2     20*/
   20 if(m.eq.3)go to 40                                                        !/*mbs4c2     21*/
      if(mq.eq.0)go to 30                                                       !/*mbs4c2     22*/
      sc(j)=dz                                                                  !/*mbs4c2     23*/
      sc(j+2)=(-b+q)/(2.*a)                                                     !/*mbs4c2     24*/
      sc(j+1)=-(sc(18)*sc(j+2)+sc(16)*sc(j))/sc(17)                             !/*mbs4c2     25*/
      go to 60                                                                  !/*mbs4c2     26*/
   30 sc(j)=(-b+q)/(2.*a)                                                       !/*mbs4c2     27*/
      sc(j+2)=qa+qb*sc(j)                                                       !/*mbs4c2     28*/
      sc(j+1)=-(sc(18)*sc(j+2)+sc(16)*sc(j))/sc(17)                             !/*mbs4c2     29*/
      go to 60                                                                  !/*mbs4c2     30*/
   40 if(mq.eq.0)go to 50                                                       !/*mbs4c2     31*/
      sc(j+1)=dz                                                                !/*mbs4c2     32*/
      sc(j)=(-b+q)/(2.*a)                                                       !/*mbs4c2     33*/
      sc(j+2)=-(sc(16)*sc(j)+sc(17)*sc(j+1))/sc(18)                             !/*mbs4c2     34*/
      go to 60                                                                  !/*mbs4c2     35*/
   50 sc(j+1)=(-b+q)/(2.*a)                                                     !/*mbs4c2     36*/
      sc(j)=qa+qb*sc(j+1)                                                       !/*mbs4c2     37*/
      sc(j+2)=-(sc(16)*sc(j)+sc(17)*sc(j+1))/sc(18)                             !/*mbs4c2     38*/
!                                                                               !/*mbs4c2     39*/
   60 return                                                                    !/*mbs4c2     40*/
      end                                                                       !/*mbs4c2     41*/
