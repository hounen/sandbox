!#ident mbodyo.F from deck mbo4c2                                               !/*mbo4c2      1*/
      subroutine mbodyo(mm,k2,k3,k4,k5,ab,ac,sc)                                !/*mbo4c2      2*/
!        mm=1:  check orthogonality of vectors k2 and k3, adjust k3.            !/*mbo4c2      3*/
!        mm=2:  check cross product of k2 and k3 as k4, compare to              !/*mbo4c2      4*/
!               k5 and adjust k5.                                               !/*mbo4c2      5*/
      use global_parameters                                                     !/*mbox        1*/
      implicit real(kindr) (a-h,o-z)                                            !/*mbox        2*/
      implicit integer(kindi) (i-n)                                             !/*mbox        3*/
      dimension sc(33)                                                          !/*mbo4c2      8*/
!                                                                               !/*mbo4c2      9*/
      if(mm.eq.2)go to 20                                                       !/*mbo4c2     10*/
!                                                                               !/*mbo4c2     11*/
!        check for orthogonal vectors sc(k2) and sc(k3)                         !/*mbo4c2     12*/
      ab=sc(k2)*sc(k3)+sc(k2+1)*sc(k3+1)+sc(k2+2)*sc(k3+2)                      !/*mbo4c2     13*/
      if(ab.eq.0.)return                                                        !/*mbo4c2     14*/
      if(abs(ab).gt..01)call erprnt(2,1,0,0,0,0,0,0,                            !/*mbo4c2     15*/
     & '22hvector not orthogonal.')                                             !/*mbo4c2     16*/
      if(abs(ab).gt..01)return                                                  !/*mbo4c2     17*/
!        adjust slightly to be orthogonal                                       !/*mbo4c2     18*/
      rb=sqrt(1.-ab**2)                                                         !/*mbo4c2     19*/
      do 10 kk=1,3                                                              !/*mbo4c2     20*/
   10 sc(k3+kk-1)=(sc(k3+kk-1)-sc(k2+kk-1)*ab)/rb                               !/*mbo4c2     21*/
      return                                                                    !/*mbo4c2     22*/
!                                                                               !/*mbo4c2     23*/
!        find vector k4 as cross product k2&k3, compare to k5                   !/*mbo4c2     24*/
   20 call crspro(sc(k2),sc(k3),sc(k4))                                         !/*mbo4c2     25*/
      ac=sc(k5)*sc(k4)+sc(k5+1)*sc(k4+1)+sc(k5+2)*sc(k4+2)                      !/*mbo4c2     26*/
      if(abs(ac).lt..99995)call erprnt(2,1,0,0,0,0,0,0,                         !/*mbo4c2     27*/
     & '28hthird vector not orthogonal.')                                       !/*mbo4c2     28*/
      if(abs(ac).lt..99995)return                                               !/*mbo4c2     29*/
      do 30 kk=1,3                                                              !/*mbo4c2     30*/
      if(ac.gt.0.)sc(k5+kk-1)=sc(k4+kk-1)                                       !/*mbo4c2     31*/
   30 if(ac.le.0.)sc(k5+kk-1)=-sc(k4+kk-1)                                      !/*mbo4c2     32*/
      return                                                                    !/*mbo4c2     33*/
      end                                                                       !/*mbo4c2     34*/
