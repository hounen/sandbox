!#ident avrxyz.F from deck vx4c                                                 !/*vx4c        1*/
      subroutine avrxyz(rc,km,df,no)                                            !/*vxx         1*/
!        mesh-based weight window generator.  rc = coordinate point.            !/*vx4c        3*/
!        search mesh indices asm() to find km() axis mesh indices.              !/*vx4c        4*/
!        df = -one to check if coordinate is in the mesh.                       !/*vx4c        5*/
      use global_data                                                           !/*vxx         2*/
!                                                                               !/*vxx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*vxx         4*/
      implicit integer(kindi) (i-n)                                             !/*vxx         5*/
!                                                                               !/*vxx         6*/
      dimension rc(3)                                                           !/*vx4c        7*/
      dimension km(3)                                                           !/*vx4c        8*/
      character cx*5                                                            !/*vx4c        9*/
!                                                                               !/*vx4c       10*/
      no=0                                                                      !/*vx4c       11*/
      do 30 i=1,3                                                               !/*vx4c       12*/
      ia=lasm+i                                                                 !/*vx4c       13*/
      r=rc(i)                                                                   !/*vx4c       14*/
      if(r.lt.asm(ia,1).or.r.gt.asm(ia,ism(i)))no=1                             !/*vx4c       15*/
      il=1                                                                      !/*vx4c       16*/
      iu=ism(i)                                                                 !/*vx4c       17*/
   10 if(iu-il.eq.1)go to 30                                                    !/*vx4c       18*/
      ih=(iu+il)/2                                                              !/*vx4c       19*/
      if(r.lt.asm(ia,ih))go to 20                                               !/*vx4c       20*/
      il=ih                                                                     !/*vx4c       21*/
      go to 10                                                                  !/*vx4c       22*/
   20 iu=ih                                                                     !/*vx4c       23*/
      go to 10                                                                  !/*vx4c       24*/
   30 km(i)=iu                                                                  !/*vx4c       25*/
      if(no.eq.0)return                                                         !/*vx4c       26*/
!                                                                               !/*vx4c       27*/
!        particle outside the mesh.                                             !/*vx4c       28*/
      if(df.ge.0.)return                                                        !/*vx4c       29*/
      if(mai.eq.0)go to 40                                                      !/*vx4c       30*/
!                                                                               !/*vx4c       31*/
!        mai > 0 --- called from avrwwg (mcrun).  use errprn and count.         !/*vx4c       32*/
      cx='great'                                                                !/*vx4c       33*/
      if(r.lt.asm(ia,1))cx='small'                                              !/*vx4c       34*/
      call errprn(1,nsom,0,zero,zero,' ',' ','track started outside '//         !/*vx4c       35*/
     & 'wwg mesh: '//avrm(3*(nwgeoa-1)+i)//' too '//cx//'.')                    !/*vx4c       36*/
      return                                                                    !/*vx4c       37*/
!                                                                               !/*vx4c       38*/
!        mai = 0 --- called from avrwgi (imcn).  use erprnt.  fatal.            !/*vx4c       39*/
   40 call erprnt(1,1,0,0,0,0,0,1,                                              !/*vx4c       40*/
     & '42hreference point outside superimposed mesh.')                         !/*vx4c       41*/
      return                                                                    !/*vx4c       42*/
      end                                                                       !/*vx4c       43*/
