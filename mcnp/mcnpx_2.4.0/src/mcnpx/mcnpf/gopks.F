!#ident gopks.F from deck gk                                                    !/*gk          1*/
      subroutine gopks(ie,nb)                                                   !/*gk          3*/
#ifdef GKSSIM                                                                   /*!gkx         1*/
!        perform gks function 'open gks'.                                       !/*gk          4*/
#ifdef DISSPLA                                                                  /*!gk          5*/
!        if disspla is to be used with a device interface library other         !/*gk          6*/
!        than cgs, appropriate coding must be provided in                       !/*gk4b        1*/
!        routines gopks, gopwk, gacwk, gdawk, gclrwk, and guwk.                 !/*gk          8*/
#endif                                                                          /*!cpp       356*/
      use gkssim_data                                                           !/*gkxc        1*/
!                                                                               !/*gk         10*/
#ifdef CGS                                                                      /*!gk         13*/
      call gstart                                                               !/*gk         14*/
#endif                                                                          /*!cpp       357*/
#ifdef DISSCGS                                                                  /*!gk         15*/
      call gstart                                                               !/*gk         16*/
      call libdisp                                                              !/*gk         17*/
#endif                                                                          /*!cpp       358*/
!                                                                               !/*gk         28*/
!        initialize /gkssim/.                                                   !/*gk         29*/
      als=0.                                                                    !/*gk         30*/
      chup(1)=0.                                                                !/*gk         31*/
      chup(2)=1.                                                                !/*gk         32*/
      jta(1)=0                                                                  !/*gk         33*/
      jta(2)=0                                                                  !/*gk         34*/
      kls=1                                                                     !/*gk         35*/
      lvd=0                                                                     !/*gk         36*/
      ltype=1                                                                   !/*gk         37*/
      wsf=1.                                                                    !/*gk         38*/
      icol=0                                                                    !/*gk4b        2*/
      npages=0                                                                  !/*gk4b        3*/
      psize(1)=8.5                                                              !/*gk4b        4*/
      psize(2)=11.0                                                             !/*gk4b        5*/
      psize(3)=7.5                                                              !/*gk4b        6*/
      psize(4)=10.0                                                             !/*gk4b        7*/
#endif                                                                          /*!gkx         3*/
      return                                                                    !/*gk         39*/
      end                                                                       !/*gk         40*/
