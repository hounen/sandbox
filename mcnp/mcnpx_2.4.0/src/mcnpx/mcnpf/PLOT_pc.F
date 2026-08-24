!#ident PLOT_pc.F from deck mpd                                                 !/*mpd         1*/
      module plot_data                                                          !/*mpd         2*/
#ifdef PLOT                                                                     /*!mpd         3*/
!                                                                               !/*mpd         4*/
      use global_parameters                                                     !/*mpd         5*/
      implicit none                                                             !/*mpd         6*/
      save                                                                      !/*mpd         7*/
      public                                                                    !/*mpd         8*/
      integer, private :: k                                                     !/*mpd         9*/
!                                                                               !/*mpd        10*/
!***********************************************************************        !/*mpd        11*/
!        parameter variables and arrays.                                        !/*mpd        12*/
!                                                                               !/*mpd        13*/
      integer, parameter :: mclb=19                                             !/*mpd        14*/
      integer, parameter :: mplm=100                                            !/*mpd        15*/
      integer, parameter :: nkeyp=35                                            !/*mpd        16*/
      real(kindr), parameter :: res=1.d0/1500.d0                                !/*mpd        17*/
!                                                                               !/*mpd        18*/
!***********************************************************************        !/*mpd        19*/
!        integer variables and arrays.                                          !/*mpd        20*/
!                                                                               !/*mpd        21*/
      integer(kindi) :: lamx                                                    !/*mpd        22*/
      integer(kindi) :: lcoe                                                    !/*mpd        23*/
      integer(kindi) :: lcrs                                                    !/*mpd        24*/
      integer(kindi) :: ljst                                                    !/*mpd        25*/
      integer(kindi) :: lkcl                                                    !/*mpd        26*/
      integer(kindi) :: lkfm                                                    !/*mpd        27*/
      integer(kindi) :: llcl                                                    !/*mpd        28*/
      integer(kindi) :: llsg                                                    !/*mpd        29*/
      integer(kindi) :: lncs                                                    !/*mpd        30*/
      integer(kindi) :: lplb                                                    !/*mpd        31*/
      integer(kindi) :: lqmx                                                    !/*mpd        32*/
      integer(kindi) :: lzst                                                    !/*mpd        33*/
!                                                                               !/*mpd        34*/
      integer(kindi) :: icolor(mplm) = (/ (0, k=1,mplm) /)                      !/*mpd        35*/
      integer(kindi) :: icurs                                                   !/*mpd        36*/
      integer(kindi) :: icurs1 = 0                                              !/*mpd        37*/
      integer(kindi) :: iqc                                                     !/*mpd        38*/
      integer(kindi) :: jloc = 0                                                !/*mpd        39*/
      integer(kindi) :: jscal = 0                                               !/*mpd        40*/
      integer(kindi) :: lcolor = 300                                            !/*mpd        41*/
      integer(kindi) :: ncel                                                    !/*mpd        42*/
      integer(kindi) :: nlb                                                     !/*mpd        43*/
      integer(kindi) :: nxp                                                     !/*mpd        44*/
      integer(kindi) :: levplt = -1                                             !/*mpd        45*/
      integer(kindi) :: meshpl = 1                                              !/*mpd        46*/
!                                                                               !/*mpd        47*/
!***********************************************************************        !/*mpd        48*/
!        real variables and arrays.                                             !/*mpd        49*/
!                                                                               !/*mpd        50*/
      real(kindr) :: basis(9) = (/ 0.,1.,0.,0.,0.,1.,0.,0.,0. /)                !/*mpd        51*/
      real(kindr) :: exsav(2)                                                   !/*mpd        52*/
      real(kindr) :: extent(2) = (/ 100.,100. /)                                !/*mpd        53*/
      real(kindr) :: origin(3) = (/ 0.,0.,0. /)                                 !/*mpd        54*/
      real(kindr) :: orsav(3)                                                   !/*mpd        55*/
      real(kindr) :: plmx(4,4)                                                  !/*mpd        56*/
      real(kindr) :: pxx(4,4)                                                   !/*mpd        57*/
      real(kindr) :: sclabl(6) = (/ 1.,0.,1.,0.,9.,0. /)                        !/*mpdk        1*/
      real(kindr) :: sch =                                                      !/*mpd        59*/
#if !defined(XLIB) && !defined(LAHEY) && !defined(QWIN)                         /*!mpd        60*/
     & 0.02                                                                     !/*mpd        61*/
#else                                                                           /*!mpd        62*/
     & 0.03                                                                     !/*mpd        63*/
#endif                                                                          /*!mpd        64*/
!                                                                               !/*mpd        65*/
!***********************************************************************        !/*mpd        66*/
!        character variables and arrays.                                        !/*mpd        67*/
!                                                                               !/*mpd        68*/
!        the command keywords of plot.                                          !/*mpd        69*/
      character*8 :: keyp(nkeyp) = (/                                           !/*mpd        70*/
     & 'term    ','file    ','viewport','&       ','end     ',                  !/*mpd        71*/
     & 'return  ','mcplot  ','options ','status  ','runtpe  ',                  !/*mpd        72*/
     & 'basis   ','origin  ','extent  ','px      ','py      ',                  !/*mpd        73*/
     & 'pz      ','label   ','scales  ','center  ','factor  ',                  !/*mpd        74*/
     & 'theta   ','cursor  ','restore ','locate  ','color   ',                  !/*mpd        75*/
     & 'shade   ','?       ','help    ','pause   ','level   ',                  !/*mpd        76*/
     & 'mbody   ','interact','mesh    ','        ','        ' /)                !/*mpd        77*/
!                                                                               !/*mpd        78*/
!        color keywords of plot - see gxon for color indices.                   !/*mpd        79*/
      character*12 :: hcolor(10) = (/                                           !/*mpdk        2*/
     & 'dark-blue   ','blue        ','light-blue  ','sky-blue    ',             !/*mpdk        3*/
     & 'light-green ','green       ','pea-green   ','yellow      ',             !/*mpdk        4*/
     & 'orange      ','rust        ' /)                                         !/*mpdk        5*/
#endif                                                                          /*!mpd        84*/
      end module plot_data                                                      !/*mpd        85*/
