!     $Id: usrbdx.h,v 1.4 2002/03/27 18:14:19 gwm Exp $                       !/*us          1*/
!=== usrbdx ==========================================================*         !/*us          2*/
!                                                                               !/*us          3*/
!---------------------------------------------------------------------*         !/*us          4*/
!     module usrbdx:                                                  *         !/*us          5*/
!     a. ferrari : user defined boundary crossing scoring             *         !/*us          6*/
!          last change a. ferrari 16-may-1990                         *         !/*us          7*/
!                                                                     *         !/*us          8*/
!                                                                     *         !/*us          9*/
!     up to mxusbx user defined bdrx are allowed                      *         !/*us         10*/
!            lusbdx = logical flag, .true. if at least 1 user defined *         !/*us         11*/
!                     bdrx is used                                    *         !/*us         12*/
!            nusrbx = number of user defined bdrx used                *         !/*us         13*/
!            itusbx = type of binning: 1 = linear energy, linear angle*         !/*us         14*/
!                     2 = linear energy, logarithmic angle, -1 = loga-*         !/*us         15*/
!                     rithmic energy, linear angle, -2 = logarithmic  *         !/*us         16*/
!                     energy, logarithmic angle                       *         !/*us         17*/
!            idusbx = distribution to be scored: the usual values are *         !/*us         18*/
!                     allowed                                         *         !/*us         19*/
!            nr1usx = first region                                    *         !/*us         20*/
!            nr2usx = second region                                   *         !/*us         21*/
!            ausbdx = area (cm**2) of the detector                    *         !/*us         22*/
!            lwusbx = one way if false, two ways if true              *         !/*us         23*/
!            lfusbx = current if false, fluence if true               *         !/*us         24*/
!            titusx = bdrx name                                       *         !/*us         25*/
!            ipusbx = logical unit to print the results on: formatted *         !/*us         26*/
!                     if > 0, unformatted if < 0                      *         !/*us         27*/
!            kbusbx = initial location in blank common of the consi-  *         !/*us         28*/
!                     dered bdrx (real*8 address)                     *         !/*us         29*/
!            nebxbn = number of energy intervals                      *         !/*us         30*/
!            nabxbn = number of angular intervals                     *         !/*us         31*/
!     ebxlow/ebxhgh = minimum and maximum energies                    *         !/*us         32*/
!     abxlow/abxhgh = minimum and maximum angle (steradian)           *         !/*us         33*/
!            debxbn = energy bin width                                *         !/*us         34*/
!            dabxbn = angular (steradian) bin width                   *         !/*us         35*/
!                                                                     *         !/*us         36*/
!---------------------------------------------------------------------*         !/*us         37*/
!                                                                               !/*us         38*/
      parameter (mxusbx=50)                                                     !/*us         39*/
      logical lusbdx, lfusbx, lwusbx                                            !/*us         40*/
      character*10 titusx                                                       !/*us         41*/
      common /usrbx/ ebxlow(mxusbx), ebxhgh(mxusbx), abxlow(mxusbx),            !/*us         42*/
     & abxhgh(mxusbx), debxbn(mxusbx), dabxbn(mxusbx), ausbdx(mxusbx),          !/*us         43*/
     & nebxbn(mxusbx), nabxbn(mxusbx), lfusbx(mxusbx), lwusbx(mxusbx),          !/*us         44*/
     & nr1usx(mxusbx), nr2usx(mxusbx), itusbx(mxusbx), idusbx(mxusbx),          !/*us         45*/
     & kbusbx(mxusbx), ipusbx(mxusbx), nusrbx, lusbdx                           !/*us         46*/
      common /usxch/ titusx(mxusbx)                                             !/*us         47*/
