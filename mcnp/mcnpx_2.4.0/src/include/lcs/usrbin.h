!     $Id: usrbin.h,v 1.4 2002/03/27 18:14:19 gwm Exp $                       !/*us          1*/
!=== usrbin ==========================================================*         !/*us          2*/
!                                                                               !/*us          3*/
!---------------------------------------------------------------------*         !/*us          4*/
!     module usrbin:                                                  *         !/*us          5*/
!     a. ferrari & a. fasso': user defined binnings                   *         !/*us          6*/
!          last change a. ferrari 3-oct-1989                          *         !/*us          7*/
!                                                                     *         !/*us          8*/
!                                                                     *         !/*us          9*/
!     up to mxusbn user defined binnings are allowed                  *         !/*us         10*/
!            lusbin = logical flag, .true. if at least 1 user defined *         !/*us         11*/
!                     binning is used                                 *         !/*us         12*/
!            nusrbn = number of user defined binnings used            *         !/*us         13*/
!            itusbn = type of binning: 0 = cartesian, .ne. 0 = rz     *         !/*us         14*/
!            idusbn = distribution to be scored: the usual values are *         !/*us         15*/
!                     allowed                                         *         !/*us         16*/
!            titusb = binning name                                    *         !/*us         17*/
!            ipusbn = logical unit to print the results on: formatted *         !/*us         18*/
!                     if > 0, unformatted if < 0                      *         !/*us         19*/
!            kbusbn = initial location in blank common of the consi-  *         !/*us         20*/
!                     dered binning (real*8 address)                  *         !/*us         21*/
!            nxbin  = number of x (r for rz) intervals                *         !/*us         22*/
!            nybin  = number of y (1 for rz) intervals                *         !/*us         23*/
!            nzbin  = number of z intervals                           *         !/*us         24*/
!         xlow/high = minimum and maximum x (r for rz)                *         !/*us         25*/
!         ylow/high = minimum and maximum y (meaningless for rz)      *         !/*us         26*/
!         zlow/high = minimum and maximum z                           *         !/*us         27*/
!            dxusbn = x (r) bin width                                 *         !/*us         28*/
!            dyusbn = y bin width                                     *         !/*us         29*/
!            dzusbn = z bin width                                     *         !/*us         30*/
!                                                                     *         !/*us         31*/
!---------------------------------------------------------------------*         !/*us         32*/
!                                                                               !/*us         33*/
      parameter (mxusbn=50)                                                     !/*us         34*/
      logical lusbin                                                            !/*us         35*/
      character*10 titusb                                                       !/*us         36*/
      common /usrbn/ xlow(mxusbn), xhigh(mxusbn), ylow(mxusbn), yhigh           !/*us         37*/
     & (mxusbn), zlow(mxusbn), zhigh(mxusbn), dxusbn(mxusbn), dyusbn            !/*us         38*/
     & (mxusbn), dzusbn(mxusbn), nxbin(mxusbn), nybin(mxusbn), nzbin            !/*us         39*/
     & (mxusbn), itusbn(mxusbn), idusbn(mxusbn), kbusbn(mxusbn), ipusbn         !/*us         40*/
     & (mxusbn), nusrbn, lusbin                                                 !/*us         41*/
      common /usrch/ titusb(mxusbn)                                             !/*us         42*/
