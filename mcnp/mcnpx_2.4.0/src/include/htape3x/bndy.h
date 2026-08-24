!     $Id: bndy.h,v 1.4 2002/03/27 18:14:10 gwm Exp $                         !/*bn          1*/
      parameter (lb2=2000, lb3=100)                                             !/*bn          2*/
      parameter (lb31=lb3+1)                                                    !/*bn          3*/
      parameter (lb21=lb2+1)                                                    !/*bn          4*/
#ifdef HTPLOT                                                                   /*!bn          5*/
      real xx, yy                                                               !/*bn          6*/
      common /bndy/ ergb(lb2), timb(lb3), dnerg(lb21), dfparm(lpx1), y0         !/*bn          7*/
     & (lb2), xx(2*lb21), yy(2*lb21), dntim(lb31)                               !/*bn          8*/
#endif                                                                          /*!bn          9*/
#ifndef HTPLOT                                                                  /*!bn         10*/
      common /bndy/ ergb(lb2), timb(lb3), dnerg(lb21), dfparm(lpx1),            !/*bn         11*/
     & dntim(lb31)                                                              !/*bn         12*/
#endif                                                                          /*!bn         13*/
