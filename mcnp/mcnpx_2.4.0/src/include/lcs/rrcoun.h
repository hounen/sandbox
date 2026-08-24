!     $Id: rrcoun.h,v 1.4 2002/03/27 18:14:18 gwm Exp $                       !/*rr          1*/
!----------------------------------------------------------------------*        !/*rr          2*/
!     module rrcoun:                                                   *        !/*rr          3*/
!     russian roulette counters, a. fasso' 1987                        *        !/*rr          4*/
!                                                                      *        !/*rr          5*/
!----------------------------------------------------------------------*        !/*rr          6*/
      logical lrroul                                                            !/*rr          7*/
      common /rrcoun/ weikil, ekil, weisur, esur, weinor, enor, weiwei,         !/*rr          8*/
     & ewei, lrroul                                                             !/*rr          9*/
