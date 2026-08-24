!     $Id: alloc.h,v 1.4 2002/03/27 18:14:10 gwm Exp $                        !/*al          1*/
      character*6 names                                                         !/*al          2*/
      parameter (num=101)                                                       !/*al          3*/
      common /alloc/ maxblk, ierr, ict, istat, len(num), iptt(num)              !/*al          4*/
      common /alloc1/ names(num)                                                !/*al          5*/
