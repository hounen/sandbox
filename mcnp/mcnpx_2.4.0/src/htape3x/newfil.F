!     $Id: newfil.F,v 1.5 2002/03/27 18:14:00 gwm Exp $                       !/*ne          1*/
      subroutine newfil (nfile,lun)                                             !/*ne          2*/
#include <htape3x/com32b.h>                                                     /*!ne          3*/
      character *8 nfile                                                        !/*ne          4*/
      close (lun)                                                               !/*ne          5*/
      open (lun,file=nfile,status='old',access='sequential',                    !/*ne          6*/
     & form='unformatted')                                                      !/*ne          7*/
      return                                                                    !/*ne          8*/
      end                                                                       !/*ne          9*/
