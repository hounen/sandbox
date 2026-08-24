!     $Id: uopen.F,v 1.4 2002/03/27 18:16:23 gwm Exp $                        !/*uo          1*/
      subroutine uopen (jtty,lun,fname,f)                                       !/*uo          2*/
!                                                                               !/*uo          3*/
!     Opens a files with a unique nonexisting name in the sequence              !/*uo          4*/
!     "file, fileA, fileB, ..."                                                 !/*uo          5*/
!                                                                               !/*uo          6*/
#include <htape3x/com32b.h>                                                     /*!uo          7*/
      character fname*8, f*1, new*7                                             !/*uo          8*/
      data new /'new'/                                                          !/*uo          9*/
      if (inqire(fname).ne.0) call unique (fname,jtty)                          !/*uo         10*/
      if (f.eq.'f') then                                                        !/*uo         11*/
        open (lun,file=fname,status=new,form='formatted')                       !/*uo         12*/
      else                                                                      !/*uo         13*/
      open (lun,file=fname,status=new,access='sequential',form=                 !/*uo         14*/
     & 'unformatted')                                                           !/*uo         15*/
      endif                                                                     !/*uo         16*/
      return                                                                    !/*uo         17*/
      end                                                                       !/*uo         18*/
