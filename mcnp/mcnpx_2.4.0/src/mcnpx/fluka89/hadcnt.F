!     $Id: hadcnt.F,v 1.5 2002/03/27 18:14:44 gwm Exp $                       !/*ha          1*/
      subroutine hadcnt (kmes,nout)                                             !/*ha          2*/
#include <lcs/double.h>                                                         /*!ha          3*/
#include <lcs/inout.h>                                                          /*!ha          4*/
      parameter (lmes=10)                                                       !/*ha          5*/
      dimension nmes(lmes)                                                      !/*ha          6*/
! ..  save nmes                                                                 !/*ha          7*/
      data nmes /lmes*0/                                                        !/*ha          8*/
      if (kmes.gt.0) then                                                       !/*ha          9*/
        nmes(kmes)=nmes(kmes)+1                                                 !/*ha         10*/
        nmes(lmes)=nmes(lmes)+1                                                 !/*ha         11*/
        nout=nmes(kmes)                                                         !/*ha         12*/
      elseif (kmes.lt.0) then                                                   !/*ha         13*/
        nmes(lmes-1)=nmes(lmes-1)+1                                             !/*ha         14*/
        nout=0                                                                  !/*ha         15*/
      else                                                                      !/*ha         16*/
        nout=0                                                                  !/*ha         17*/
        if (nmes(lmes).gt.0) then                                               !/*ha         18*/
          write (io,10) nmes(1)                                                 !/*ha         19*/
          write (io,20) nmes(2)                                                 !/*ha         20*/
          write (io,30) nmes(3)                                                 !/*ha         21*/
          write (io,40) nmes(4)                                                 !/*ha         22*/
          write (io,50) nmes(5)                                                 !/*ha         23*/
          write (itty,10) nmes(1)                                               !/*ha         24*/
          write (itty,20) nmes(2)                                               !/*ha         25*/
          write (itty,30) nmes(3)                                               !/*ha         26*/
          write (itty,40) nmes(4)                                               !/*ha         27*/
          write (itty,50) nmes(5)                                               !/*ha         28*/
          write (io,60) nmes(lmes-1),nmes(lmes)                                 !/*ha         29*/
          write (itty,60) nmes(lmes-1),nmes(lmes)                               !/*ha         30*/
        endif                                                                   !/*ha         31*/
      endif                                                                     !/*ha         32*/
      return                                                                    !/*ha         33*/
!.                                                                              !/*ha         34*/
!                                                                               !/*ha         35*/
   10 format (i10,' charge conservation failures in HADEVT')                    !/*ha         36*/
   20 format (i10,' baryon conservation failures in HADEVT')                    !/*ha         37*/
   30 format (i10,' strangeness conservation failures in HADEVT')               !/*ha         38*/
   40 format (i10,' energy conservation failures in HADEVT')                    !/*ha         39*/
   50 format (i10,' momentum conservation failures in HADEVT')                  !/*ha         40*/
   60 format (i10,' interactions accepted in HADEVT'/i10,                       !/*ha         41*/
     & ' interactions rejected in HADEVT')                                      !/*ha         42*/
      end                                                                       !/*ha         43*/
