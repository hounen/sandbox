!     $Id: wapcom.h,v 1.4 2002/03/27 18:14:12 gwm Exp $                       !/*wa          1*/
!=======================================================================        !/*wa          2*/
      parameter (lwaps=270, lwidth=41, lmid=21)                                 !/*wa          3*/
      common /wapcom/ waps(lwidth,lwaps), elbnde(0:lwaps), deln(0:lwaps)        !/*wa          4*/
     & , third(0:lwaps), iz0(lwaps), maxz(0:lwaps), maxn(0:lwaps), izmin        !/*wa          5*/
     & (lwaps), izmax(lwaps)                                                    !/*wa          6*/
      dimension delp(0:lwaps)                                                   !/*wa          7*/
      equivalence (deln,delp)                                                   !/*wa          8*/
      character*1 cwaps                                                         !/*wa          9*/
      common /cwapcom/ cwaps(lwidth,lwaps)                                      !/*wa         10*/
!=======================================================================        !/*wa         11*/
