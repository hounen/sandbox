!     $Id: dgmcom.h,v 1.4 2002/03/27 18:14:10 gwm Exp $                       !/*dg          1*/
      parameter (iedgmn=31, iadgmn=10, iedgmp=31, iadgmp=10)                    !/*dg          2*/
      common /dgmcom/ xdgmn(iedgmn,iadgmn), edgmn(iedgmn), adgmn(iadgmn)        !/*dg          3*/
     & , zdgmn(iadgmn), xdgmp(iedgmp,iadgmp), edgmp(iedgmp), adgmp              !/*dg          4*/
     & (iadgmp), zdgmp(iadgmp)                                                  !/*dg          5*/
