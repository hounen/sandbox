!     $Id: charxs.h,v 1.4 2002/03/27 18:14:21 gwm Exp $                       !/*ch          1*/
      parameter (mcase=20, mcas1=21)                                            !/*ch          2*/
      parameter (lpt1=16)                                                       !/*ch          3*/
      parameter (ndef=4)                                                        !/*ch          4*/
      character*80 jtitl1(mcase), jtitl2(mcase)                                 !/*ch          5*/
      character msub(ndef)*8, isub(ndef)*8, kpart(lpt1)*8                       !/*ch          6*/
      character*8 inxs, outxs, histp, xstal                                     !/*ch          7*/
      common /charxs/ msub, inxs, outxs, histp, xstal, kpart, jtitl1,           !/*ch          8*/
     &  jtitl2                                                                  !/*ch          9*/
      equivalence (isub,inxs)                                                   !/*ch         10*/
      common /xsexc/ fnorm0(mcase), tlnon, tsnon, tlfis, tsfis, tlels,          !/*ch         11*/
     & tsels, tltot, tstot, nnerg                                               !/*ch         12*/
     & (mcase), nnang(mcase), nntype(mcase), ntype1(mcase), nerg1(mcase)        !/*ch         13*/
     & , nang1(mcase), lerg(mcas1), lang(mcas1), ltyp(mcas1), nsize             !/*ch         14*/
     & (mcas1), kplot0(mcase), imom0(mcase), iyeld0(mcase), iperg, ipang        !/*ch         15*/
     & , iptyp, ncase, nerg, nang                                               !/*ch         16*/
#ifndef CHEAP                                                                   /*!ch         17*/
      parameter (ndp2=1)                                                        !/*ch         18*/
#endif                                                                          /*!ch         19*/
#ifdef CHEAP                                                                    /*!ch         20*/
      parameter (ndp2=2)                                                        !/*ch         21*/
#endif                                                                          /*!ch         22*/
#ifndef XSEXPOINTERS                                                            /*!ch         23*/
      parameter (mdas=200000)                                                   !/*ch         24*/
      common /array/ iblk(mdas)                                                 !/*ch         25*/
#endif                                                                          /*!ch         26*/
#ifdef XSEXPOINTERS                                                             /*!ch         27*/
      parameter (mdas=200)                                                      !/*ch         28*/
      common /array/ kdy                                                        !/*ch         29*/
#endif                                                                          /*!ch         30*/
!                                                                               !/*ch         31*/
!        fixed dynamically allocated common.                                    !/*ch         32*/
#ifdef XSEXPOINTERS                                                             /*!ch         33*/
      dimension iblk(1)                                                         !/*ch         34*/
      pointer (kdy,iblk)                                                        !/*ch         35*/
#endif                                                                          /*!ch         36*/
!                                                                               !/*ch         37*/
