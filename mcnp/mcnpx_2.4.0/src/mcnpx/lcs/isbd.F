!     $Id: isbd.F,v 1.4 2002/03/27 18:15:09 gwm Exp $                         !/*is          1*/
      block data isbd                                                           !/*is          2*/
! ISBD   called by ISABEL                                                       !/*is          3*/
#include <lcs/double.h>                                                         /*!is          4*/
#ifndef OLDM                                                                    /*!is          5*/
#include <lcs/phprm2.h>                                                         /*!is          6*/
#endif                                                                          /*!is          7*/
#include <lcs/vegas.h>                                                          /*!is          8*/
      dimension ar1(1048), ar2(3143)                                            !/*is          9*/
      equivalence (ar1,csa), (ar2,proj)                                         !/*is         10*/
      data ar1 /1048*0.d0/, ar2 /3143*0.d0/                                     !/*is         11*/
      data icsb, ind, ieght /58*0/                                              !/*is         12*/
      data nuc, npmon, jpro, ipro, npout, kaskew, nopot /507*0/                 !/*is         13*/
#ifdef OLDM                                                                     /*!is         14*/
      data amass /938.28d0,939.573d0,139.567d0,134.96d0,139.567d0,0.0d0,        !/*is         15*/
     & 493.67d0,497.67d0,493.67d0/                                              !/*is         16*/
#else                                                                           /*!is         17*/
      data amass /wprot,wneut,wpion,wpi0,wpion                                  !/*is         18*/
     & ,0.0d0,wkaon,wka0,wkaon/                                                 !/*is         19*/
#endif                                                                          /*!is         20*/
      data monin, se, ses, sr, srs /7*0,20*dp0/                                 !/*is         21*/
#ifdef OLDM                                                                     /*!is         22*/
      data xmasss /-1.000000d+00,9.382800d+02,1.876560d+03,9.395730d+02,        !/*is         23*/
     & 1.875627d+03,2.808421d+03,1.879146d+03,2.808951d+03,3.727418d+03/        !/*is         24*/
#else                                                                           /*!is         25*/
      data xmasss /-1.000000d+00,wprot,1.8765440d+03,wneut,                     !/*is         26*/
     & wdeut,w_he3,1.8791316d+03,wtrit,walph/                                   !/*is         27*/
#endif                                                                          /*!is         28*/
      data aa /360*dp0/, bb /3600*dp0/, transp /dp0/, na /11*0/, kcas /0        !/*is         29*/
     & /                                                                        !/*is         30*/
      end                                                                       !/*is         31*/
