!     $Id: xsbd.F,v 1.4 2002/03/27 18:14:51 gwm Exp $                         !/*xs          1*/
      block data xsbd                                                           !/*xs          2*/
!  B&P ROUTINE                                                                  !/*xs          3*/
#include <lcs/double.h>                                                         /*!xs          4*/
#include <hexs/barpo.h>                                                         /*!xs          5*/
!     aai- target nucleus mass numbers                                          !/*xs          6*/
      data aai /4.00d0,9.01d0,12.00d0,14.00d0,16.00d0,23.00d0,26.98d0,40        !/*xs          7*/
     & .08d0,47.90d0,55.85d0,63.50d0,79.90d0,95.94d0,112.40d0,118.69d0,1        !/*xs          8*/
     & 37.34d0,183.85d0,207.19d0,238.03d0/                                      !/*xs          9*/
!     nucleus-nucleus cross-section parameters                                  !/*xs         10*/
      data cx /2.07d0,560.d0,0.8d0,0.426d0,100.d0,-2.05d0,1.9d0,200.d0,0        !/*xs         11*/
     & .07d0,0.87d0,20.d0,-1.55d0,2.1d0,700.d0,-1.01d0,1.08d0,400.d0,-0.        !/*xs         12*/
     & 59d0,0.94d0,2.45d0,225.d0,-2.25d0,2.d0,100.d0,-4.61d0,3.18d0,185         !/*xs         13*/
     & .d0,-3.d0,2.4d0,185.d0,-3.d0,2.4d0,185.d0,-4.77d0,3.18d0,185.d0,-        !/*xs         14*/
     & 4.77d0,3.18d0/                                                           !/*xs         15*/
      end                                                                       !/*xs         16*/
