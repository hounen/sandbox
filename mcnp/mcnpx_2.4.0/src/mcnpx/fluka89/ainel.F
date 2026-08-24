!     $Id: ainel.F,v 1.4 2002/03/27 18:14:38 gwm Exp $                        !/*ai          1*/
      double precision function ainel (ij,ilo,e,a,sqa)                          !/*ai          2*/
!                                                                     *         !/*ai          3*/
!---------------------------------------------------------------------*         !/*ai          4*/
!                                                                     *         !/*ai          5*/
#include <lcs/double.h>                                                         /*!ai          6*/
#include <fluka89/flprm1.h>                                                     /*!ai          7*/
#include <fluka89/flprm2.h>                                                     /*!ai          8*/
#include <lcs/inout.h>                                                          /*!ai          9*/
!********************************************************************           !/*ai         10*/
!     version by                     j. ranft                                   !/*ai         11*/
!                                    leipzig                                    !/*ai         12*/
!     last change 16. july 81 by     pertti aarnio                              !/*ai         13*/
!                                    helsinki university of                     !/*ai         14*/
!                                    technology, finland                        !/*ai         15*/
!                                                                               !/*ai         16*/
!     function of fluka to give inelasticities for production of                !/*ai         17*/
!     particles ilo by particle ij                                              !/*ai         18*/
!                                                                               !/*ai         19*/
!     note!!!!!!! non-standard particle numbering                               !/*ai         20*/
!                                                                               !/*ai         21*/
!     input variables:                                                          !/*ai         22*/
!        ij    = type of the primary                                            !/*ai         23*/
!                1=proton                                                       !/*ai         24*/
!                2=neutron                                                      !/*ai         25*/
!                3=charged pion                                                 !/*ai         26*/
!                                                                               !/*ai         27*/
!        ilo   = type of the secondary                                          !/*ai         28*/
!                1=proton                                                       !/*ai         29*/
!                2=neutron                                                      !/*ai         30*/
!                3=charged pion                                                 !/*ai         31*/
!                4=neutral pion                                                 !/*ai         32*/
!                5=nuclear excitation excluding low energy                      !/*ai         33*/
!                  secondaries from intranuclear cascade                        !/*ai         34*/
!                6=intranuclear proton                                          !/*ai         35*/
!                7=intranuclear neutron                                         !/*ai         36*/
!                                                                               !/*ai         37*/
!        e     = kinetic energy of the primary in gev                           !/*ai         38*/
!        a     = atomic weight of the medium                                    !/*ai         39*/
!        sqa   = sqrt(a)                                                        !/*ai         40*/
!                                                                               !/*ai         41*/
!     other variables:                                                          !/*ai         42*/
!        ak    = look-up table for inelastcities                                !/*ai         43*/
!                                                                               !/*ai         44*/
!********************************************************************           !/*ai         45*/
!                                                                               !/*ai         46*/
      dimension ak(4,3)                                                         !/*ai         47*/
      data ak /0.35d0,0.3d0,0.25d0,0.1d0,0.3d0,0.35d0,0.25d0,0.1d0,0.15d        !/*ai         48*/
     & 0,0.15d0,0.5d0,0.2d0/                                                    !/*ai         49*/
!                                                                               !/*ai         50*/
      go to (10,10,10,10,20,30,40), ilo                                         !/*ai         51*/
!                                                                               !/*ai         52*/
   10 continue                                                                  !/*ai         53*/
      if (e.lt.0.125d0) go to 50                                                !/*ai         54*/
      ainel=ak(ilo,ij)*(1.d0-eexi(ij,e,a)/e)                                    !/*ai         55*/
      return                                                                    !/*ai         56*/
!                                                                               !/*ai         57*/
   20 continue                                                                  !/*ai         58*/
      if (e.lt.0.125d0) go to 60                                                !/*ai         59*/
      anel=eexi(ij,e,a)/e                                                       !/*ai         60*/
      akek=ekeka(2,e,a,sqa)/e+ekeka(3,e,a,sqa)/e                                !/*ai         61*/
      ainel=anel-akek                                                           !/*ai         62*/
      if (akek.ge.anel) ainel=ekeka(1,e,a,sqa)/e                                !/*ai         63*/
      return                                                                    !/*ai         64*/
!                                                                               !/*ai         65*/
   30 continue                                                                  !/*ai         66*/
      if (e.lt.0.125d0) go to 50                                                !/*ai         67*/
      ainel=ekeka(2,e,a,sqa)/e                                                  !/*ai         68*/
      return                                                                    !/*ai         69*/
!                                                                               !/*ai         70*/
   40 continue                                                                  !/*ai         71*/
      if (e.lt.0.125d0) go to 50                                                !/*ai         72*/
      ainel=ekeka(3,e,a,sqa)/e                                                  !/*ai         73*/
      return                                                                    !/*ai         74*/
!                                                                               !/*ai         75*/
   50 continue                                                                  !/*ai         76*/
      ainel=0.d0                                                                !/*ai         77*/
      return                                                                    !/*ai         78*/
!                                                                               !/*ai         79*/
   60 continue                                                                  !/*ai         80*/
      ainel=1.d0                                                                !/*ai         81*/
      return                                                                    !/*ai         82*/
      end                                                                       !/*ai         83*/
