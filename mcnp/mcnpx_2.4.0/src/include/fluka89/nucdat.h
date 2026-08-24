!     $Id: nucdat.h,v 1.4 2002/03/27 18:14:08 gwm Exp $                       !/*nu          1*/
!=== nucdat ==========================================================*         !/*nu          2*/
!                                                                     *         !/*nu          3*/
!---------------------------------------------------------------------*         !/*nu          4*/
!                                                                     *         !/*nu          5*/
!  plabrc is the the reduced planck constant times the light velocity           !/*nu          6*/
!  expressed in gev fm                                                          !/*nu          7*/
      parameter (plabrc=0.197327053d+00)                                        !/*nu          8*/
      parameter (amuamu=0.93149432d+00)                                         !/*nu          9*/
!     parameter ( amprot = 0.9382796   d+00 )                                   !/*nu         10*/
      parameter (amprot=0.93827231d+00)                                         !/*nu         11*/
!     parameter ( amneut = 0.9395731   d+00 )                                   !/*nu         12*/
      parameter (amneut=0.93956563d+00)                                         !/*nu         13*/
      parameter (amelec=0.51099906d-03)                                         !/*nu         14*/
      parameter (r0nucl=1.12d+00)                                               !/*nu         15*/
      parameter (coulpr=0.001439965d+00)                                        !/*nu         16*/
!     parameter ( fertho = 15.73       d-09 )                                   !/*nu         17*/
!     parameter ( expebn = 7.d+00 / 3.d+00  )                                   !/*nu         18*/
!     parameter ( bexc12 = fertho * 65.41634134195703d+00 )                     !/*nu         19*/
      parameter (fertho=14.33d-09)                                              !/*nu         20*/
      parameter (expebn=2.39d+00)                                               !/*nu         21*/
      parameter (bexc12=fertho*72.40715579499394d+00)                           !/*nu         22*/
      parameter (amuc12=amuamu-amelec/2.d+00-bexc12/12.d+00)                    !/*nu         23*/
      parameter (amhydr=amprot+amelec)                                          !/*nu         24*/
      parameter (amhton=amhydr-amneut)                                          !/*nu         25*/
      parameter (amntou=amneut-amuc12)                                          !/*nu         26*/
      parameter (amucsq=amuc12*amuc12)                                          !/*nu         27*/
      parameter (ebndav=0.5d+00*(amprot+amneut)-amuc12)                         !/*nu         28*/
!                                                                     *         !/*nu         29*/
!---------------------------------------------------------------------*         !/*nu         30*/
