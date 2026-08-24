!     $Id: emsho.h,v 1.4 2002/03/27 18:14:14 gwm Exp $                        !/*em          1*/
!=== emsho ============================================================*        !/*em          2*/
!                                                                      *        !/*em          3*/
!----------------------------------------------------------------------*        !/*em          4*/
!     include file: emsho copy                created 27/11/86 by pa   *        !/*em          5*/
!     changes: none                                                    *        !/*em          6*/
!     included in the following subroutines or functions: not updated  *        !/*em          7*/
!                                                                      *        !/*em          8*/
!     description of the common block(s) and variable(s)               *        !/*em          9*/
!     egslo    = .true. egs is used for em cascades                    *        !/*em         10*/
!     egshlo   = .true. photohadrons are produced                      *        !/*em         11*/
!     egselo   = .true. electrohadrons are produced                    *        !/*em         12*/
!     egseth   =  cut-off transport energy of egs electrons            *        !/*em         13*/
!     egsbia   =  biasing factor for electro/photohadron production:   *        !/*em         14*/
!                 the interaction are sampled using a mean free path   *        !/*em         15*/
!                 equal to the actual one divided by egsbia            *        !/*em         16*/
!     egspth   =  cut-off transport energy of egs photons              *        !/*em         17*/
!     egshet   =  cut-off energy for electrohadron production          *        !/*em         18*/
!     egshpt   =  cut-off energy for photohadron production            *        !/*em         19*/
!                                                                      *        !/*em         20*/
!----------------------------------------------------------------------*        !/*em         21*/
!                                                                               !/*em         22*/
      logical egslo, egshlo, egselo                                             !/*em         23*/
      common /emsho/ egseth, egspth, egshet, egshpt, egsbia, egslo,             !/*em         24*/
     & egshlo, egselo                                                           !/*em         25*/
