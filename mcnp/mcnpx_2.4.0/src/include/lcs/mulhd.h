!     $Id: mulhd.h,v 1.4 2002/03/27 18:14:16 gwm Exp $                        !/*mu          1*/
!=== mulhd ============================================================*        !/*mu          2*/
!                                                                               !/*mu          3*/
!----------------------------------------------------------------------*        !/*mu          4*/
!                                                                      *        !/*mu          5*/
!   common mulhd for the new multiple scattering in fluka              *        !/*mu          6*/
!                  created by a. ferrari & p. sala on 23-oct-1989      *        !/*mu          7*/
!                                                                      *        !/*mu          8*/
!          included in:                                                *        !/*mu          9*/
!                        bdnopt                                        *        !/*mu         10*/
!                        fluka                                         *        !/*mu         11*/
!                        kaskad                                        *        !/*mu         12*/
!                        mulhad                                        *        !/*mu         13*/
!                        mulmix                                        *        !/*mu         14*/
!                        stepop                                        *        !/*mu         15*/
!                                                                      *        !/*mu         16*/
!                                                                      *        !/*mu         17*/
!                        mxxmdf = maximum number of media              *        !/*mu         18*/
!                                                                      *        !/*mu         19*/
!----------------------------------------------------------------------*        !/*mu         20*/
!                                                                               !/*mu         21*/
      parameter (mxxpt1=2)                                                      !/*mu         22*/
      logical ltopt                                                             !/*mu         23*/
!                                                                               !/*mu         24*/
      common /mulhd/ blcc(mxxmdf), blccra(mxxmdf), xcc(mxxmdf), ztilde          !/*mu         25*/
     & (mxxmdf,mxxpt1), alpztl(mxxmdf,mxxpt1), rldu(mxxmdf), alpzt2             !/*mu         26*/
     & (mxxmdf,mxxpt1), teff0(mxxmdf), xr0(mxxmdf), ecutm(mxxmdf,30,2),         !/*mu         27*/
     & estepf(mxxmdf), hthnsz(mxxmdf,30), ae1o3(mxxmdf), thmspr, thmssc,        !/*mu         28*/
     & hmsamp, hmreje, jtokpa(30), ltopt, nfscat                                !/*mu         29*/
