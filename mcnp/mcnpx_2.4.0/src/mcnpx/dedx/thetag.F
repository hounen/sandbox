!     $Id: thetag.F,v 1.4 2002/03/27 18:14:38 gwm Exp $                       !/*th          1*/
      subroutine thetag                                                         !/*th          2*/
#include <lcs/double.h>                                                         /*!th          3*/
!                                                                               !/*th          4*/
!        this routine calls routine theta to get (compute) the                  !/*th          5*/
!        theta values and then stores them in the common                        !/*th          6*/
!        block8 for shell correction useage and for printing later.             !/*th          7*/
!                                                                               !/*th          8*/
!        routine by daniel pickens                                              !/*th          9*/
!        consultant - computer sciences corp.                                   !/*th         10*/
!        10 september 1983                                                      !/*th         11*/
!                                                                               !/*th         12*/
#include <dedx/block8.h>                                                        /*!th         13*/
      external atmdat                                                           !/*th         14*/
!                                                                               !/*th         15*/
!        determine the k shell theta values.                                    !/*th         16*/
!                                                                               !/*th         17*/
      do 10 iz=1,max_elem                                                       !/*th         18*/
      numshl=1                                                                  !/*th         19*/
      nprinc=1                                                                  !/*th         20*/
      nangle=1                                                                  !/*th         21*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         22*/
!                                                                               !/*th         23*/
!        determine the l shell theta values.                                    !/*th         24*/
!                                                                               !/*th         25*/
      numshl=2                                                                  !/*th         26*/
      nprinc=2                                                                  !/*th         27*/
      nangle=1                                                                  !/*th         28*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         29*/
      numshl=3                                                                  !/*th         30*/
      nangle=2                                                                  !/*th         31*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         32*/
!                                                                               !/*th         33*/
!        determine the m shell theta values.                                    !/*th         34*/
!                                                                               !/*th         35*/
      numshl=4                                                                  !/*th         36*/
      nprinc=3                                                                  !/*th         37*/
      nangle=1                                                                  !/*th         38*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         39*/
      numshl=5                                                                  !/*th         40*/
      nangle=2                                                                  !/*th         41*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         42*/
      numshl=6                                                                  !/*th         43*/
      nangle=3                                                                  !/*th         44*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         45*/
!                                                                               !/*th         46*/
!        determine the n shell theta values.                                    !/*th         47*/
!                                                                               !/*th         48*/
      numshl=7                                                                  !/*th         49*/
      nprinc=4                                                                  !/*th         50*/
      nangle=1                                                                  !/*th         51*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         52*/
      numshl=8                                                                  !/*th         53*/
      nangle=2                                                                  !/*th         54*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         55*/
      numshl=9                                                                  !/*th         56*/
      nangle=3                                                                  !/*th         57*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         58*/
      numshl=10                                                                 !/*th         59*/
      nangle=4                                                                  !/*th         60*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         61*/
!                                                                               !/*th         62*/
!        determine the o shell theta values.                                    !/*th         63*/
!                                                                               !/*th         64*/
      numshl=11                                                                 !/*th         65*/
      nprinc=5                                                                  !/*th         66*/
      nangle=1                                                                  !/*th         67*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         68*/
      numshl=12                                                                 !/*th         69*/
      nangle=2                                                                  !/*th         70*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         71*/
      numshl=13                                                                 !/*th         72*/
      nangle=3                                                                  !/*th         73*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         74*/
      numshl=14                                                                 !/*th         75*/
      nangle=4                                                                  !/*th         76*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         77*/
!                                                                               !/*th         78*/
!        determine the p shell theta values.                                    !/*th         79*/
!                                                                               !/*th         80*/
      numshl=15                                                                 !/*th         81*/
      nprinc=6                                                                  !/*th         82*/
      nangle=1                                                                  !/*th         83*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         84*/
      numshl=16                                                                 !/*th         85*/
      nangle=2                                                                  !/*th         86*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         87*/
      numshl=17                                                                 !/*th         88*/
      nangle=3                                                                  !/*th         89*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         90*/
!                                                                               !/*th         91*/
!        determine the q shell theta values.                                    !/*th         92*/
!                                                                               !/*th         93*/
      numshl=max_shell                                                          !/*th         94*/
      nprinc=7                                                                  !/*th         95*/
      nangle=1                                                                  !/*th         96*/
      thetas(numshl,iz)=theta(iz,numshl,nprinc,nangle)                          !/*th         97*/
!                                                                               !/*th         98*/
   10 continue                                                                  !/*th         99*/
!                                                                               !/*th        100*/
      return                                                                    !/*th        101*/
      end                                                                       !/*th        102*/
