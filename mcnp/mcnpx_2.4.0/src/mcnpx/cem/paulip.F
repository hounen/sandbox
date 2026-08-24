      subroutine paulip (partin, ipatin, v, tfp, tfn, mv, np, irefl,            !/*pa          1*/
     &                   ipa)                                                   !/*pa          2*/
#ifdef CEM                                                                      /*!pa          3*/
! ======================================================================        !/*pa          4*/
!                                                                               !/*pa          5*/
!     Pauli principle subroutine. Converts momenta from interaction             !/*pa          6*/
!     CM frame to lab frame, and determines if any nucleon final                !/*pa          7*/
!     state is Pauli blocked.                                                   !/*pa          8*/
!                                                                               !/*pa          9*/
!     Called by: CASCAD                                                         !/*pa         10*/
!                                                                               !/*pa         11*/
!     Calls: KINEMA                                                             !/*pa         12*/
!                                                                               !/*pa         13*/
!   CEM95 written by S. G. Mashnik                                              !/*pa         14*/
!                                                                               !/*pa         15*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*pa         16*/
!   More editing, AJS  July, 1997.                                              !/*pa         17*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*pa         18*/
!                                                                               !/*pa         19*/
! ======================================================================        !/*pa         20*/
!                                                                               !/*pa         21*/
!   The C. M. momentum components are initially stored in pmemo(1:3).           !/*pa         22*/
!   They are transformed into the lab frame in KINEMA; then tested to           !/*pa         23*/
!   see if the lab kinetic energies are greater than the Fermi kinetic          !/*pa         24*/
!   energy.                                                                     !/*pa         25*/
!                                                                               !/*pa         26*/
!  Definition of partin:                                                        !/*pa         27*/
!                       partin(1); x coordinate of particle                     !/*pa         28*/
!                       partin(2); y coordinate of particle                     !/*pa         29*/
!                       partin(3); z coordinate of particle                     !/*pa         30*/
!                       partin(4); sin(theta), direction of momentum            !/*pa         31*/
!                       partin(5); cos(theta), direction of momentum            !/*pa         32*/
!                       partin(6); sin(phi), direction of momentum              !/*pa         33*/
!                       partin(7); cos(phi), direction of momentum              !/*pa         34*/
!                       partin(8); kinetic energy of particle                   !/*pa         35*/
!                       partin(9); rest mass of particle                        !/*pa         36*/
!                                                                               !/*pa         37*/
!  Definition of ipatin:                                                        !/*pa         38*/
!                       ipatin(1); charge of particle                           !/*pa         39*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*pa         40*/
!                       ipatin(3); strangeness of particle                      !/*pa         41*/
!                       ipatin(4); particle baryon number                       !/*pa         42*/
!                       ipatin(5); zone number of nucleus where particle        !/*pa         43*/
!                                  is located.                                  !/*pa         44*/
!                                                                               !/*pa         45*/
! ======================================================================        !/*pa         46*/
#include <cem/cdouble.h>                                                        /*!pa         47*/
      dimension pmemo(9,150), imemo(5,150), partin(9), ipatin(5),               !/*pa         48*/
     &          v(3), p(3), pstar(3), tfp(10), tfn(10)                          !/*pa         49*/
      common /exiton/ nexip, nexin, nhol                                        !/*pa         50*/
      common /gener/  ngen(150), ing, igs(150), meso                            !/*pa         51*/
      common /indexx/  indi                                                     !/*pa         52*/
      common /memory/ pmemo, imemo                                              !/*pa         53*/
      common /mpi/    empich, empi0                                             !/*pa         54*/
! ======================================================================        !/*pa         55*/
      j1 = ipatin(5)                                                            !/*pa         56*/
      l = 1                                                                     !/*pa         57*/
   10 if (np.eq.2 .and. l.eq.2) l = l + 1                                       !/*pa         58*/
      mtemp = mv + l                                                            !/*pa         59*/
      pstar(1) = pmemo(1,mtemp)                                                 !/*pa         60*/
      pstar(2) = pmemo(2,mtemp)                                                 !/*pa         61*/
      pstar(3) = pmemo(3,mtemp)                                                 !/*pa         62*/
      call kinema (pstar, v, p, ct, st, cfi, sfi, t, pmemo(9,mtemp))            !/*pa         63*/
      pmemo(1,mtemp) = partin(1)                                                !/*pa         64*/
      pmemo(2,mtemp) = partin(2)                                                !/*pa         65*/
      pmemo(3,mtemp) = partin(3)                                                !/*pa         66*/
      pmemo(4,mtemp) = st                                                       !/*pa         67*/
      pmemo(5,mtemp) = ct                                                       !/*pa         68*/
      pmemo(6,mtemp) = sfi                                                      !/*pa         69*/
      pmemo(7,mtemp) = cfi                                                      !/*pa         70*/
      pmemo(8,mtemp) = t                                                        !/*pa         71*/
      imemo(5,mtemp) = ipatin(5)                                                !/*pa         72*/
      ngen(mtemp) = ing + 1                                                     !/*pa         73*/
      if (imemo(4,mtemp).ne.1 .or. imemo(3,mtemp).ne.0) then                    !/*pa         74*/
!  Not a baryon:                                                                !/*pa         75*/
        if (l.lt.np) then                                                       !/*pa         76*/
          l = l + 1                                                             !/*pa         77*/
          go to 10                                                              !/*pa         78*/
        elseif (l.ge.np .and. ipa.eq.0) then                                    !/*pa         79*/
          ipa = 2                                                               !/*pa         80*/
          return                                                                !/*pa         81*/
        else                                                                    !/*pa         82*/
          go to 20                                                              !/*pa         83*/
        endif                                                                   !/*pa         84*/
      else                                                                      !/*pa         85*/
!  A baryon:                                                                    !/*pa         86*/
        temp = imemo(1,mtemp)                                                   !/*pa         87*/
        tf = tfp(j1)*temp + tfn(j1)*(dp1 - temp)                                !/*pa         88*/
        if (pmemo(8,mtemp).gt.tf) then                                          !/*pa         89*/
          if (l.lt.np) then                                                     !/*pa         90*/
!   If all particles have not been checked, go back.                            !/*pa         91*/
            l = l + 1                                                           !/*pa         92*/
            go to 10                                                            !/*pa         93*/
          elseif (l.ge.np .and. ipa.eq.0) then                                  !/*pa         94*/
!   Energy was less than cutof3 in CASCAD, the 2 imaginary                      !/*pa         95*/
!   potentials were more than 30% different, and both nucleons                  !/*pa         96*/
!   exceeded the Fermi energy:                                                  !/*pa         97*/
            ipa = 2                                                             !/*pa         98*/
            return                                                              !/*pa         99*/
          endif                                                                 !/*pa        100*/
        else                                                                    !/*pa        101*/
!   Kinetic energy <= Fermi energy; return to CASCAD with ipa unchanged.        !/*pa        102*/
          return                                                                !/*pa        103*/
        endif                                                                   !/*pa        104*/
      endif                                                                     !/*pa        105*/
   20 if (l.ge.np .and. ipa.ne.0) then                                          !/*pa        106*/
!       if ((partin(9).eq.dp14m2 .and. np.eq.2) .and.                           !/*pa        107*/
        if ((abs(partin(9)-empich).lt.dp1m2 .and. np.eq.2) .and.                !/*pa        108*/
     &      (pmemo(9,mv+1).gt.dp9m1 .and.                                       !/*pa        109*/
     &       pmemo(9,mv+3).gt.dp9m1)) indi = 3                                  !/*pa        110*/
        partin(4) = pmemo(4,mv+3)                                               !/*pa        111*/
        partin(5) = pmemo(5,mv+3)                                               !/*pa        112*/
        partin(6) = pmemo(6,mv+3)                                               !/*pa        113*/
        partin(7) = pmemo(7,mv+3)                                               !/*pa        114*/
        partin(8) = pmemo(8,mv+3)                                               !/*pa        115*/
        partin(9) = pmemo(9,mv+3)                                               !/*pa        116*/
        ipatin(1) = imemo(1,mv+3)                                               !/*pa        117*/
        ipatin(2) = imemo(2,mv+3)                                               !/*pa        118*/
        ipatin(3) = imemo(3,mv+3)                                               !/*pa        119*/
        ipatin(4) = imemo(4,mv+3)                                               !/*pa        120*/
        ing = ngen(mv+3)                                                        !/*pa        121*/
        nhol = nhol + 1                                                         !/*pa        122*/
        if (np.gt.2) then                                                       !/*pa        123*/
          ntemp = mv + np                                                       !/*pa        124*/
          pmemo(4,mv+3) = pmemo(4,ntemp)                                        !/*pa        125*/
          pmemo(5,mv+3) = pmemo(5,ntemp)                                        !/*pa        126*/
          pmemo(6,mv+3) = pmemo(6,ntemp)                                        !/*pa        127*/
          pmemo(7,mv+3) = pmemo(7,ntemp)                                        !/*pa        128*/
          pmemo(8,mv+3) = pmemo(8,ntemp)                                        !/*pa        129*/
          pmemo(9,mv+3) = pmemo(9,ntemp)                                        !/*pa        130*/
          imemo(1,mv+3) = imemo(1,ntemp)                                        !/*pa        131*/
          imemo(2,mv+3) = imemo(2,ntemp)                                        !/*pa        132*/
          imemo(3,mv+3) = imemo(3,ntemp)                                        !/*pa        133*/
          imemo(4,mv+3) = imemo(4,ntemp)                                        !/*pa        134*/
          meso = 1                                                              !/*pa        135*/
        endif                                                                   !/*pa        136*/
        mv = mv + np - 1                                                        !/*pa        137*/
        irefl = 0                                                               !/*pa        138*/
      endif                                                                     !/*pa        139*/
#endif                                                                          /*!pa        140*/
      return                                                                    !/*pa        141*/
      end                                                                       !/*pa        142*/
