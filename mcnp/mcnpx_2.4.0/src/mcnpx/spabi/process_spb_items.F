!     $Id: process_spb_items.F,v 1.4 2002/03/27 18:16:20 gwm Exp $            !/*pr          1*/
      subroutine process_spb_items                                              !/*pr          2*/
!     Process the data items on an SPABI card.                                  !/*pr          3*/
!                                                                               !/*pr          4*/
!     If first item, parse to set NUM_PRIMARIES_ACTIVE(), the number            !/*pr          5*/
!     of primary particle types active for the current secondary type.          !/*pr          6*/
!                                                                               !/*pr          7*/
!     Otherwise, load the values for the energy-band upper limits,              !/*pr          8*/
!     and the associated biasing parameters.                                    !/*pr          9*/
!                                                                               !/*pr         10*/
#ifdef SPABI                                                                    /*!pr         11*/
      use global_data                                                           !/*pra         1*/
      use imcn_data                                                             !/*pra         2*/
!                                                                               !/*pra         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*pra         4*/
      implicit integer(kindi) (i-n)                                             !/*pra         5*/
!                                                                               !/*pr         14*/
      do id_sec=1,mipt                                                          !/*pr         15*/
         if(nqp(id_sec).ne.0)go to 10                                           !/*pr         16*/
      enddo                                                                     !/*pr         17*/
      return                                                                    !/*pr         18*/
!     (The error message should be given elsewhere.)                            !/*pr         19*/
!                                                                               !/*pr         20*/
!     primary particle designator item...                                       !/*pr         21*/
   10 if (nwc.eq.1) then                                                        !/*pr         22*/
         if(kitm.ne.0)call erprnt(2,1,0,0,0,0,0,0,                              !/*pr         23*/
     &    '41h Incident particle designator is missing.')                       !/*pr         24*/
         if(hitm.eq.'all') then                                                 !/*pr         25*/
            num_primaries_active(id_sec)=0                                      !/*pr         26*/
         else                                                                   !/*pr         27*/
            num_primaries_active(id_sec)=leng(hitm)                             !/*pr         28*/
            do loc_prim=1,num_primaries_active(id_sec)                          !/*pr         29*/
               ik=index(hip,hitm(loc_prim:loc_prim))                            !/*pr         30*/
               list_secs_primaries(id_sec,loc_prim)=ik                          !/*pr         31*/
            enddo                                                               !/*pr         32*/
         endif                                                                  !/*pr         33*/
!                                                                               !/*pr         34*/
!     energy-band boundary or biasing parameter...                              !/*pr         35*/
      else                                                                      !/*pr         36*/
         if (mod(nwc,2).eq.0) then                                              !/*pr         37*/
            energies_and_params(main_spb_offset + len_dynamic_spb_data +        !/*pr         38*/
     &               nwc/2) = ritm                                              !/*pr         39*/
         else                                                                   !/*pr         40*/
            energies_and_params(main_spb_offset + len_dynamic_spb_data +        !/*pr         41*/
     &               num_energy_bands(id_sec) + nwc/2) = ritm                   !/*pr         42*/
         endif                                                                  !/*pr         43*/
      endif                                                                     !/*pr         44*/
#endif                                                                          /*!pr         45*/
      return                                                                    !/*pr         46*/
      end                                                                       !/*pr         47*/
