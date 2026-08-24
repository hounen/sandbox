!     $Id: sec_par_bias.F,v 1.4 2002/03/27 18:16:20 gwm Exp $                 !/*se          1*/
      subroutine sec_par_bias(id_secondary,id_primary,e_secondary,              !/*se          2*/
     &                         nk,np,kp,nb)                                     !/*sea         1*/
!     Sample secondary-particle biasing scheme if applicable.                   !/*se          4*/
!                                                                               !/*se          5*/
!     Input arguments (not altered by routine):                                 !/*se          6*/
!        id_secondary = IPT value of the secondary particle.                    !/*se          7*/
!        id_primary = IPT value of the primary particle.                        !/*se          8*/
!        e_secondary = Energy of the secondary particle.                        !/*se          9*/
!        nk = summary table PAX index.                                          !/*sea         2*/
!        np = particle weight balance PWB index.                                !/*sea         3*/
!        kp = weight balance by nuclide PAN nuclide index.                      !/*sea         4*/
!        nb = reaction type for bankit(nb) call.                                !/*sea         5*/
!                                                                               !/*se         10*/
!     Local arguments (always set by routine):                                  !/*sea         6*/
!        i_factor = Integer biasing factor, always >= 0,                        !/*se         12*/
!                   not necessarily == int(f_factor).                           !/*se         13*/
!        f_factor = Floating point biasing factor, always > 0.                  !/*se         14*/
!                   The weight of the secondary track(s) should be              !/*se         15*/
!                   divided by this factor to maintain a fair game              !/*se         16*/
!                   (same definition even when i_factor == 0).                  !/*se         17*/
!                                                                               !/*se         18*/
#ifdef SPABI                                                                    /*!se         19*/
      use global_data                                                           !/*sea         7*/
!                                                                               !/*sea         8*/
      implicit real(kindr) (a-h,o-z)                                            !/*sea         9*/
      implicit integer(kindi) (i-n)                                             !/*sea        10*/
!                                                                               !/*sea        11*/
      wt=wgt                                                                    !/*sea        12*/
      ga=gamef                                                                  !/*sea        13*/
!                                                                               !/*se         21*/
!     Is biasing requested for this secondary particle type?                    !/*se         22*/
      if (num_energy_bands(id_secondary).gt.0) then                             !/*se         23*/
!        Determine eligibility for biasing.                                     !/*se         24*/
         if (num_primaries_active(id_secondary).gt.0) then                      !/*se         25*/
            do loc_primary=1,num_primaries_active(id_secondary)                 !/*se         26*/
               if (list_secs_primaries(id_secondary,loc_primary) .eq.           !/*se         27*/
     &             id_primary) go to 10                                         !/*se         28*/
            enddo                                                               !/*se         29*/
   10       continue                                                            !/*se         30*/
         else                                                                   !/*se         31*/
            loc_primary=0                                                       !/*se         32*/
         endif                                                                  !/*se         33*/
!                                                                               !/*se         34*/
!        Determine which energy band, or off the table.                         !/*se         35*/
         if (loc_primary.le.num_primaries_active(id_secondary)) then            !/*se         36*/
!                                                                               !/*se         37*/
            do loc_energy=1,num_energy_bands(id_secondary)                      !/*se         38*/
               if (energies_and_params(main_spb_offset +                        !/*se         39*/
     &             interior_offset(id_secondary) + loc_energy) .gt. erg)        !/*se         40*/
     &             go to 20                                                     !/*se         41*/
            enddo                                                               !/*se         42*/
!                                                                               !/*se         43*/
   20       continue                                                            !/*se         44*/
         else                                                                   !/*se         45*/
            loc_energy=num_energy_bands(id_secondary)+1                         !/*se         46*/
         endif                                                                  !/*se         47*/
!                                                                               !/*se         48*/
!        Do the biasing if still eligible.                                      !/*se         49*/
         if (loc_energy.le.num_energy_bands(id_secondary)) then                 !/*se         50*/
            ft = energies_and_params(main_spb_offset +                          !/*se         51*/
     &           interior_offset(id_secondary) +                                !/*se         52*/
     &           num_energy_bands(id_secondary) + loc_energy)                   !/*se         53*/
!                                                                               !/*se         54*/
            if (ft.ge.one) then                                                 !/*se         55*/
!              Splitting...                                                     !/*se         56*/
               i_factor = nint(ft)                                              !/*se         57*/
               f_factor = i_factor                                              !/*se         58*/
            else                                                                !/*se         59*/
!              Roulette...                                                      !/*se         60*/
               f_factor = ft                                                    !/*se         61*/
               if (rang().lt.ft) then                                           !/*se         62*/
                  i_factor = 1                                                  !/*se         63*/
               else                                                             !/*se         64*/
                  i_factor = 0                                                  !/*se         65*/
               endif                                                            !/*se         66*/
            endif                                                               !/*se         67*/
         else                                                                   !/*se         68*/
            i_factor = 1                                                        !/*se         69*/
            f_factor = one                                                      !/*se         70*/
         endif                                                                  !/*se         71*/
      else                                                                      !/*se         72*/
         i_factor = 1                                                           !/*se         73*/
         f_factor = one                                                         !/*se         74*/
      endif                                                                     !/*se         75*/
!                                                                               !/*sea        14*/
!     Adjust particle quantities for biasing.                                   !/*sea        15*/
      npa = npa*i_factor                                                        !/*sea        16*/
      if (i_factor.gt.0.and.f_factor.ne.one) then                               !/*sea        17*/
         wgt = wgt/f_factor                                                     !/*sea        18*/
         gamef = gamef*f_factor                                                 !/*sea        19*/
      endif                                                                     !/*sea        20*/
!                                                                               !/*sea        21*/
!     TEMPORARY expedient: Use the igain_photon_from_neutrons category.         !/*sea        22*/
!     There is no secondary-particle-biasing PAX/PWB category.                  !/*sea        23*/
      if (f_factor.ge.one) then                                                 !/*sea        24*/
         call gain_pax (nk,ipt,npa-1,zero,zero)                                 !/*sea        25*/
      else if (i_factor.eq.0) then                                              !/*sea        26*/
         call gain_pax (nk, ipt, -1, -wt, -wt*erg)                              !/*sea        27*/
         if(np.gt.0)call contrib_pwb (ipt,np,icl,-wt)                           !/*sea        28*/
      else                                                                      !/*sea        29*/
         call gain_pax (igain_photon_from_neutrons, 2,                          !/*sea        30*/
     &        npa-1, wgt-wt, (wgt-wt)*erg)                                      !/*sea        31*/
         if(np.gt.0)call contrib_pwb (ipt,np,icl,wgt-wt)                        !/*sea        32*/
      endif                                                                     !/*sea        33*/
!                                                                               !/*sea        34*/
      if(kp.ne.0) then                                                          !/*sea        35*/
         if(kp.gt.0) then                                                       !/*sea        36*/
            pan(kpan+2,4,kp)=pan(kpan+2,4,kp)+npa-1                             !/*sea        37*/
            pcc(kpcc+1,icl)=pcc(kpcc+1,icl)+npa-1                               !/*sea        38*/
            do 30 i=1,15                                                        !/*sea        39*/
   30       if(erg.gt.ebl(lebl+i))go to 40                                      !/*sea        40*/
   40       febl(kfeb+1,i)=febl(kfeb+1,i)+npa-1                                 !/*sea        41*/
         elseif(kp.eq.-1) then                                                  !/*sea        42*/
            pan(kpan+2,4,mpan)=pan(kpan+2,4,mpan)+npa-1                         !/*sea        43*/
            pcc(kpcc+1,icl)=pcc(kpcc+1,icl)+npa-1                               !/*sea        44*/
            febl(kfeb+1,jgp)=febl(kfeb+1,jgp)+npa-1                             !/*sea        45*/
         elseif(kp.eq.-2) then                                                  !/*sea        46*/
            pan(kpan+2,4,mpan)=pan(kpan+2,4,mpan)+npa-1                         !/*sea        47*/
         elseif(kp.eq.-3) then                                                  !/*sea        48*/
            pan(kpan+2,5,mpan)=pan(kpan+2,5,mpan)+npa-1                         !/*sea        49*/
         endif                                                                  !/*sea        50*/
      endif                                                                     !/*sea        51*/
!                                                                               !/*sea        52*/
      if(npa.gt.0)call bankit(nb)                                               !/*sea        53*/
      npa=1                                                                     !/*sea        54*/
      wgt=wt                                                                    !/*sea        55*/
      gamef=ga                                                                  !/*sea        56*/
#endif                                                                          /*!se         79*/
      return                                                                    !/*se         80*/
      end                                                                       !/*se         81*/
