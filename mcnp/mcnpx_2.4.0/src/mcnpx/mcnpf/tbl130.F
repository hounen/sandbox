!#ident tbl130.F from deck t34c2                                                !/*t34c2       1*/
      subroutine tbl130                                                         !/*t34c2       2*/
!        print table 130: weight balance for each particle in each cell.        !/*t34c2       3*/
      use global_data                                                           !/*t3x         1*/
!                                                                               !/*t3x         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*t3x         3*/
      implicit integer(kindi) (i-n)                                             !/*t3x         4*/
!                                                                               !/*t3x         5*/
      dimension jc(9)                                                           !/*t34c2       5*/
      dimension pt(items_pwb_total),pw(items_pwb_total,9),st(4,9)               !/*t3x         6*/
      character hc*132                                                          !/*t34c2       7*/
!                                                                               !/*t34c2       8*/
!        print table for each particle type (n,p,e only).                       !/*t3x         7*/
      if(dbcn(26).ne.0.)then                                                    !/*t3x         8*/
      label_pwb_physical (ipwb_neutron_nuc_interaction,1)=' '                   !/*t3x         9*/
      label_pwb_physical (ipwb_neutron_tabular_bound,1)=' '                     !/*t3x        10*/
      label_pwb_physical (ipwb_neutron_particle_decay,1)=' '                    !/*t3x        11*/
      label_pwb_physical (ipwb_neutron_tabular_sampl,1)=' '                     !/*t3x        12*/
      label_pwb_physical (ipwb_photon_particle_decay,2)=' '                     !/*t3x        13*/
      label_pwb_physical (ipwb_photon_nuc_interaction,2)=' '                    !/*t3x        14*/
      label_pwb_physical (ipwb_photon_tabular_sampl,2)=' '                      !/*t3x        15*/
      label_pwb_physical (ipwb_electron_nuc_interaction,3)=' '                  !/*t3x        16*/
      label_pwb_physical (ipwb_electron_particle_decay,3)=' '                   !/*t3x        17*/
      endif                                                                     !/*t3x        18*/
      do 200 ip=1,mipt                                                          !/*t3x        19*/
      if(dbcn(26).ne.0..and.ip.gt.3)go to 200                                   !/*t3x        20*/
      if(kpt(ip).eq.0)go to 200                                                 !/*t34c2      11*/
      write(iuo,10)hnp(ip)                                                      !/*t34c2      12*/
   10 format(1h1,a8,28h weight balance in each cell,67x,                        !/*t34c2      13*/
     & 15hprint table 130)                                                      !/*t34c2      14*/
      do 20 k=1,items_pwb_total                                                 !/*t3x        21*/
   20 pt(k)=0.                                                                  !/*t34c2      16*/
!                                                                               !/*t34c2      17*/
!        process data up to 9 columns at a time.                                !/*t34c2      18*/
!        get the next kc columns for nonzero-importance cells ic=jc(kc).        !/*t34c2      19*/
!        ic=mxa+1 is the total over all cells column.                           !/*t34c2      20*/
      ic=0                                                                      !/*t34c2      21*/
   30 kc=0                                                                      !/*t34c2      22*/
   40 ic=ic+1                                                                   !/*t34c2      23*/
      if(ic.le.mxa.and.fim(lfim+ip,ic).eq.0.)go to 40                           !/*t34c2      24*/
      kc=kc+1                                                                   !/*t34c2      25*/
      jc(kc)=ic                                                                 !/*t34c2      26*/
!                                                                               !/*t34c2      27*/
!        load pwb*fpi into the pw array for nonzero-importance cells.           !/*t34c2      28*/
!        accumulate total over all cells, pt.                                   !/*t34c2      29*/
!        write table header information if last column is not total.            !/*t34c2      30*/
      if(ic.gt.mxa)go to 60                                                     !/*t34c2      31*/
      do 50 k=1,items_pwb_total                                                 !/*t3x        22*/
      pw(k,kc)=pwb(lpwb+ip,k,ic)*fpi                                            !/*t34c2      33*/
   50 pt(k)=pt(k)+pw(k,kc)                                                      !/*t34c2      34*/
      if(kc.lt.9)go to 40                                                       !/*t34c2      35*/
      write(iuo,'(/8x,10hcell index,i9,8i12)')(jc(i),i=1,kc)                    !/*t34c2      36*/
      write(iuo,'(7x,11hcell number,i9,8i12)')(ncl(lncl+jc(i)),i=1,kc)          !/*t34c2      37*/
      go to 100                                                                 !/*t34c2      38*/
!                                                                               !/*t34c2      39*/
!        if the last column, kc, is the totals column, move total over          !/*t34c2      40*/
!        cells (pt) to last column (pw) and write header with total.            !/*t34c2      41*/
   60 do 70 k=1,items_pwb_total                                                 !/*t3x        23*/
   70 pw(k,kc)=pt(k)                                                            !/*t34c2      43*/
      if(kc.eq.1)                                                               !/*t34c2      44*/
     & write(iuo,'(/8x,10hcell index/7x,11hcell number,4x,5htotal)')            !/*t34c2      45*/
      if(kc.eq.1)go to 100                                                      !/*t34c2      46*/
      hc=' '                                                                    !/*t34c2      47*/
      write(iuo,'(/8x,10hcell index,i9,8i12)')(jc(i),i=1,kc-1)                  !/*t34c2      48*/
      write(hc,'(7x,11hcell number,i9,8i12)')(ncl(lncl+jc(i)),i=1,kc-1)         !/*t34c2      49*/
      hc(12*kc+11:12*kc+15)='total'                                             !/*t34c2      50*/
      write(iuo,'(a128)')hc                                                     !/*t34c2      51*/
!                                                                               !/*t34c2      52*/
!        write the weight balance in each cell for ie = 1/2/3 =                 !/*t3x        24*/
!        external/variance reduction/physical events.                           !/*t3x        25*/
  100 do 150 ie=1,3                                                             !/*t3x        26*/
!                                                                               !/*t3x        27*/
      if (ie.eq.1) then                                                         !/*t3x        28*/
         write(iuo,'(/17h external events:)')                                   !/*t3x        29*/
         i1=itm_pwb_external_begin                                              !/*t3x        30*/
         i2=itm_pwb_external_end                                                !/*t3x        31*/
         do 110 i=i1,i2                                                         !/*t3x        32*/
  110    write(iuo,300)label_pwb_external(i),(pw(i,k),k=1,kc)                   !/*t3x        33*/
!                                                                               !/*t3x        34*/
      else if (ie.eq.2) then                                                    !/*t3x        35*/
         write(iuo,'(/27h variance reduction events:)')                         !/*t3x        36*/
         i1=itm_pwb_variance_begin                                              !/*t3x        37*/
         i2=itm_pwb_variance_end                                                !/*t3x        38*/
         do 120 i=i1,i2                                                         !/*t3x        39*/
         if(label_pwb_variance(i).eq.' ')go to 120                              !/*t3x        40*/
         if(ip.gt.2.and.(label_pwb_variance(i).eq.'dxtran'.or.                  !/*t3x        41*/
     &      label_pwb_variance(i).eq.'forced collisions'.or.                    !/*t3x        42*/
     &      label_pwb_variance(i).eq.'exp. transform'))go to 120                !/*t3x        43*/
         write(iuo,300)label_pwb_variance(i),(pw(i,k),k=1,kc)                   !/*t3x        44*/
  120    continue                                                               !/*t3x        45*/
!                                                                               !/*t3x        46*/
      else if (ie.eq.3) then                                                    !/*t3x        47*/
         write(iuo,'(/17h physical events:)')                                   !/*t3x        48*/
         i1=itm_pwb_physical_begin                                              !/*t3x        49*/
         i2=itm_pwb_physical_end                                                !/*t3x        50*/
         iq=min(ip,4)                                                           !/*t3x        51*/
         if(ip.gt.3.and.charge(ip).ne.zero)iq=5                                 !/*t3x        52*/
         do 130 i=i1,i2                                                         !/*t3x        53*/
         if(label_pwb_physical(i,iq).eq.' ')go to 130                           !/*t3x        54*/
         write(iuo,300)label_pwb_physical(i,iq),(pw(i,k),k=1,kc)                !/*t3x        59*/
  130    continue                                                               !/*t3x        60*/
      endif                                                                     !/*t3x        61*/
!                                                                               !/*t3x        62*/
!        get totals for external/variance/physical events.                      !/*t3x        63*/
      write(iuo,'(18x,9a12)')('  ----------',k=1,kc)                            !/*t3x        64*/
      do 140 k=1,kc                                                             !/*t3x        65*/
      st(ie,k)=0.                                                               !/*t3x        66*/
      do 140 i=i1,i2                                                            !/*t3x        67*/
  140 st(ie,k)=st(ie,k)+pw(i,k)                                                 !/*t3x        68*/
      write(iuo,300)'            total',(st(ie,k),k=1,kc)                       !/*t3x        69*/
  150 continue                                                                  !/*t3x        70*/
!                                                                               !/*t34c2     128*/
!        write the table footer information.                                    !/*t34c2     129*/
  180 ec=0.                                                                     !/*t34c2     130*/
      do 190 k=1,kc                                                             !/*t34c2     131*/
      st(4,k)=st(1,k)+st(2,k)+st(3,k)                                           !/*t34c2     132*/
      er=abs(st(1,k))+abs(st(2,k))+abs(st(3,k))                                 !/*t34c2     133*/
      if(abs(st(4,k)).lt.1.e-10*er)st(4,k)=0.                                   !/*t34c2     134*/
  190 ec=max(ec,abs(st(4,k)))                                                   !/*t34c2     135*/
      if(ec.gt.0.)then                                                          !/*t34c2     136*/
         write(iuo,'(/18x,9a12)')('  ----------',k=1,kc)                        !/*t34c2     137*/
         write(iuo,300)'            total',(st(4,k),k=1,kc)                     !/*t34c2     138*/
         write(iuo,'(1h )')                                                     !/*t34c2     139*/
      endif                                                                     !/*t34c2     140*/
!                                                                               !/*t34c2     141*/
!        get next set of 9 cells.                                               !/*t34c2     142*/
      if(ic.le.mxa)go to 30                                                     !/*t34c2     143*/
!                                                                               !/*t34c2     144*/
!        end of loop over particle type.                                        !/*t34c2     145*/
  200 continue                                                                  !/*t34c2     146*/
  300 format(1x,a17,1p9e12.4)                                                   !/*t34c2     147*/
      return                                                                    !/*t34c2     148*/
      end                                                                       !/*t34c2     149*/
