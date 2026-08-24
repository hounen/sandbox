!#ident tbl140.F from deck t44c2                                                !/*t44c2       1*/
      subroutine tbl140                                                         !/*t44c2       2*/
!        print table 140: neutron/photon activity of each nuclide in            !/*t44c2       3*/
!        each cell.                                                             !/*t44c2       4*/
      use global_data                                                           !/*t4x         1*/
!                                                                               !/*t4x         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*t4x         3*/
      implicit integer(kindi) (i-n)                                             !/*t4x         4*/
!                                                                               !/*t4x         5*/
      parameter (ka=4,kb=8,kc=3,kd=8,kt=9)                                      !/*t44c2       6*/
      character ha(2,ka)*12,hb(2,kb)*12,hc(2,kc)*12,hd(2,kd)*12,                !/*t44c2       7*/
     & hh*12,hn*10                                                              !/*t44c2       8*/
      dimension st(9)                                                           !/*t44c2       9*/
!                                                                               !/*t44c2      10*/
!        character storage of common header labels.                             !/*t44c2      11*/
      data  ha/                                                                 !/*t44c2      12*/
     &     '  cell',       ' index',                                            !/*t44c2      13*/
     &     '  cell',       '  name',                                            !/*t44c2      14*/
     &     '   nuclides',  ' ',                                                 !/*t44c2      15*/
     &     '     atom',    ' fraction'/                                         !/*t44c2      16*/
!                                                                               !/*t44c2      17*/
!        character storage for neutron table header labels.                     !/*t44c2      18*/
      data hb/                                                                  !/*t44c2      19*/
     &     '       total', '  collisions',                                      !/*t44c2      20*/
     &     '  collisions', '    * weight',                                      !/*t44c2      21*/
     &     '   wgt. lost', '  to capture',                                      !/*t44c2      22*/
     &     '   wgt. gain', '  by fission',                                      !/*t44c2      23*/
     &     '   wgt. gain', '   by (n,xn)',                                      !/*t44c2      24*/
     &     '     photons', '    produced',                                      !/*t44c2      25*/
     &     '  photon wgt', '    produced',                                      !/*t44c2      26*/
     &     '  avg photon', '      energy'/                                      !/*t44c2      27*/
!                                                                               !/*t44c2      28*/
!        character storage for photoatomic table header labels.                 !/*t44c2      29*/
      data hc/                                                                  !/*t44c2      30*/
     &     '       total', '  collisions',                                      !/*t44c2      31*/
     &     '  collisions', '    * weight',                                      !/*t44c2      32*/
     &     '   wgt. lost', '  to capture'/                                      !/*t44c2      33*/
!                                                                               !/*t44c2      34*/
!        character storage for photonuclear table header labels.                !/*t44c2      35*/
      data hd/                                                                  !/*t44c2      36*/
     &     '       total', '  collisions',                                      !/*t44c2      37*/
     &     '  collisions', '    * weight',                                      !/*t44c2      38*/
     &     '     photons', '    produced',                                      !/*t44c2      39*/
     &     '  photon wgt', '    produced',                                      !/*t44c2      40*/
     &     '  avg photon', '      energy',                                      !/*t44c2      41*/
     &     '    neutrons', '    produced',                                      !/*t44c2      42*/
     &     ' neutron wgt', '    produced',                                      !/*t44c2      43*/
     &     ' avg neutron', '      energy'/                                      !/*t44c2      44*/
!                                                                               !/*t44c2      45*/
!**************************** neutrons *********************************        !/*t44c2      46*/
!                                                                      *        !/*t44c2      47*/
!        print activity per nuclide per cell for neutrons.                      !/*t44c2      48*/
      if(kpt(1).eq.0)go to 110                                                  !/*t44c2      49*/
      write(iuo,10)                                                             !/*t44c2      50*/
   10 format(48h1neutron activity of each nuclide in each cell, ,               !/*t44c2      51*/
     & 19hper source particle,37x,15hprint table 140/)                          !/*t44c2      52*/
      write(iuo,'(1x,2a6,a11,a9,8a12)')(ha(1,i),i=1,ka),(hb(1,i),i=1,kb)        !/*t44c2      53*/
      write(iuo,'(1x,2a6,a11,a9,8a12)')(ha(2,i),i=1,ka),(hb(2,i),i=1,kb)        !/*t44c2      54*/
      do 20 i=1,kt                                                              !/*t44c2      55*/
   20 st(i)=0.                                                                  !/*t44c2      56*/
!                                                                               !/*t44c2      57*/
!        loop over all cells with materials and finite importance.              !/*t44c2      58*/
      do 50 ic=1,mxa                                                            !/*t44c2      59*/
      im=mat(lmat+ic)                                                           !/*t44c2      60*/
      if(im.eq.0.or.fim(lfim+1,ic).eq.0)go to 50                                !/*t44c2      61*/
      write(iuo,'(1x)')                                                         !/*t44c2      62*/
      write(hh,'(2i6)')ic,ncl(lncl+ic)                                          !/*t44c2      63*/
      ip=ipan(lipa+ic)                                                          !/*t44c2      64*/
!                                                                               !/*t44c2      65*/
!        loop over all nuclides.                                                !/*t44c2      66*/
      do 40 ii=jmd(ljmd+im),jmd(ljmd+im+1)-1                                    !/*t44c2      67*/
      it=lme(llme+1,ii)                                                         !/*t44c2      68*/
      if(it.eq.0)go to 40                                                       !/*t44c2      69*/
      call zaid(2,hn,ixl(lixl+1,it))                                            !/*t44c2      70*/
!                                                                               !/*t44c2      71*/
!        print each nuclide in the cell with associated data.                   !/*t44c2      72*/
      e1=0.                                                                     !/*t44c2      73*/
      if(pan(lpan+1,7,ip).ne.0.)e1=pan(lpan+1,8,ip)/pan(lpan+1,7,ip)            !/*t44c2      74*/
      write(iuo,'(1x,a12,1x,a10,1pe9.2,i12,1p4e12.4,i12,1p2e12.4)')             !/*t44c2      75*/
     & hh,hn,fme(lfme+ii),nint(pan(lpan+1,1,ip)),fpi*pan(lpan+1,2,ip),          !/*t44c2      76*/
     & fpi*pan(lpan+1,3,ip),fpi*pan(lpan+1,4,ip),fpi*pan(lpan+1,5,ip),          !/*t44c2      77*/
     & nint(pan(lpan+1,6,ip)),fpi*pan(lpan+1,7,ip),e1                           !/*t44c2      78*/
      hh=' '                                                                    !/*t44c2      79*/
!                                                                               !/*t44c2      80*/
!        sum activity over all nuclides in all cells.                           !/*t44c2      81*/
      do 30 i=1,kb                                                              !/*t44c2      82*/
   30 st(i)=st(i)+pan(lpan+1,i,ip)                                              !/*t44c2      83*/
   40 ip=ip+1                                                                   !/*t44c2      84*/
   50 continue                                                                  !/*t44c2      85*/
!                                                                               !/*t44c2      86*/
!        print the total over all nuclides and all cells.                       !/*t44c2      87*/
      e1=0.                                                                     !/*t44c2      88*/
      if(st(7).ne.0.)e1=st(8)/st(7)                                             !/*t44c2      89*/
      write(iuo,'(/8x,5htotal,20x,i12,1p4e12.4,i12,2e12.4)')                    !/*t44c2      90*/
     & nint(st(1)),(fpi*st(i),i=2,5),nint(st(6)),fpi*st(7),e1                   !/*t44c2      91*/
!                                                                               !/*t44c2      92*/
!        compute and print the sum over all cells by nuclide.                   !/*t44c2      93*/
      write(iuo,'(//33h  total over all cells by nuclide,8a12)')                !/*t44c2      94*/
     & (hb(1,i),i=1,kb)                                                         !/*t44c2      95*/
      write(iuo,'(33x,8a12/)')(hb(2,i),i=1,kb)                                  !/*t44c2      96*/
!                                                                               !/*t44c2      97*/
!        loop over each neutron nuclide (in) to get totals.                     !/*t44c2      98*/
      do 100 in=1,mxe                                                           !/*t44c2      99*/
      call zaid(2,hn,ixl(lixl+1,in))                                            !/*t44c2     100*/
      if(hn(10:10).ne.'c'.and.hn(10:10).ne.'d'.and.                             !/*t44c2     101*/
     & hn(10:10).ne.'m')go to 100                                               !/*t44c2     102*/
      do 60 i=1,kt                                                              !/*t44c2     103*/
   60 st(i)=0.                                                                  !/*t44c2     104*/
!                                                                               !/*t44c2     105*/
!        search all nuclides (it) of all cells (ic) to find matches             !/*t44c2     106*/
!        with nuclide (in) and accumulate and print activity (st).              !/*t44c2     107*/
      do 90 ic=1,mxa                                                            !/*t44c2     108*/
      im=mat(lmat+ic)                                                           !/*t44c2     109*/
      if(im.eq.0.or.fim(lfim+1,ic).eq.0)go to 90                                !/*t44c2     110*/
      ip=ipan(lipa+ic)                                                          !/*t44c2     111*/
      do 80 ii=jmd(ljmd+im),jmd(ljmd+im+1)-1                                    !/*t44c2     112*/
      it=lme(llme+1,ii)                                                         !/*t44c2     113*/
      if(it.ne.in)go to 80                                                      !/*t44c2     114*/
      do 70 i=1,kb                                                              !/*t44c2     115*/
   70 st(i)=st(i)+pan(lpan+1,i,ip)                                              !/*t44c2     116*/
   80 ip=ip+1                                                                   !/*t44c2     117*/
   90 continue                                                                  !/*t44c2     118*/
      e1=0.                                                                     !/*t44c2     119*/
      if(st(7).ne.0.)e1=st(8)/st(7)                                             !/*t44c2     120*/
      write(iuo,'(14x,a10,9x,i12,1p4e12.4,i12,2e12.4)')                         !/*t44c2     121*/
     & hn,nint(st(1)),(fpi*st(i),i=2,5),nint(st(6)),fpi*st(7),e1                !/*t44c2     122*/
  100 continue                                                                  !/*t44c2     123*/
!                                                                               !/*t44c2     124*/
!**************************** photoatomic ******************************        !/*t44c2     125*/
!                                                                      *        !/*t44c2     126*/
!        print photoatomic activity per nuclide per cell.                       !/*t44c2     127*/
  110 if(kpt(2).eq.0)return                                                     !/*t44c2     128*/
      write(iuo,120)                                                            !/*t44c2     129*/
  120 format(52h1photoatomic activity of each nuclide in each cell, ,           !/*t44c2     130*/
     & 19hper source particle,33x,15hprint table 140/)                          !/*t44c2     131*/
      write(iuo,'(1x,2a6,a11,a9,3a12)')(ha(1,i),i=1,ka),(hc(1,i),i=1,kc)        !/*t44c2     132*/
      write(iuo,'(1x,2a6,a11,a9,3a12)')(ha(2,i),i=1,ka),(hc(2,i),i=1,kc)        !/*t44c2     133*/
      do 130 i=1,kt                                                             !/*t44c2     134*/
  130 st(i)=0.                                                                  !/*t44c2     135*/
!                                                                               !/*t44c2     136*/
!        loop over all cells with materials and finite importance.              !/*t44c2     137*/
      do 160 ic=1,mxa                                                           !/*t44c2     138*/
      im=mat(lmat+ic)                                                           !/*t44c2     139*/
      if(im.eq.0.or.fim(lfim+2,ic).eq.0)go to 160                               !/*t44c2     140*/
      write(iuo,'(1x)')                                                         !/*t44c2     141*/
      write(hh,'(2i6)')ic,ncl(lncl+ic)                                          !/*t44c2     142*/
      ip=ipan(lipa+ic)                                                          !/*t44c2     143*/
!                                                                               !/*t44c2     144*/
!        loop over all nuclides                                                 !/*t44c2     145*/
      do 150 ii=jmd(ljmd+im),jmd(ljmd+im+1)-1                                   !/*t44c2     146*/
      it=lme(llme+2,ii)                                                         !/*t44c2     147*/
      if(it.eq.0)go to 150                                                      !/*t44c2     148*/
      call zaid(2,hn,ixl(lixl+1,it))                                            !/*t44c2     149*/
!                                                                               !/*t44c2     150*/
!        print each nuclide in the cell with associated data                    !/*t44c2     151*/
      write(iuo,'(1x,a12,1x,a10,1pe9.2,i12,1p3e12.4)')                          !/*t44c2     152*/
     & hh,hn,fme(lfme+ii),nint(pan(lpan+2,1,ip)),fpi*pan(lpan+2,2,ip),          !/*t44c2     153*/
     & fpi*pan(lpan+2,3,ip)                                                     !/*t44c2     154*/
      hh=' '                                                                    !/*t44c2     155*/
!                                                                               !/*t44c2     156*/
!        sum activity over all nuclides in all cells.                           !/*t44c2     157*/
      do 140 i=1,kc                                                             !/*t44c2     158*/
  140 st(i)=st(i)+pan(lpan+2,i,ip)                                              !/*t44c2     159*/
  150 ip=ip+1                                                                   !/*t44c2     160*/
  160 continue                                                                  !/*t44c2     161*/
!                                                                               !/*t44c2     162*/
!        print the total over all nuclides and all cells                        !/*t44c2     163*/
      write(iuo,'(/8x,5htotal,20x,i12,1p2e12.4)')                               !/*t44c2     164*/
     & nint(st(1)),(fpi*st(i),i=2,3)                                            !/*t44c2     165*/
!                                                                               !/*t44c2     166*/
!        compute and print the sum over all cells by nuclide                    !/*t44c2     167*/
      write(iuo,'(//33h  total over all cells by nuclide,3a12)')                !/*t44c2     168*/
     & (hc(1,i),i=1,kc)                                                         !/*t44c2     169*/
      write(iuo,'(33x,3a12/)')(hc(2,i),i=1,kc)                                  !/*t44c2     170*/
!                                                                               !/*t44c2     171*/
!        loop over each neutron nuclide (in) to get totals.                     !/*t44c2     172*/
      do 210 in=1,mxe                                                           !/*t44c2     173*/
      call zaid(2,hn,ixl(lixl+1,in))                                            !/*t44c2     174*/
      if(hn(10:10).ne.'p'.and.hn(10:10).ne.'g')go to 210                        !/*t44c2     175*/
      do 170 i=1,kt                                                             !/*t44c2     176*/
  170 st(i)=0.                                                                  !/*t44c2     177*/
!                                                                               !/*t44c2     178*/
!        search all nuclides (it) of all cells (ic) to find matches             !/*t44c2     179*/
!        with nuclide (in) and accumulate and print activity (st).              !/*t44c2     180*/
      do 200 ic=1,mxa                                                           !/*t44c2     181*/
      im=mat(lmat+ic)                                                           !/*t44c2     182*/
      if(im.eq.0.or.fim(lfim+2,ic).eq.0)go to 200                               !/*t44c2     183*/
      ip=ipan(lipa+ic)                                                          !/*t44c2     184*/
      do 190 ii=jmd(ljmd+im),jmd(ljmd+im+1)-1                                   !/*t44c2     185*/
      it=lme(llme+2,ii)                                                         !/*t44c2     186*/
      if(it.ne.in)go to 190                                                     !/*t44c2     187*/
      do 180 i=1,kc                                                             !/*t44c2     188*/
  180 st(i)=st(i)+pan(lpan+2,i,ip)                                              !/*t44c2     189*/
  190 ip=ip+1                                                                   !/*t44c2     190*/
  200 continue                                                                  !/*t44c2     191*/
      write(iuo,'(14x,a10,9x,i12,1p2e12.4)')                                    !/*t44c2     192*/
     & hn,nint(st(1)),(fpi*st(i),i=2,3)                                         !/*t44c2     193*/
  210 continue                                                                  !/*t44c2     194*/
!                                                                               !/*t44c2     195*/
!**************************** photonuclear *****************************        !/*t44c2     196*/
!                                                                      *        !/*t44c2     197*/
!                                                                               !/*t44c2     198*/
!        print activity per nuclide per cell.                                   !/*t44c2     199*/
      if(ispn.eq.0)return                                                       !/*t44c2     200*/
      write(iuo,220)                                                            !/*t44c2     201*/
  220 format(53h1photonuclear activity of each nuclide in each cell, ,          !/*t44c2     202*/
     & 19hper source particle,32x,15hprint table 140/)                          !/*t44c2     203*/
      write(iuo,'(1x,2a6,a11,a9,8a12)')(ha(1,i),i=1,ka),(hd(1,i),i=1,kd)        !/*t44c2     204*/
      write(iuo,'(1x,2a6,a11,a9,8a12)')(ha(2,i),i=1,ka),(hd(2,i),i=1,kd)        !/*t44c2     205*/
      do 230 i=1,kt                                                             !/*t44c2     206*/
  230 st(i)=0.                                                                  !/*t44c2     207*/
!                                                                               !/*t44c2     208*/
!        loop over all cells with materials and finite importance.              !/*t44c2     209*/
      do 260 ic=1,mxa                                                           !/*t44c2     210*/
      im=mat(lmat+ic)                                                           !/*t44c2     211*/
      if(im.eq.0.or.fim(lfim+2,ic).eq.0)go to 260                               !/*t44c2     212*/
      nl=1                                                                      !/*t44c2     213*/
      write(hh,'(2i6)')ic,ncl(lncl+ic)                                          !/*t44c2     214*/
      ip=ipan(lipa+ic)                                                          !/*t44c2     215*/
!                                                                               !/*t44c2     216*/
!        loop over all nuclides.                                                !/*t44c2     217*/
      do 250 ii=jmd(ljmd+im),jmd(ljmd+im+1)-1                                   !/*t44c2     218*/
      it=lmn(llmn+ii)                                                           !/*t44c2     219*/
      if(it.eq.0)go to 250                                                      !/*t44c2     220*/
      call zaid(2,hn,ixl(lixl+1,it))                                            !/*t44c2     221*/
!                                                                               !/*t44c2     222*/
!        print each nuclide in the cell with associated data.                   !/*t44c2     223*/
      e1=0.                                                                     !/*t44c2     224*/
      if(pan(lpan+3,4,ip).ne.0.)e1=pan(lpan+3,5,ip)/pan(lpan+3,4,ip)            !/*t44c2     225*/
      e2=0.                                                                     !/*t44c2     226*/
      if(pan(lpan+3,7,ip).ne.0.)e2=pan(lpan+3,8,ip)/pan(lpan+3,7,ip)            !/*t44c2     227*/
      if(nl.ne.0)write(iuo,'(1x)')                                              !/*t44c2     228*/
      nl=0                                                                      !/*t44c2     229*/
      write(iuo,'(1x,a12,1x,a10,1pe9.2,i12,e12.4,2(i12,2e12.4))')               !/*t44c2     230*/
     & hh,hn,fme(lfme+ii),nint(pan(lpan+3,1,ip)),fpi*pan(lpan+3,2,ip),          !/*t44c2     231*/
     & nint(pan(lpan+3,3,ip)),fpi*pan(lpan+3,4,ip),e1,                          !/*t44c2     232*/
     & nint(pan(lpan+3,6,ip)),fpi*pan(lpan+3,7,ip),e2                           !/*t44c2     233*/
      hh=' '                                                                    !/*t44c2     234*/
!                                                                               !/*t44c2     235*/
!        sum activity over all nuclides in all cells.                           !/*t44c2     236*/
      do 240 i=1,kd                                                             !/*t44c2     237*/
  240 st(i)=st(i)+pan(lpan+3,i,ip)                                              !/*t44c2     238*/
  250 ip=ip+1                                                                   !/*t44c2     239*/
  260 continue                                                                  !/*t44c2     240*/
!                                                                               !/*t44c2     241*/
!        print the total over all nuclides and all cells.                       !/*t44c2     242*/
      e1=0.                                                                     !/*t44c2     243*/
      if(st(4).ne.0.)e1=st(5)/st(4)                                             !/*t44c2     244*/
      e2=0.                                                                     !/*t44c2     245*/
      if(st(7).ne.0.)e2=st(8)/st(7)                                             !/*t44c2     246*/
      write(iuo,'(/8x,5htotal,20x,i12,1pe12.4,2(i12,2e12.4))')                  !/*t44c2     247*/
     & nint(st(1)),fpi*st(2),nint(st(3)),fpi*st(4),e1,nint(st(6)),              !/*t44c2     248*/
     & fpi*st(7),e2                                                             !/*t44c2     249*/
!                                                                               !/*t44c2     250*/
!        compute and print the sum over all cells by nuclide.                   !/*t44c2     251*/
      write(iuo,'(//33h  total over all cells by nuclide,8a12)')                !/*t44c2     252*/
     & (hd(1,i),i=1,kd)                                                         !/*t44c2     253*/
      write(iuo,'(33x,8a12/)')(hd(2,i),i=1,kd)                                  !/*t44c2     254*/
!                                                                               !/*t44c2     255*/
!        loop over each neutron nuclide (in) to get totals.                     !/*t44c2     256*/
      do 310 in=1,mxe                                                           !/*t44c2     257*/
      call zaid(2,hn,ixl(lixl+1,in))                                            !/*t44c2     258*/
      if(hn(10:10).ne.'u')go to 310                                             !/*t44c2     259*/
      do 270 i=1,kt                                                             !/*t44c2     260*/
  270 st(i)=0.                                                                  !/*t44c2     261*/
!                                                                               !/*t44c2     262*/
!        search all nuclides (it) of all cells (ic) to find matches             !/*t44c2     263*/
!        with nuclide (in) and accumulate and print activity (st).              !/*t44c2     264*/
      do 300 ic=1,mxa                                                           !/*t44c2     265*/
      im=mat(lmat+ic)                                                           !/*t44c2     266*/
      if(im.eq.0.or.fim(lfim+2,ic).eq.0)go to 300                               !/*t44c2     267*/
      ip=ipan(lipa+ic)                                                          !/*t44c2     268*/
      do 290 ii=jmd(ljmd+im),jmd(ljmd+im+1)-1                                   !/*t44c2     269*/
      it=lmn(llmn+ii)                                                           !/*t44c2     270*/
      if(it.ne.in)go to 290                                                     !/*t44c2     271*/
      do 280 i=1,kd                                                             !/*t44c2     272*/
  280 st(i)=st(i)+pan(lpan+3,i,ip)                                              !/*t44c2     273*/
  290 ip=ip+1                                                                   !/*t44c2     274*/
  300 continue                                                                  !/*t44c2     275*/
!                                                                               !/*t44c2     276*/
!        print the total over all nuclides and all cells.                       !/*t44c2     277*/
      e1=0.                                                                     !/*t44c2     278*/
      if(st(4).ne.0.)e1=st(5)/st(4)                                             !/*t44c2     279*/
      e2=0.                                                                     !/*t44c2     280*/
      if(st(7).ne.0.)e2=st(8)/st(7)                                             !/*t44c2     281*/
      write(iuo,'(14x,a10,9x,i12,1pe12.4,2(i12,2e12.4))')hn,                    !/*t44c2     282*/
     & nint(st(1)),fpi*st(2),nint(st(3)),fpi*st(4),e1,nint(st(6)),              !/*t44c2     283*/
     & fpi*st(7),e2                                                             !/*t44c2     284*/
  310 continue                                                                  !/*t44c2     285*/
      return                                                                    !/*t44c2     286*/
      end                                                                       !/*t44c2     287*/
