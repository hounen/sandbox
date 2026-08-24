!#ident kdarg.F from deck ka                                                    !/*ka          1*/
      function kdarg(hk,jc,m,ha,r)                                              !/*ka          2*/
!        return the type of the next item in hk starting at column jc.          !/*ka          3*/
!        item delimiters are blank, comma, and equals sign.                     !/*ka          4*/
!        return the item left justified in ha.                                  !/*ka          5*/
!        if the item is numeric, return its value in r.                         !/*ka          6*/
!        set jc = item's last column + 1.                                       !/*ka          7*/
!        if there are no more items in hk, return -1 and set jc=81.             !/*ka          8*/
!        if m.ne.0 and item's type is 0, return -2 and don't change jc.         !/*ka          9*/
      use global_parameters                                                     !/*kax         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*kax         2*/
      implicit integer(kindi) (i-n)                                             !/*kax         3*/
      character ha*(*),hb*80,hk*80                                              !/*ka         12*/
!                                                                               !/*ka         13*/
      kdarg=-2                                                                  !/*ka         14*/
      r=0.                                                                      !/*ka         15*/
      iu=jc-1                                                                   !/*ka         16*/
   10 ha=' '                                                                    !/*ka         17*/
      call nxtsym(hk,' ,=',iu+1,it,iu,1)                                        !/*ka4a        1*/
      if(iu.eq.0)go to 20                                                       !/*ka         19*/
      ha=hk(it:iu)                                                              !/*ka         20*/
      if(ha.eq.'='.or.ha.eq.',')go to 10                                        !/*ka         21*/
      k=kdata(ha)                                                               !/*ka         22*/
      if(m.ne.0.and.k.eq.0)return                                               !/*ka         23*/
      kdarg=k                                                                   !/*ka         24*/
      jc=iu+1                                                                   !/*ka         25*/
      if(k.eq.0)return                                                          !/*ka         26*/
      hb=' '                                                                    !/*ka         27*/
      hb(80-iu+it:80)=ha                                                        !/*ka         28*/
      read(hb,'(e80.0)')r                                                       !/*ka         29*/
      return                                                                    !/*ka         30*/
   20 jc=81                                                                     !/*ka         31*/
      kdarg=-1                                                                  !/*ka         32*/
      return                                                                    !/*ka         33*/
      end                                                                       !/*ka         34*/
