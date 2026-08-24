!#ident kdata.F from deck kd                                                    !/*kd          1*/
      function kdata(hd)                                                        !/*kd          2*/
!        return the kind of data in character string hd.                        !/*kd          3*/
!        2 = integer     1 = real or double precision     0 = neither           !/*kd          4*/
      character hd*(*)                                                          !/*kd          5*/
!                                                                               !/*kd          6*/
      kdata=0                                                                   !/*kd          7*/
      kp=1                                                                      !/*kd          8*/
      nd=0                                                                      !/*kd          9*/
      nn=0                                                                      !/*kd         10*/
      nm=0                                                                      !/*kd         11*/
      do 60 ip=1,len(hd)                                                        !/*kd         12*/
!ccc     an "ED" must be upper case.                                            !/*kd4a        1*/
      ih=index('0123456789. +-edED',hd(ip:ip))                                  !/*cor4-2    250*/
      if(ih.eq.12)go to 60                                                      !/*kd         14*/
      go to(10,20,40,50)kp                                                      !/*kd         15*/
   10 kp=2                                                                      !/*kd         16*/
      if(ih.eq.13.or.ih.eq.14)go to 60                                          !/*cor4-2    251*/
   20 if(ih.eq.0.or.ih.gt.11)go to 30                                           !/*kd         18*/
      if(ih.eq.11)nd=nd+1                                                       !/*kd         19*/
      if(ih.ne.11)nn=nn+1                                                       !/*kd         20*/
      go to 60                                                                  !/*kd         21*/
   30 kp=3                                                                      !/*kd         22*/
      if(ih.gt.14)go to 60                                                      !/*cor4-2    252*/
   40 kp=4                                                                      !/*kd         24*/
      if(ih.eq.13.or.ih.eq.14)go to 60                                          !/*cor4-2    253*/
   50 if(ih.eq.0.or.ih.gt.10)return                                             !/*kd         26*/
      nm=nm+1                                                                   !/*kd         27*/
   60 continue                                                                  !/*kd         28*/
      if(kp.ne.1.and.nn.eq.0.or.nd.gt.1.or.kp.ge.3.and.nm.eq.0)return           !/*kd         29*/
      kdata=1                                                                   !/*kd         30*/
      if(kp.eq.1.or.kp.eq.2.and.nd.eq.0)kdata=2                                 !/*kd         31*/
      return                                                                    !/*kd         32*/
      end                                                                       !/*kd         33*/
