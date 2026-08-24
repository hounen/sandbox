!#ident jbin.F from deck bi                                                     !/*bi          1*/
      function jbin(i,va)                                                       !/*bi          2*/
!        return which bin of bin type i contains va.                            !/*bi          3*/
      use global_data                                                           !/*bix         1*/
!                                                                               !/*bix         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*bix         3*/
      implicit integer(kindi) (i-n)                                             !/*bix         4*/
!                                                                               !/*bix         5*/
      character ha(5)*3,hb(5)*6                                                 !/*bi4c        1*/
      data ha/'cos','erg','tme','val',' '/,                                     !/*bi4c        2*/
     & hb/'angle','energy',' time',' user',' '/                                 !/*bi4c        3*/
!                                                                               !/*bi          5*/
      if(img.ne.0.and.i.eq.7.and.iptal(lipt+7,4,ital)-1.eq.igm)go to 50         !/*bi          6*/
      ic=iptal(lipt+i,1,ital)                                                   !/*bi          7*/
      ib=ic+iptal(lipt+i,4,ital)-1                                              !/*bi          8*/
      if(va.gt.tds(ib))go to 40                                                 !/*bi          9*/
   10 if(ib-ic.eq.1)go to 30                                                    !/*bi         10*/
      ih=(ic+ib)/2                                                              !/*bi         11*/
      if(va.le.tds(ih))go to 20                                                 !/*bi         12*/
      ic=ih                                                                     !/*bi         13*/
      go to 10                                                                  !/*bi         14*/
   20 ib=ih                                                                     !/*bi         15*/
      go to 10                                                                  !/*bi         16*/
   30 jbin=ib-iptal(lipt+i,1,ital)                                              !/*bi         17*/
      return                                                                    !/*bi         18*/
!                                                                               !/*bi         19*/
!        increment the ntbb count if the tally is beyond the last bin.          !/*bi         20*/
   40 jbin=0                                                                    !/*bi         21*/
      j=i-5                                                                     !/*bi         22*/
      if(i.eq.3)j=4                                                             !/*bi         23*/
      call errprn(1,ntbb(lntb+j,ital),3,zero+jptal(ljpt+1,ital),va,             !/*bi4b        1*/
     & 'tal',ha(j),'tally not scored beyond last '//hb(j)//' bin.')             !/*bi4a        5*/
      return                                                                    !/*bi         25*/
   50 jbin=jgp                                                                  !/*bi         26*/
      return                                                                    !/*bi         27*/
      end                                                                       !/*bi         28*/
