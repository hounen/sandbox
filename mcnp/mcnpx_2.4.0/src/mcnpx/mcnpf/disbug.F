!#ident disbug.F from deck db                                                   !/*db          1*/
      subroutine disbug                                                         !/*db          3*/
#ifdef DISSPLA                                                                  /*!dbx         1*/
!        limitations in disspla require the diameters of linear axes to         !/*db          4*/
!        be less than the amount represented by the parameter db.  if           !/*db          5*/
!        the diameter is greater than this amount, the data are scaled.         !/*db          6*/
      use global_data                                                           !/*dbx         2*/
      use mcplot_data                                                           !/*dbx         3*/
!                                                                               !/*dbx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*dbx         5*/
      implicit integer(kindi) (i-n)                                             !/*dbx         6*/
!                                                                               !/*dbx         7*/
      parameter (db=5.9405929e25)                                               !/*db          9*/
!                                                                               !/*db         10*/
!        x axis.                                                                !/*db         11*/
      if(lax.ge.3.and.nfree.eq.1.or.xyzmx(1)-xyzmn(1).le.db)go to 20            !/*db         12*/
      dissf(1)=10.**(-aint(log10(max(abs(xyzmn(1)),abs(xyzmx(1))))))            !/*db         13*/
      ablo(1)=ablo(1)*dissf(1)                                                  !/*db         14*/
      abhi(1)=abhi(1)*dissf(1)                                                  !/*db         15*/
      do 10 i=1,npt(1)                                                          !/*db         16*/
   10 ab1(lab1+i)=ab1(lab1+i)*dissf(1)                                          !/*db         17*/
      xyzmx(1)=xyzmx(1)*dissf(1)                                                !/*db         18*/
      xyzmn(1)=xyzmn(1)*dissf(1)                                                !/*db         19*/
      jlim(1)=0                                                                 !/*db         20*/
!                                                                               !/*db         21*/
!        y axis.                                                                !/*db         22*/
   20 if(abs(lax-3).eq.1.and.nfree.eq.1.or.xyzmx(2)-xyzmn(2).le.db)             !/*db         23*/
     & go to 50                                                                 !/*db         24*/
      dissf(2)=10.**(-aint(log10(max(abs(xyzmn(2)),abs(xyzmx(2))))))            !/*db         25*/
      if(nfree.eq.1)go to 30                                                    !/*db         26*/
      ablo(2)=ablo(2)*dissf(2)                                                  !/*db         27*/
      abhi(2)=abhi(2)*dissf(2)                                                  !/*db         28*/
   30 do 40 i=1,npt(nfree)                                                      !/*db         29*/
      if(nfree.eq.1)ord(lord+i)=ord(lord+i)*dissf(2)                            !/*db         30*/
   40 if(nfree.eq.2)ab2(lab2+i)=ab2(lab2+i)*dissf(2)                            !/*db         31*/
      xyzmx(2)=xyzmx(2)*dissf(2)                                                !/*db         32*/
      xyzmn(2)=xyzmn(2)*dissf(2)                                                !/*db         33*/
      jlim(2)=0                                                                 !/*db         34*/
!                                                                               !/*db         35*/
!        ordinate axis of 3d plot.                                              !/*db         36*/
   50 if(nfree.eq.1.or.j3d.eq.0.or.xyzmx(3)-xyzmn(3).le.db)return               !/*db         37*/
      dissf(3)=10.**(-aint(log10(max(abs(xyzmn(3)),abs(xyzmx(3))))))            !/*db         38*/
      xyzmx(3)=xyzmx(3)*dissf(3)                                                !/*db         39*/
      xyzmn(3)=xyzmn(3)*dissf(3)                                                !/*db         40*/
#endif                                                                          /*!dbx         8*/
      return                                                                    !/*db         41*/
      end                                                                       !/*db         42*/
