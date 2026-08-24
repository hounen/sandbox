!#ident x3dmat.F from deck xy                                                   !/*xy          1*/
      function x3dmat(i)                                                        !/*xy          4*/
#if defined(MCPLOT) && defined(DISSPLA)                                         /*!xyx         1*/
!        return the grid x value for a 3d plot.                                 !/*xy          5*/
!        disspla 3d routines require functions x3dmat and y3dmat.               !/*xy          6*/
      use global_data                                                           !/*xyx         2*/
      use mcplot_data                                                           !/*xyx         3*/
!                                                                               !/*xyx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*xyx         5*/
      implicit integer(kindi) (i-n)                                             !/*xyx         6*/
!                                                                               !/*xyx         7*/
      real x3dmat                                                               !/*xyx         8*/
!                                                                               !/*xy         10*/
      x3dmat=ab1(lab1+ibl(ifree(1),1)+i-1)                                      !/*xy         11*/
#else                                                                           /*!xyx         9*/
      x3dmat=0.                                                                 !/*xyx        10*/
#endif                                                                          /*!xyx        11*/
      return                                                                    !/*xy         12*/
      end                                                                       !/*xyx        12*/
      function y3dmat(i)                                                        !/*xyx        13*/
#if defined(MCPLOT) && defined(DISSPLA)                                         /*!xyx        14*/
!                                                                               !/*xy         13*/
!        return the grid y value for a 3d plot.                                 !/*xy         14*/
      use global_data                                                           !/*xyx        15*/
      use mcplot_data                                                           !/*xyx        16*/
!                                                                               !/*xyx        17*/
      implicit real(kindr) (a-h,o-z)                                            !/*xyx        18*/
      implicit integer(kindi) (i-n)                                             !/*xyx        19*/
!                                                                               !/*xyx        20*/
      real y3dmat                                                               !/*xyx        21*/
      y3dmat=ab2(lab2+ibl(ifree(2),1)+i-1)                                      !/*xy         16*/
#else                                                                           /*!xyx        22*/
      y3dmat=0.                                                                 !/*xyx        23*/
#endif                                                                          /*!xyx        24*/
      return                                                                    !/*xy         17*/
      end                                                                       !/*xy         18*/
