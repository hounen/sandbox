!#ident findlv.F from deck fv                                                   !/*fv          1*/
      subroutine findlv                                                         !/*fv          2*/
!        set up icl, lev, and udt for sursrc or sourck.                         !/*fv          3*/
      use global_data                                                           !/*fvx         1*/
!                                                                               !/*fvx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*fvx         3*/
      implicit integer(kindi) (i-n)                                             !/*fvx         4*/
!                                                                               !/*fvx         5*/
!                                                                               !/*fv          5*/
      jt=0                                                                      !/*fv          6*/
      mm=0                                                                      !/*fv          7*/
      if(jsu.ne.0)mm=2                                                          !/*fv          8*/
      if(icl.gt.0)go to 50                                                      !/*fv          9*/
      ju=0                                                                      !/*fv         10*/
!                                                                               !/*fv         11*/
!        look for the cell in the list of previously found cells.               !/*fv         12*/
!        jt=cell icl universe;  ju=present location universe.                   !/*fv         13*/
   10 js=1                                                                      !/*fv         14*/
      do 20 i=1,nlse                                                            !/*fv         15*/
      icl=lse(klse+i)                                                           !/*fv         16*/
      if(junf.ne.0)jt=abs(jun(ljun+icl))                                        !/*fv         17*/
      if(jt.eq.ju)call chkcel(icl,mm,js)                                        !/*fv         18*/
   20 if(js.eq.0)go to 50                                                       !/*fv         19*/
!                                                                               !/*fv         20*/
!        look for the cell among all cells of the universe.                     !/*fv         21*/
      do 30 icl=1,mxa                                                           !/*fv         22*/
      if(junf.ne.0)jt=abs(jun(ljun+icl))                                        !/*fv         23*/
      if(jt.eq.ju)call chkcel(icl,mm,js)                                        !/*fv         24*/
   30 if(js.eq.0)go to 40                                                       !/*fv         25*/
      call expirx(0,'findlv','source is not in any cell.')                      !/*fv         26*/
      return                                                                    !/*fv         27*/
!                                                                               !/*fv         28*/
!        add the cell to the list of found cells.                               !/*fv         29*/
   40 nlse=nlse+1                                                               !/*fv         30*/
      lse(klse+nlse)=icl                                                        !/*fv         31*/
!                                                                               !/*fv         32*/
!        move into the universe, if any, that fills the cell.                   !/*fv         33*/
   50 if(junf.eq.0)return                                                       !/*fv         34*/
   60 jsu=-abs(jsu)                                                             !/*fv4c        1*/
      if(lat(llat+1,icl).ne.0)call findel(icl)                                  !/*fv4c        2*/
      jsu=abs(jsu)                                                              !/*fv4c        3*/
      if(mfl(lmfl+1,icl).eq.0)return                                            !/*fv         36*/
      j=-mfl(lmfl+1,icl)                                                        !/*fv         37*/
      if(j.gt.0)go to 70                                                        !/*fv         38*/
      ju=-j                                                                     !/*fv         39*/
      call dunlev(icl)                                                          !/*fv         40*/
      if(mfl(lmfl+2,icl).eq.0)go to 10                                          !/*fv         41*/
      icl=mfl(lmfl+2,icl)                                                       !/*fv         42*/
      go to 60                                                                  !/*fv         43*/
   70 n=3+iii-laf(j+1,1)+laf(j+1,2)*(jjj-laf(j+2,1)+laf(j+2,2)*                 !/*fv         44*/
     & (kkk-laf(j+3,1)))                                                        !/*fv         45*/
      if(laf(j+1,n).eq.abs(jun(ljun+icl)))return                                !/*fv         46*/
      ju=laf(j+1,n)                                                             !/*fv         47*/
      call dunlev(icl)                                                          !/*fv         48*/
      if(laf(j+2,n).eq.0)go to 10                                               !/*fv         49*/
      icl=laf(j+2,n)                                                            !/*fv         50*/
      go to 60                                                                  !/*fv         51*/
      end                                                                       !/*fv         52*/
