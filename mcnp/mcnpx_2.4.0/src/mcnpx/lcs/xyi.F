!     $Id: xyi.F,v 1.4 2002/03/27 18:15:27 gwm Exp $                          !/*xy          1*/
      subroutine xyi (ii,jj,kk)                                                 !/*xy          2*/
#include <lcs/double.h>                                                         /*!xy          3*/
#include <lcs/btcom.h>                                                          /*!xy          4*/
      w1=s(ii)*1.d+30                                                           !/*xy          5*/
      w2=s(ii+1)*1.d+30                                                         !/*xy          6*/
      if (abs(iv)-1) 10,20,10                                                   !/*xy          7*/
   10 w3=space(jj)*1.d+30                                                       !/*xy          8*/
      w4=space(jj+1)*1.d+30                                                     !/*xy          9*/
      w5=space(jj+2)*1.d+30                                                     !/*xy         10*/
      w6=space(jj+3)*1.d+30                                                     !/*xy         11*/
      ll=ii+2                                                                   !/*xy         12*/
      if (iv) 40,60,40                                                          !/*xy         13*/
   20 w3=s(ii+2)*1.d+30                                                         !/*xy         14*/
      w4=s(ii+3)*1.d+30                                                         !/*xy         15*/
      w5=space(jj)*1.d+30                                                       !/*xy         16*/
      w6=space(jj+1)*1.d+30                                                     !/*xy         17*/
      ll=ii+4                                                                   !/*xy         18*/
      if (iv) 50,30,30                                                          !/*xy         19*/
   30 nm=7                                                                      !/*xy         20*/
      i7=1                                                                      !/*xy         21*/
   40 mm=7                                                                      !/*xy         22*/
      nn=1                                                                      !/*xy         23*/
      go to 70                                                                  !/*xy         24*/
   50 nm=1                                                                      !/*xy         25*/
      i7=7                                                                      !/*xy         26*/
   60 mm=1                                                                      !/*xy         27*/
      nn=7                                                                      !/*xy         28*/
   70 do 100 i=1,3                                                              !/*xy         29*/
      s(ll)=w1*space(mm)                                                        !/*xy         30*/
      s(ll+1)=w2*space(nn)                                                      !/*xy         31*/
      if (abs(iv)-1) 90,80,90                                                   !/*xy         32*/
   80 s(ll+2)=w3*space(nm)                                                      !/*xy         33*/
      s(ll+3)=w4*space(i7)                                                      !/*xy         34*/
      ll=ll+2                                                                   !/*xy         35*/
      nm=nm+1                                                                   !/*xy         36*/
      i7=i7+1                                                                   !/*xy         37*/
   90 mm=mm+1                                                                   !/*xy         38*/
      nn=nn+1                                                                   !/*xy         39*/
  100 ll=ll+2                                                                   !/*xy         40*/
      if (iv) 110,120,130                                                       !/*xy         41*/
  110 nm=7                                                                      !/*xy         42*/
      i7=1                                                                      !/*xy         43*/
      go to 170                                                                 !/*xy         44*/
  120 mm=1                                                                      !/*xy         45*/
      nn=7                                                                      !/*xy         46*/
      nm=7                                                                      !/*xy         47*/
      i7=7                                                                      !/*xy         48*/
      go to 150                                                                 !/*xy         49*/
  130 if (iv-1) 160,160,140                                                     !/*xy         50*/
  140 mm=7                                                                      !/*xy         51*/
      nn=1                                                                      !/*xy         52*/
      nm=1                                                                      !/*xy         53*/
      i7=7                                                                      !/*xy         54*/
  150 ll=jj+4                                                                   !/*xy         55*/
      go to 180                                                                 !/*xy         56*/
  160 nm=1                                                                      !/*xy         57*/
      i7=7                                                                      !/*xy         58*/
  170 ll=jj                                                                     !/*xy         59*/
  180 do 210 i=1,3                                                              !/*xy         60*/
      space(ll+2)=w5*space(nm)                                                  !/*xy         61*/
      space(ll+3)=w6*space(i7)                                                  !/*xy         62*/
      if (abs(iv)-1) 190,200,190                                                !/*xy         63*/
  190 space(ll)=w3*space(mm)                                                    !/*xy         64*/
      space(ll+1)=w4*space(nn)                                                  !/*xy         65*/
      ll=ll+2                                                                   !/*xy         66*/
      mm=mm+1                                                                   !/*xy         67*/
      nn=nn+1                                                                   !/*xy         68*/
  200 nm=nm+1                                                                   !/*xy         69*/
      i7=i7+1                                                                   !/*xy         70*/
  210 ll=ll+2                                                                   !/*xy         71*/
      ll=kk+2                                                                   !/*xy         72*/
      if (abs(iv)-1) 220,250,220                                                !/*xy         73*/
  220 mm=jj+4                                                                   !/*xy         74*/
      nn=ii+2                                                                   !/*xy         75*/
      do 230 i=kk,ll                                                            !/*xy         76*/
      space(i)=space(mm)+space(mm+1)+space(mm+2)+space(mm+3)+s(nn)+s(nn+        !/*xy         77*/
     & 1)                                                                       !/*xy         78*/
      mm=mm+4                                                                   !/*xy         79*/
  230 nn=nn+2                                                                   !/*xy         80*/
  240 return                                                                    !/*xy         81*/
  250 mm=ii+4                                                                   !/*xy         82*/
      nn=jj+2                                                                   !/*xy         83*/
      do 260 i=kk,ll                                                            !/*xy         84*/
      space(i)=space(nn)+space(nn+1)+s(mm)+s(mm+1)+s(mm+2)+s(mm+3)              !/*xy         85*/
      mm=mm+4                                                                   !/*xy         86*/
  260 nn=nn+2                                                                   !/*xy         87*/
      go to 240                                                                 !/*xy         88*/
      end                                                                       !/*xy         89*/
