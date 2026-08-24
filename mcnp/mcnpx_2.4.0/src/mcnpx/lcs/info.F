!     $Id: info.F,v 1.4 2002/03/27 18:15:08 gwm Exp $                         !/*in          1*/
      subroutine info                                                           !/*in          2*/
!L*ca double                                                                    !/*in          3*/
!L    ==================================================================        !/*in          4*/
!L*ca lpt$                                                                      !/*in          5*/
!L*ca concom                                                                    !/*in          6*/
!L*ca komon                                                                     !/*in          7*/
!L*ca rdata0                                                                    !/*in          8*/
!L*ca inout                                                                     !/*in          9*/
!L*ca rucom                                                                     !/*in         10*/
!L      write (io,*) ' nocas  ',nocas                                           !/*in         11*/
!L      write (io,*) ' name7  ',name7                                           !/*in         12*/
!L      write (io,*) ' tip7  ',tip7                                             !/*in         13*/
!L      write (io,*) ' mtl  ',mtl                                               !/*in         14*/
!L      write (io,*) ' nmed7  ',nmed7                                           !/*in         15*/
!L      write (io,*) ' ibold7  ',ibold7                                         !/*in         16*/
!L      write (io,*) ' iblz7  ',iblz7                                           !/*in         17*/
!L      write (io,*) ' e7  ',e7                                                 !/*in         18*/
!L      write (io,*) ' ec7  ',ec7                                               !/*in         19*/
!L      write (io,*) ' u7  ',u7                                                 !/*in         20*/
!L      write (io,*) ' v7  ',v7                                                 !/*in         21*/
!L      write (io,*) ' w7  ',w7                                                 !/*in         22*/
!L      write (io,*) ' wt7  ',wt7                                               !/*in         23*/
!L      write (io,*) ' x7  ',x7                                                 !/*in         24*/
!L      write (io,*) ' y7  ',y7                                                 !/*in         25*/
!L      write (io,*) ' z7  ',z7                                                 !/*in         26*/
!L      write (io,*) ' xc7  ',xc7                                               !/*in         27*/
!L      write (io,*) ' yc7  ',yc7                                               !/*in         28*/
!L      write (io,*) ' zc7  ',zc7                                               !/*in         29*/
!L      write (io,*) ' tc7  ',tc7                                               !/*in         30*/
!L      write (io,*) ' jaj7  ',jaj7                                             !/*in         31*/
!L      write (io,*) ' lelem  ',lelem                                           !/*in         32*/
!L      write (io,*) ' namax  ',namax                                           !/*in         33*/
!L      write (io,*) ' nbnk  ',nbnk                                             !/*in         34*/
!LC     if (jaj7.ne.0) then                                                     !/*in         35*/
!LC       call getxmu (x7,y7,z7,u7,v7,w7,jaj7,xmu7)                             !/*in         36*/
!LC     else                                                                    !/*in         37*/
!LC       xmu7=dp0                                                              !/*in         38*/
!LC     endif                                                                   !/*in         39*/
!L      write (io,*) ' xmu7  ',xmu7                                             !/*in         40*/
!L      write (io,*) ' sfac7  ',sfac7                                           !/*in         41*/
!L      write (io,*) ' oldwt  ',oldwt                                           !/*in         42*/
!L      write (io,*) ' nrand  ',nrand                                           !/*in         43*/
!L      write (io,*) ' rani  ',rani                                             !/*in         44*/
!L      write (io,*) ' ranj  ',ranj                                             !/*in         45*/
!L      write (io,*) ' ranb  ',ranb                                             !/*in         46*/
!L      write (io,*) ' rans  ',rans                                             !/*in         47*/
!L    ==================================================================        !/*in         48*/
      return                                                                    !/*in         49*/
      end                                                                       !/*in         50*/
