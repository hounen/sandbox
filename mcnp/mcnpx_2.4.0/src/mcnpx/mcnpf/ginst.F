!#ident ginst.F from deck gin4c2                                                !/*gin4c2      1*/
      subroutine ginst(nw,nd,l1,h1,ip,x1,x2,y1,y2,lb,ld,hd)                     !/*gin4c2      3*/
#ifdef GKSSIM                                                                   /*!gnx         1*/
!        perform gks function 'initialise string'.                              !/*gin4c2      4*/
!        nw = workstation identifier                                            !/*gin4c2      5*/
!        nd = string device number                                              !/*gin4c2      6*/
!        l1 = length of initial string (.ge.0)                                  !/*gin4c2      7*/
!        h1 = initial string                                                    !/*gin4c2      8*/
!        ip = prompt/echo type                                                  !/*gin4c2      9*/
!        x1,x2,y1,y2 = echo area in device coordinates                          !/*gin4c2     10*/
!        lb = input buffer size                                                 !/*gin4c2     11*/
!        ld = dimension of data record array                                    !/*gin4c2     12*/
!        hd = data record                                                       !/*gin4c2     13*/
      real x1,x2,y1,y2                                                          !/*gin4c2     14*/
      character h1*(*),hd(ld)*80                                                !/*gin4c2     15*/
#ifdef XLIB                                                                     /*!gin4c2     16*/
      call xginst(h1,lb,l1,x1,y1)                                               !/*gin4c2     17*/
#endif                                                                          /*!cpp       360*/
#endif                                                                          /*!gnx         2*/
      return                                                                    !/*gin4c2     18*/
      end                                                                       !/*gin4c2     19*/
