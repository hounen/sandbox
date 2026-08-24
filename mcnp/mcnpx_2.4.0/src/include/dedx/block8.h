!     $Id: block8.h,v 1.4 2002/03/27 18:14:05 gwm Exp $                       !/*bl          1*/
      parameter (max_shell=18, max_elem=103)                                    !/*bl          2*/
      common /block8/shel(max_shell,max_elem),eshel(max_shell,max_elem),        !/*bl          3*/
     & slater(max_shell,max_elem),thetas(max_shell,max_elem)                    !/*bl          4*/
