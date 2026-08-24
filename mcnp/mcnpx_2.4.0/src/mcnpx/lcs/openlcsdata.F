!     $Id: openlcsdata.F,v 1.9 2002/06/14 17:05:43 jxh Exp $                    !/*op          1*/
!                                                                               !/*op          2*/
! This program opens the binary data files needed by                            !/*op          3*/
! the LAHET portions of mcnpx in a consistant way.                              !/*op          4*/
!                                                                               !/*op          5*/
! By side-effect, Fortran Unit # "unit" will be opened                          !/*op          6*/
! upon return.                                                                  !/*op          7*/
!                                                                               !/*op          8*/
      subroutine openlcsdata (unit, prog, path, form)                           !/*op         10*/
!                                                                               !/*op         12*/
! get the mcnp hdpath variable                                                  !/*op         13*/
!                                                                               !/*op         14*/
      use global_parameters, only : hdpth0                                      !/*opxc        1*/
      use global_tables, only : hdpath                                          !/*opxc        2*/
!                                                                               !/*op         18*/
! declare local variables                                                       !/*op         19*/
!                                                                               !/*op         20*/
! unit is the fortran unit number to open                                       !/*op         21*/
! prog is the program name of the caller in case of "expire"                    !/*op         22*/
! path is the name to find                                                      !/*op         23*/
! form is 1 for unformatted/sequential and 2 for formatted                      !/*op         24*/
!                                                                               !/*op         25*/
      integer unit                                                              !/*op         27*/
      integer i                                                                 !/*op         28*/
      character *(*) prog                                                       !/*op         29*/
      character *(*) path                                                       !/*op         30*/
      integer form                                                              !/*op         31*/
!                                                                               !/*op         32*/
! datapath is the directory to use in finding data files.                       !/*op         34*/
! ios is the status variable used for file inquiries                            !/*op         35*/
!                                                                               !/*op         36*/
      character*128 datapath                                                    !/*op         37*/
      logical ios                                                               !/*op         38*/
!                                                                               !/*op        176*/
! First, Look in the current working directory.                                 !/*op        177*/
!                                                                               !/*op        178*/
      datapath = "./" // path                                                   !/*op        179*/
#ifdef PCDOS                                                                    /*!opxc        3*/
      datapath = path                                                           !/*opxc        4*/
#endif                                                                          /*!opxc        5*/
      inquire (file  = datapath, exist = ios)                                   !/*op        180*/
!                                                                               !/*op        181*/
! If the file was not in the current working directory, then                    !/*op        182*/
! look in the hdpath common variable.                                           !/*op        183*/
!                                                                               !/*op        184*/
      if (.not. ios) then                                                       !/*op        185*/
         i = index (hdpath, " ") - 1                                            !/*op        186*/
         if (i .gt. 0) then                                                     !/*op        187*/
            datapath = hdpath(1:i) // "/" // path                               !/*op        188*/
#ifdef PCDOS                                                                    /*!opxc        6*/
            datapath = hdpath(1:i) // "\" // path                               !/*opxc        7*/
#endif                                                                          /*!opxc        8*/
            inquire (file  = datapath, exist = ios)                             !/*op        189*/
         endif                                                                  !/*op        190*/
      endif                                                                     !/*op        191*/
!                                                                               !/*op        192*/
! If not found yet, look at the environment variable.                           !/*op        193*/
!                                                                               !/*op        194*/
      if (.not. ios) then                                                       !/*op        195*/
         call getenv ("DATAPATH", datapath)                                     !/*op        196*/
         if (datapath .ne. " ") then                                            !/*op        197*/
            i = index (datapath, " ") - 1                                       !/*op        198*/
            datapath = datapath(1:i) // "/" // path                             !/*op        199*/
#ifdef PCDOS                                                                    /*!opxc        9*/
            datapath = datapath(1:i) // "\" // path                             !/*opxc       10*/
#endif                                                                          /*!opxc       11*/
            inquire (file = datapath, exist = ios)                              !/*op        200*/
         endif                                                                  !/*op        201*/
      endif                                                                     !/*op        202*/
!                                                                               !/*op        203*/
! Finally, check the AUTOCONFed library path.                                   !/*op        204*/
!                                                                               !/*op        205*/
      if (.not. ios) then                                                       !/*op        206*/
         datapath = hdpth0                                                      !/*opxc       12*/
! #ifdef UNIX                                                                   !/*opxc       13*/
!          if(LIBPREFIX.ne.' ') datapath = LIBPREFIX                            !/*opxc       14*/
! #endif                                                                        !/*opxc       15*/
         i = index (datapath, " ") - 1                                          !/*op        208*/
         datapath = datapath(1:i) // "/" // path                                !/*op        209*/
#ifdef PCDOS                                                                    /*!opxc       16*/
         datapath = datapath(1:i) // "\" // path                                !/*opxc       17*/
#endif                                                                          /*!opxc       18*/
         inquire (file  = datapath, exist = ios)                                !/*op        210*/
       endif                                                                    !/*op        211*/
!                                                                               !/*op        212*/
! After all that, ios tells us if we found a file, and                          !/*op        213*/
! datapath has the pathname.                                                    !/*op        214*/
!                                                                               !/*op        215*/
      if (ios) then                                                             !/*op        216*/
         if (form .eq. 1) then                                                  !/*op        217*/
            open (unit = unit,                                                  !/*op        218*/
     &           file = datapath,                                               !/*op        219*/
     &           status = 'old',                                                !/*op        220*/
     &           form = 'unformatted',                                          !/*op        221*/
     &           access='sequential')                                           !/*op        222*/
         else if (form .eq. 2) then                                             !/*op        223*/
            open (unit = unit,                                                  !/*op        224*/
     &           file = datapath,                                               !/*op        225*/
     &           status = 'old',                                                !/*op        226*/
     &           form = 'formatted')                                            !/*op        227*/
         else                                                                   !/*op        228*/
            call expire (0, prog, "Bad value of form to openlcsdata.")          !/*op        229*/
         endif                                                                  !/*op        230*/
      else                                                                      !/*op        231*/
!                                                                               !/*op        232*/
!     The following assignment is to get around an obscure feature of           !/*op        233*/
!     Fortran where parameters to a routine like expire should not be           !/*op        234*/
!     different datatypes.                                                      !/*op        235*/
!                                                                               !/*op        236*/
         datapath = path                                                        !/*op        237*/
         call expire (0, prog, "Cannot find " // datapath)                      !/*op        238*/
      endif                                                                     !/*op        239*/
      return                                                                    !/*op        240*/
      end                                                                       !/*op        241*/
