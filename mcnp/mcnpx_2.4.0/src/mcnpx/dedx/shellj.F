!     $Id: shellj.F,v 1.5 2002/03/27 18:14:34 gwm Exp $                       !/*sh          1*/
      function shellj (z,t,ck,cl1,cl2,cl,cm1,cm2,cm3,cm,cn1,cn2,cn3,cn4         !/*sh          2*/
     & ,cn,co1,co2,co3,co,cp1,cp2,cp3,cp,ikshl,ilshl,imshl,inshl,ioshl          !/*sh          3*/
     & ,ipshl,ptraut)                                                           !/*sh          4*/
#include <lcs/double.h>                                                         /*!sh          5*/
!        JANNI SHELL CORRECTION CODING                                          !/*sh          6*/
!                                                                               !/*sh          7*/
!        purpose :  this routine is a mini-executive that directs               !/*sh          8*/
!                   the calling of the shell correction routines                !/*sh          9*/
!                   based upon the specific author/method that                  !/*sh         10*/
!                   is requested.                                               !/*sh         11*/
!                                                                               !/*sh         12*/
!        author :   daniel pickens, computer sciences corporation               !/*sh         13*/
!                   10 june 1984.                                               !/*sh         14*/
!        revised:   j.lopez, csc, oct 1985.                                     !/*sh         15*/
!                                                                               !/*sh         16*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         17*/
!                   t      - is the energy value.                               !/*sh         18*/
!                   ptraut - logical variable, if true then author              !/*sh         19*/
!                            and reference information is writen                !/*sh         20*/
!                            to scratch file 10 for later printing.             !/*sh         21*/
!                                                                               !/*sh         22*/
!        outputs :  ck     - the k shell correction.                            !/*sh         23*/
!                   cl1    - the l shell, s subshell correction.                !/*sh         24*/
!                   cl2    - the l shell, p subshell correction.                !/*sh         25*/
!                   cl     - the l shell correction.                            !/*sh         26*/
!                   cm1    - the m shell, s subshell correction.                !/*sh         27*/
!                   cm2    - the m shell, p subshell correction.                !/*sh         28*/
!                   cm3    - the m shell, d subshell correction.                !/*sh         29*/
!                   cm     - the m shell correction.                            !/*sh         30*/
!                   cn1    - the n shell, s subshell correction.                !/*sh         31*/
!                   cn2    - the n shell, p subshell correction.                !/*sh         32*/
!                   cn3    - the n shell, d subshell correction.                !/*sh         33*/
!                   cn4    - the n shell, f subshell correction.                !/*sh         34*/
!                   cn     - the n shell correction.                            !/*sh         35*/
!                   co1    - the o shell, s subshell correction.                !/*sh         36*/
!                   co2    - the o shell, p subshell correction.                !/*sh         37*/
!                   co3    - the o shell, d subshell correction.                !/*sh         38*/
!                   co     - the o shell correction.                            !/*sh         39*/
!                   cp1    - the p shell, s subshell correction.                !/*sh         40*/
!                   cp2    - the p shell, p subshell correction.                !/*sh         41*/
!                   cp     - the p shell correction.                            !/*sh         42*/
!                   shell  - the total shell correction.                        !/*sh         43*/
!                                                                               !/*sh         44*/
!        errors :   none.                                                       !/*sh         45*/
!                                                                               !/*sh         46*/
!     if ptraut is true print the shell corrections.                            !/*sh         47*/
!                                                                               !/*sh         48*/
      logical ptraut                                                            !/*sh         49*/
!                                                                               !/*sh         50*/
      parameter (prot_mass=938.2723128d0)                                       !/*sh         51*/
      parameter (e=prot_mass)                                                   !/*sh         52*/
!                                                                               !/*sh         53*/
      s=t/e                                                                     !/*sh         54*/
      betasq=s*(s+dp2)/(s+dp1)**2                                               !/*sh         55*/
!                                                                               !/*sh         56*/
!        shell corrections:                                                     !/*sh         57*/
!                                                                               !/*sh         58*/
!        k-shell                                               value            !/*sh         59*/
!        routine name  author         type                     ikshl            !/*sh         60*/
!        ------------  -------------- ---------------------    ----             !/*sh         61*/
!        shelwk        m. walske      total shell (default)      1              !/*sh         62*/
!        shelkk        g. khandelwal  total shell                2              !/*sh         63*/
!                                                                               !/*sh         64*/
      if (ikshl.eq.1) then                                                      !/*sh         65*/
        ck=shelwk(z,betasq,ptraut)                                              !/*sh         66*/
      elseif (ikshl.eq.2) then                                                  !/*sh         67*/
        ck=shelkk(z,betasq,ptraut)                                              !/*sh         68*/
      else                                                                      !/*sh         69*/
        stop 'shellj - select type of k-shell correction'                       !/*sh         70*/
      endif                                                                     !/*sh         71*/
!                                                                               !/*sh         72*/
!        l-shell                                               value            !/*sh         73*/
!        routine name  author         type                     ilshl            !/*sh         74*/
!        ------------  -------------- ---------------------    ----             !/*sh         75*/
!        shlbil        h. bichsel     indiv.subshell(default)    1              !/*sh         76*/
!        sheljl        j. janni       total shell(original)      2              !/*sh         77*/
!        shelwl        m. walske      total shell                3              !/*sh         78*/
!        shelkl        g. khandelwal  individual subshell        4              !/*sh         79*/
!        shelbl        h. bichsel     total shell                5              !/*sh         80*/
!                                                                               !/*sh         81*/
      if (ilshl.eq.1) then                                                      !/*sh         82*/
        cl=shlbil(z,betasq,cl1,cl2,ptraut)                                      !/*sh         83*/
      elseif (ilshl.eq.2) then                                                  !/*sh         84*/
        cl=sheljl(z,betasq,cl1,cl2,ptraut)                                      !/*sh         85*/
      elseif (ilshl.eq.3) then                                                  !/*sh         86*/
        cl=shelwl(z,betasq,cl1,cl2,ptraut)                                      !/*sh         87*/
      elseif (ilshl.eq.4) then                                                  !/*sh         88*/
        cl=shelkl(z,betasq,cl1,cl2,ptraut)                                      !/*sh         89*/
      elseif (ilshl.eq.5) then                                                  !/*sh         90*/
        cl=shelbl(z,betasq,cl1,cl2,ptraut)                                      !/*sh         91*/
      else                                                                      !/*sh         92*/
        stop 'shellj - select type of l-shell correction'                       !/*sh         93*/
      endif                                                                     !/*sh         94*/
!                                                                               !/*sh         95*/
!        m-shell                                               value            !/*sh         96*/
!        routine name  author         type                     imshl            !/*sh         97*/
!        ------------  -------------- ---------------------    ----             !/*sh         98*/
!        shelbm        h. bichsel     indiv. subshell(default)   1              !/*sh         99*/
!        sheljm        j. janni       indiv. subshell(original   2              !/*sh        100*/
!                                     scaling  method)                          !/*sh        101*/
!        shelkm        g. khandelwal  total shell                3              !/*sh        102*/
!        shlkim        g. khandelwal  indiv. subshell            4              !/*sh        103*/
!        shelsm        scaled from l  indiv. subhsell(scaled     5              !/*sh        104*/
!                                     using Bichsel subshell                    !/*sh        105*/
!                                                                               !/*sh        106*/
      if (imshl.eq.1) then                                                      !/*sh        107*/
        cm=shelbm(z,betasq,cm1,cm2,cm3,ptraut)                                  !/*sh        108*/
      elseif (imshl.eq.2) then                                                  !/*sh        109*/
        cm=sheljm(z,betasq,cm1,cm2,cm3,ptraut)                                  !/*sh        110*/
      elseif (imshl.eq.3) then                                                  !/*sh        111*/
        cm=shelkm(z,betasq,cm1,cm2,cm3,ptraut)                                  !/*sh        112*/
      elseif (imshl.eq.4) then                                                  !/*sh        113*/
        cm=shlkim(z,betasq,cm1,cm2,cm3,ptraut)                                  !/*sh        114*/
      elseif (imshl.eq.5) then                                                  !/*sh        115*/
        cm=shelsm(z,betasq,cm1,cm2,cm3,ptraut)                                  !/*sh        116*/
      else                                                                      !/*sh        117*/
        stop 'shellj - select type of m-shell correction'                       !/*sh        118*/
      endif                                                                     !/*sh        119*/
!                                                                               !/*sh        120*/
!        n-shell                                               value            !/*sh        121*/
!        routine name  author         type                     inshl            !/*sh        122*/
!        ------------  -------------- ---------------------    ----             !/*sh        123*/
!        shelsn        scaled from m  indiv. subshell (scaled    1              !/*sh        124*/
!                                     using Bichsel subshell                    !/*sh        125*/
!                                     m shell)                                  !/*sh        126*/
!        sheljn        j. janni       indiv. subshell            2              !/*sh        127*/
!                                     (original method using                    !/*sh        128*/
!                                     Janni l shell)                            !/*sh        129*/
!                                                                               !/*sh        130*/
      if (inshl.eq.1) then                                                      !/*sh        131*/
        cn=shelsn(z,betasq,cn1,cn2,cn3,cn4,ptraut)                              !/*sh        132*/
      elseif (inshl.eq.2) then                                                  !/*sh        133*/
        cn=sheljn(z,betasq,cn1,cn2,cn3,cn4,ptraut)                              !/*sh        134*/
      else                                                                      !/*sh        135*/
        stop 'shellj - select type of n-shell correction'                       !/*sh        136*/
      endif                                                                     !/*sh        137*/
!                                                                               !/*sh        138*/
!        o-shell                                               value            !/*sh        139*/
!        routine name  author         type                     ioshl            !/*sh        140*/
!        ------------  -------------- ---------------------    ----             !/*sh        141*/
!        shelso        scaled from m  indiv. subshell(scaled     1              !/*sh        142*/
!                                     using Bichsel subshell                    !/*sh        143*/
!                                     m shell)                                  !/*sh        144*/
!        sheljo        j. janni       indiv. subshell(original   2              !/*sh        145*/
!                                     method using Janni                        !/*sh        146*/
!                                     l shell)                                  !/*sh        147*/
!                                                                               !/*sh        148*/
      if (ioshl.eq.1) then                                                      !/*sh        149*/
        co=shelso(z,betasq,co1,co2,co3,ptraut)                                  !/*sh        150*/
      elseif (ioshl.eq.2) then                                                  !/*sh        151*/
        co=sheljo(z,betasq,co1,co2,co3,ptraut)                                  !/*sh        152*/
      else                                                                      !/*sh        153*/
        stop 'shellj - select type of o-shell correction'                       !/*sh        154*/
      endif                                                                     !/*sh        155*/
!                                                                               !/*sh        156*/
!        p-shell                                               value            !/*sh        157*/
!        routine name  author         type                     ipshl            !/*sh        158*/
!        ------------  -------------- ---------------------    ----             !/*sh        159*/
!        shelsp        scaled from m  indiv. subshell (scaled    1              !/*sh        160*/
!                                     using Bichsel subshell                    !/*sh        161*/
!                                     m shell)                                  !/*sh        162*/
!        sheljp        j. janni       indiv. subshell            2              !/*sh        163*/
!                                     (original method using                    !/*sh        164*/
!                                     Janni l shell)                            !/*sh        165*/
!                                                                               !/*sh        166*/
      if (ipshl.eq.1) then                                                      !/*sh        167*/
        cp=shelsp(z,betasq,cp1,cp2,cp3,ptraut)                                  !/*sh        168*/
      elseif (ipshl.eq.2) then                                                  !/*sh        169*/
        cp=sheljp(z,betasq,cp1,cp2,cp3,ptraut)                                  !/*sh        170*/
      else                                                                      !/*sh        171*/
        stop 'shellj - select type of p-shell correction'                       !/*sh        172*/
      endif                                                                     !/*sh        173*/
!                                                                               !/*sh        174*/
!        add all the shell corrections together to obtain                       !/*sh        175*/
!        the total shell correction.                                            !/*sh        176*/
!                                                                               !/*sh        177*/
      shellj=ck+cl+cm+cn+co+cp                                                  !/*sh        178*/
!                                                                               !/*sh        179*/
      return                                                                    !/*sh        180*/
      end                                                                       !/*sh        181*/
