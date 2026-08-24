!     $Id: label.h,v 1.4 2002/03/27 18:14:16 gwm Exp $                        !/*la          1*/
      real x_, y_, z_, xc_, yc_, zc_, oldwt_, wt_, e_, ec_, u_, v_, w_,         !/*la          2*/
     & tc_, tip_, apr_, zpr_, erec_, ex_, tipa_, ea_, ua_, va_, wa_, wta        !/*la          3*/
     & _, tipb_, eb_, ub_, vb_, wb_, wtb_, aprr_, zprr_, erecr_, uur_,          !/*la          4*/
     & ecvr_, ur_, vr_, wr_, tca_, tcb_                                         !/*la          5*/
      integer bold_, blz_                                                       !/*la          6*/
      common /label/ emin_(lpt), emax_, wtsum_(lpt,8), nosum_(lpt,8),           !/*la          7*/
     & nhst, ncol_, nocas_, name_, mat_, nmed_, bold_, blz_, tip_, x_, y        !/*la          8*/
     & _, z_, xc_, yc_, zc_, oldwt_, wt_, e_, ec_, u_, v_, w_, tc_,             !/*la          9*/
     & lelem_, nopart_, nabov_, nbelo_, mnuc_, ex_, maxbc_, maxca_,             !/*la         10*/
     & mxmat_, kneut_, npidk_, n1col_, nobal_, nbogu_, nspre_, nwspr_,          !/*la         11*/
     & nseud_, nbert_, ibert_, icpt_, ipht_, ea_(l02), eb_(l02), tipa_          !/*la         12*/
     & (l02), tipb_(l02), ua_(l02), ub_(l02), va_(l02), vb_(l02), wa_           !/*la         13*/
     & (l02), wb_(l02), wta_(l02), wtb_(l02), tca_(l02), tcb_(l02), aprr        !/*la         14*/
     & _(3), zprr_(3), erecr_(3), uur_(3), ur_(3), vr_(3), wr_(3), ecvr_        !/*la         15*/
     & (3), namea_(l02), jtipa_(l02), jtipb_(l02)                               !/*la         16*/
