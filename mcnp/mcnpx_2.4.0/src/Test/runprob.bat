mcnpx name=inp01 xsdir=testdir
fc inp01m mctl01 > difm01
fc inp01o outp01 > difo01
del inp01r
mcnpx name=inp02 xsdir=testdir fatal
fc inp02m mctl02 > difm02
fc inp02o outp02 > difo02
del inp02r
mcnpx name=inp03 xsdir=testdir
fc inp03m mctl03 > difm03
fc inp03o outp03 > difo03
del inp03r
mcnpx name=inp04 xsdir=testdir
fc inp04m mctl04 > difm04
fc inp04o outp04 > difo04
del inp04r
mcnpx name=inp05 xsdir=testdir
fc inp05m mctl05 > difm05
fc inp05o outp05 > difo05
del inp05r
mcnpx name=inp06 xsdir=testdir fatal
fc inp06m mctl06 > difm06
fc inp06o outp06 > difo06
del inp06r
mcnpx name=inp07 xsdir=testdir
fc inp07m mctl07 > difm07
fc inp07o outp07 > difo07
del inp07r
mcnpx name=inp08 xsdir=testdir wwinp=wwinp08 rssa=inp07w
fc inp08m mctl08 > difm08
fc inp08o outp08 > difo08
fc inp08e wout08 > difw08
del inp08r
mcnpx xsdir=testdir name=inp09
fc inp09m mctl09 > difm09
fc inp09o outp09 > difo09
mcnpx name=inp10 xsdir=testdir wwinp=wwinp10
fc inp10m mctl10 > difm10
fc inp10o outp10 > difo10
fc inp10e wout10 > difw10
del inp10r
mcnpx name=inp11 xsdir=testdir fatal
fc inp11m mctl11 > difm11
fc inp11o outp11 > difo11
del inp11r
mcnpx name=inp12 xsdir=testdir
fc inp12m mctl12 > difm12
fc inp12o outp12 > difo12
fc inp12e wout12 > difw12
del inp12r
mcnpx name=inp13 xsdir=testdir
fc inp13m mctl13 > difm13
fc inp13o outp13 > difo13
del inp13r
mcnpx name=inp14 xsdir=testdir wwinp=wwinp14 fatal
fc inp14m mctl14 > difm14
fc inp14o outp14 > difo14
fc inp14e wout14 > difw14
del inp14r
mcnpx name=inp15 xsdir=testdir
fc inp15m mctl15 > difm15
fc inp15o outp15 > difo15
del inp15r
mcnpx name=inp16 xsdir=testdir
fc inp16m mctl16 > difm16
fc inp16o outp16 > difo16
del inp16r
copy inp09s srctp17
mcnpx name=inp17 s=srctp17 xsdir=testdir
fc inp17m mctl17 > difm17
fc inp17o outp17 > difo17
del inp17r srctp17
mcnpx name=inp18 xsdir=testdir
fc inp18m mctl18 > difm18
fc inp18o outp18 > difo18
del inp18r
mcnpx name=inp19 xsdir=testdir
fc inp19m mctl19 > difm19
fc inp19o outp19 > difo19
del inp19r
mcnpx name=inp20 xsdir=testdir
fc inp20m mctl20 > difm20
fc inp20o outp20 > difo20
del inp20r
mcnpx name=inp21 xsdir=testdir
fc inp21m mctl21 > difm21
fc inp21o outp21 > difo21
del inp21r
mcnpx xsdir=testdir name=inp22 rs=inp21w
fc inp22m mctl22 > difm22
fc inp22o outp22 > difo22
del inp22r
mcnpx name=inp23 xsdir=testdir
fc inp23m mctl23 > difm23
fc inp23o outp23 > difo23
del inp23r
mcnpx name=inp24 xsdir=testdir
fc inp24m mctl24 > difm24
fc inp24o outp24 > difo24
mcnpx name=inp25 run=inp24r CN
fc inp25m mctl25 > difm25
fc inp25o outp25 > difo25
mcnpx name=inp26 wss=inp09w run=inp09r CN
fc inp26m mctl26 > difm26
fc inp26o outp26 > difo26
del inp24r inp09r
mcnpx name=inp27 rss=inp09w xsdir=testdir
fc inp27m mctl27 > difm27
fc inp27o outp27 > difo27
del inp27r
mcnpx name=inp28 xsdir=testdir
move ex5a.out inp28o
fc inp28m mctl28 > difm28
fc inp28o outp28 > difo28
del ex5a.run
mcnpx name=inp29 xsdir=testdir rssa=inp07w
fc inp29m mctl29 > difm29
fc inp29o outp29 > difo29
del inp29r
mcnpx name=inp30 xsdir=testdir
fc inp30m mctl30 > difm30
fc inp30o outp30 > difo30
del inp30r
mcnpx name=inp31 xsdir=testdir
fc inp31m mctl31 > difm31
fc inp31o outp31 > difo31
del inp31r
mcnpx name=inp32 xsdir=testdir
fc inp32m mctl32 > difm32
fc inp32o outp32 > difo32
del inp32r inp32s
mcnpx name=inp33 xsdir=testdir
fc inp33m mctl33 > difm33
fc inp33o outp33 > difo33
del inp33r
mcnpx name=inp34 xsdir=testdir
fc inp34m mctl34 > difm34
fc inp34o outp34 > difo34
del inp34r
mcnpx name=inp35 xsdir=testdir
fc inp35m mctl35 > difm35
fc inp35o outp35 > difo35
del inp35r
mcnpx name=inp36 xsdir=testdir
fc inp36m mctl36 > difm36
fc inp36o outp36 > difo36
del inp36r
mcnpx name=inp37 xsdir=testdir
fc inp37m mctl37 > difm37
fc inp37o outp37 > difo37
del inp37r
mcnpx name=inp38 xsdir=testdir
fc inp38m mctl38 > difm38
fc inp38o outp38 > difo38
del inp38r
mcnpx name=inp39 xsdir=testdir
fc inp39m mctl39 > difm39
fc inp39o outp39 > difo39
del inp39r
mcnpx name=inp40 xsdir=testdir
fc inp40m mctl40 > difm40
fc inp40o outp40 > difo40
del inp40r
mcnpx name=inp41 xsdir=testdir
fc inp41m mctl41 > difm41
fc inp41o outp41 > difo41
del inp41r
mcnpx name=inp42 xsdir=testdir
fc inp42m mctl42 > difm42
fc inp42o outp42 > difo42
del inp42r
mcnpx name=inp43 xsdir=testdir
fc inp43m mctl43 > difm43
fc inp43o outp43 > difo43
del inp43r
mcnpx name=inp44 xsdir=testdir
fc inp44m mctl44 > difm44
fc inp44o outp44 > difo44
del inp44r
mcnpx name=inp45 xsdir=testdir
fc inp45m mctl45 > difm45
fc inp45o outp45 > difo45
del inp45r
mcnpx name=inp46 xsdir=testdir
fc inp46m mctl46 > difm46
fc inp46o outp46 > difo46
del inp46r
mcnpx name=inp47 xsdir=testdir
fc inp47m mctl47 > difm47
fc inp47o outp47 > difo47
del inp47r
mcnpx name=mes01 xsdir=testdir mdata=mes01d
fc mes01m mcts01 > difsm01
fc mes01o outs01 > difso01
fc mes01d mdat01 > difd01
del mes01r
mcnpx name=mes02 xsdir=testdir mdata=mes02d
fc mes02m mcts02 > difsm02
fc mes02o outs02 > difso02
fc mes02d mdat02 > difd02
del mes02r
mcnpx name=mes03 xsdir=testdir mdata=mes03d
fc mes03m mcts03 > difsm03
fc mes03o outs03 > difso03
fc mes03d mdat03 > difd03
del mes03r
mcnpx name=mes04 xsdir=testdir mdata=mes04d
fc mes04m mcts04 > difsm04
fc mes04o outs04 > difso04
fc mes04d mdat04 > difd04
del mes04r
mcnpx name=mes05 xsdir=testdir mdata=mes05d
fc mes05m mcts05 > difsm05
fc mes05o outs05 > difso05
fc mes05d mdat05 > difd05
del mes05r
mcnpx name=mes06 xsdir=testdir mdata=mes06d
fc mes06m mcts06 > difsm06
fc mes06o outs06 > difso06
fc mes06d mdat06 > difd06
del mes06r
mcnpx name=mes07 xsdir=testdir mdata=mes07d
fc mes07m mcts07 > difsm07
fc mes07o outs07 > difso07
fc mes07d mdat07 > difd07
del mes07r
mcnpx name=mes08 xsdir=testdir mdata=mes08d
fc mes08m mcts08 > difsm08
fc mes08o outs08 > difso08
fc mes08d mdat08 > difd08
del mes08r
mcnpx name=mes09 xsdir=testdir mdata=mes09d
fc mes09m mcts09 > difsm09
fc mes09o outs09 > difso09
fc mes09d mdat09 > difd09
mcnpx name=mes10 xsdir=testdir mdata=mes10d
fc mes10m mcts10 > difsm10
fc mes10o outs10 > difso10
fc mes10d mdat10 > difd10
del mes10r
mcnpx name=mes11 xsdir=testdir mdata=mes11d
fc mes11m mcts11 > difsm11
fc mes11o outs11 > difso11
fc mes11d mdat11 > difd11
del mes11r
mcnpx name=mes12 xsdir=testdir mdata=mes12d
fc mes12m mcts12 > difsm12
fc mes12o outs12 > difso12
fc mes12d mdat12 > difd12
del mes12r
mcnpx name=mes13 xsdir=testdir mdata=mes13d
fc mes13m mcts13 > difsm13
fc mes13o outs13 > difso13
fc mes13d mdat13 > difd13
del mes13r
mcnpx name=mes14 xsdir=testdir mdata=mes14d
fc mes14m mcts14 > difsm14
fc mes14o outs14 > difso14
fc mes14d mdat14 > difd14
del mes14r
mcnpx name=mes15 xsdir=testdir mdata=mes15d
fc mes15m mcts15 > difsm15
fc mes15o outs15 > difso15
fc mes15d mdat15 > difd15
del mes15r
mcnpx name=mes16 xsdir=testdir mdata=mes16d
fc mes16m mcts16 > difsm16
fc mes16o outs16 > difso16
fc mes16d mdat16 > difd16
del mes16r
mcnpx name=htp01 xsdir=testdir
fc htp01m mcth01 > difmt01
htape3x int=int01 histx=htp01w
move outt htp01t
fc htp01t outt01 > dift01
del htp01r htp01o histp
mcnpx name=htp02 xsdir=testdir
fc htp02m mcth02 > difmt02
htape3x int=int02 histx=htp02w
move outt htp02t
fc htp02t outt02 > dift02
del htp02r htp02o histp
mcnpx name=htp03 xsdir=testdir
fc htp03m mcth03 > difmt03
htape3x int=int03 histx=htp03w
move outt htp03t
fc htp03t outt03 > dift03
del htp03r htp03o histp
mcnpx name=htp04 xsdir=testdir
fc htp04m mcth04 > difmt04
htape3x int=int04 histx=htp04w
move outt htp04t
fc htp04t outt04 > dift04
del htp04r htp04o histp
mcnpx name=htp05 xsdir=testdir
fc htp05m mcth05 > difmt05
htape3x int=int05 histx=htp05w
move outt htp05t
fc htp05t outt05 > dift05
xsex3 inxs=inxs05
move outxs htp05x
fc htp05x outxs05 > difx05
del htp05r htp05o histp
mcnpx name=htp06 xsdir=testdir
fc htp06m mcth06 > difmt06
htape3x int=int06 histx=htp06w
move outt htp06t
fc htp06t outt06 > dift06
xsex3 inxs=inxs06
move outxs htp06x
fc htp06x outxs06 > difx06
del htp06r htp06o histp
