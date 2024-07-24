#!/usr/bin/gnuplot -persist
set xrange[0:20] 
set yrange[-50:200]
set xtics 0,2,20
set ytics -50,50,200
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("E_{r}=3.08" 3.08)
set terminal postscript enhanced color 15 solid
set output "aaphsbuck024.ps"
p  "DATA/ANA/BUCK/aaphs0.d" u 1:2 t "sw L=0" w l lw 6.0,\
   "DATA/ANA/BUCK/aaphs2.d" u 1:2 t "sw L=2" w l lw 6.0,\
   "DATA/ANA/BUCK/aaphs4.d" u 1:2 t "sw L=4" w l lw 6.0,\
   "DATA/ANA/SW/AAPHS0.d" u 1:2 t "buck L=0" w l lw 6.0,\
   "DATA/ANA/SW/AAPHS2.d" u 1:2 t "buck L=2" w l lw 6.0,\
   "DATA/ANA/SW/AAPHS4.d" u 1:2 t "buck L=4" w l lw 6.0,\
   "DATA/EXP/a-a_phs_0.d" u 1:2 t "exp L=0" ps 1,\
	 "DATA/EXP/a-a_phs_2.d" u 1:2 t "exp L=2" ps 1,\
	 "DATA/EXP/a-a_phs_4.d" u 1:2 t "exp L=4" ps 1 
#	 90 t "" w l 0                             
                                              
