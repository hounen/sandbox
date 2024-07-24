#!/usr/bin/gnuplot -persist
set xrange[0:20] 
set yrange[-50:200]
set xtics 0,2,20
set ytics -50,50,200
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("E_{r}=3.08" 3.08)
set terminal postscript enhanced color 30 solid
set output "AAPHS024.ps"
p  "DATA/ANA/AAPHS0.d" u 1:2 t "L=0" w l lw 6.0,\
   "DATA/ANA/AAPHS2.d" u 1:2 t "L=2" w l lw 6.0,\
   "DATA/ANA/AAPHS4.d" u 1:2 t "L=4" w l lw 6.0,\
  90 t "" w l 0

