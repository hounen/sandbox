#!/usr/bin/gnuplot -persist
set xrange[0:10] 
set xtics 0,2,10
set ytics 0,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("E_{r}=0.78" 0.7846474)
#set grid x2tics lt -1 lw 3
set terminal postscript eps enhanced color 30 solid
set output "allphs_p3_resonly.eps"
p "allphs_p3_100MeV.d" u 1:2 t "p_{3/2} total" w l lt -1 lw 4.0,\
  "allphs_p3_100MeV.d" u 1:3 t "resonance" w l lt 1 lw 6.0,\
  90 t "" w l 0

