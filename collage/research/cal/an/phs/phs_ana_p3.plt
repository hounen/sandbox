#!/usr/bin/gnuplot -persist
set xrange[0:10] 
set yrange[0:180]
set xtics 0,2,10
set ytics -180,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("E_{r}=3.08" 3.08)
set grid x2tics lt -1 lw 3
set terminal postscript eps enhanced color 30 solid
set output "phs_ana_p3.eps"
p  "data/analysis/phs_ana_p3.d" u 1:2 t "p_{3/2}" w l lw 6.0,\
  90 t "" w l 0

