#!/usr/bin/gnuplot -persist
set xrange[0:100] 
set xtics 0,2,10
set ytics -180,30,180
set xlabel "energy [mev]"
set ylabel "{/symbol d} [deg.]"
set x2tics ("e_{r}=2.08" 2.077087)
set grid x2tics lt -1 lw 3
set terminal postscript eps enhanced color 30 solid
set output "allphs_p1.eps"
p "allphs_p1_100mev.d" u 1:2 t "p_{1/2}" w l lw 6.0,\
  "allphs_p1_100mev.d" u 1:3 t "resonance" w l lw 6.0,\
  "allphs_p1_100mev.d" u 1:4 t "continuum" w l lw 6.0,\
  90 t "" w l 0
