#!/usr/bin/gnuplot -persist
set xrange[0:100] 
set xtics 0,20,100
set ytics -180,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set x2tics ("E_{r}=17.8" 17.84957)
set grid x2tics lt -1 lw 3
set terminal postscript eps enhanced color 30 solid
set output "allphs_f5_100MeV.eps"
p "allphs_f5_100MeV.d" u 1:2 t "f_{5/2}" w l lw 6.0,\
  "allphs_f5_100MeV.d" u 1:3 t "resonance" w l lw 6.0,\
  "allphs_f5_100MeV.d" u 1:4 t "continuum" w l lw 6.0,\
  90 t "" w l 0
