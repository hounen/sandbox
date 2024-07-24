#!/usr/bin/gnuplot -persist
set xrange[0:10]
set yrange[0:180]
set xtics 0,20,100
set ytics 0,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set terminal postscript eps enhanced color 30 solid
set output "phs_s1_ana.eps"
p "phs_s1_ana.d" u 1:($2+180) t "s_{1/2}" w l lw 6.0,\
  90 t "" w l 0

