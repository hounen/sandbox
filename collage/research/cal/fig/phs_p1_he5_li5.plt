#!/usr/bin/gnuplot -persist
set xrange[0:10]
set yrange[0:180]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set terminal postscript eps enhanced color 30 solid
set output "phs_p1_he5_li5.eps"
p "../helium5/phs/data/analysis/phs_ana_p1.d" \
  u 1:2 t "he5 p_{1/2}" w l lt -1 lw 4.0,\
  "../lithium5/phs/data/analysis/phs_ana_p1.d" \
  u 1:2 t "li5 p_{1/2}" w l lt 1 lw 8.0,\
  90 t "" w l 0

