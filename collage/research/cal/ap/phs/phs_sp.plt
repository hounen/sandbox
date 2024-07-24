#!/usr/bin/gnuplot -persist
set xrange[0:10] 
set yrange[0:180]
set xtics 0,2,10
set ytics -180,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set terminal postscript eps enhanced color 30 solid
set output "phs_sp.eps"
p "data/analysis/phs_ana_s1.d" u 1:($2+180) t "s_{1/2}" w l lw 6.0,\
  "data/analysis/phs_ana_p1.d" u 1:2 t "p_{1/2}" w l lw 6.0,\
  "data/analysis/phs_ana_p3.d" u 1:2 t "p_{3/2}" w l lw 6.0,\
  "data/experimental/kknn/phs_s1.d" u 1:($2-30) t "" pt 6 ps 2 lt -1 ,\
  "data/experimental/kknn/phs_p1.d" u 1:2 t "" pt 6 ps 2 lt -1,\
  "data/experimental/kknn/phs_p3.d" u 1:2 t "" pt 6 ps 2 lt -1,\
  90 t "" w l 0

