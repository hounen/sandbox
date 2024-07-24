#!/usr/bin/gnuplot -persist
set xrange[0:10] 
set yrange[0:180]
set xtics 0,2,10
set ytics 0,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("E_{r}=3.08" 3.08)
set grid x2tics lt -1 lw 3
set terminal postscript eps enhanced color 30 
set output "phs_exp_sp.eps"
p  "data/analysis/phs_ana_s1.d" u 1:($2+180) t "s_{1/2}" w l lw 6.0 lt 1 ,\
   "data/analysis/phs_ana_p1.d" u 1:2 t "p_{1/2}" w l lw 6.0 lt 2,\
   "data/analysis/phs_ana_p3.d" u 1:2 t "p_{3/2}" w l lw 8.0 lt 0 ,\
   "data/experimental/kknn/phs_s1.d" u 1:($2-30) t "" lt -1 ps 2 pt 6,\
   "data/experimental/kknn/phs_p1.d" u 1:2 t "" lt -1 ps 2 pt 6,\
   "data/experimental/kknn/phs_p3.d" u 1:2 t "" lt -1 ps 2 pt 6,\
  90 t "" w l 0

