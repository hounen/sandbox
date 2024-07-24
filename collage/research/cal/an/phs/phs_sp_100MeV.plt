#!/usr/bin/gnuplot -persist
set xrange[0:10]
set yrange[0:180]
set xtics 0,20,100
set ytics 0,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("p_{1/2}" 2.077087,"p_{3/2}" 0.7846474)
#set grid x2tics'
set terminal postscript eps enhanced color 30 solid
set output "phs_exp_com_s1.eps"
p "phs_s1_ana.d" u 1:($2+180) t "s_{1/2}" w l lw 6.0,\
  "data/experimental/kknn/phs_s1_He-n.d" u 1:($2-30) t "kknn1978-s_{1/2}" w l lw 6.0,\
  "data/experimental/morgan1968/phs_sp.d" u 1:2 t "morgan1968-s_{1/2}" w l lw 6.0,\
  90 t "" w l 0

