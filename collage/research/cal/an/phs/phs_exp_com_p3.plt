#!/usr/bin/gnuplot -persist
set xrange[0:10]
set yrange[0:180]
set xtics 0,1,10
set ytics 0,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("p_{1/2}" 2.077087,"p_{3/2}" 0.7846474)
#set grid x2tics'
set terminal postscript eps enhanced color 15 solid
set output "phs_exp_com_p3.eps"
p "data/analysis/phs_ana_p3.d" u 1:2 t "he5-p_{3/2}" w l lw 6.0,\
  "data/experimental/hoop_and_barschall/phs_sp.d" \
  u ($1*0.8):4 t "hoop1966-p_{3/2}" w p lt 3 pt 7 ps 2,\
  "data/experimental/morgan1968/phs_sp.d" \
  u ($1*0.8):4 t "morgan1968-p_{3/2}" w p lt 1 pt 7 ps 2,\
  "data/experimental/sawers/phs_p3.d" \
  u ($1*0.8):2 t "sawers1968-p_{3/2}" w p lt 4 pt 7 ps 2,\
  "data/experimental/sawers-may/phs_p3.d" \
  u ($1*0.8):2 t "sawers-may-p_{3/2}" w p lt 5 pt 7 ps 2,\
  "data/experimental/kknn/phs_p3.d" \
  u 1:2 t "kknn1978-p_{3/2}" w p lt 0 pt 7 ps 2,\
  90 t "" w l 0

