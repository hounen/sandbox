#!/usr/bin/gnuplot -persist
set xrange[0:10]
set yrange[0:180]
set xtics 0,1,15
set ytics 0,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set terminal postscript eps enhanced color 15 solid
set output "li5_phs_exp_com_p3.eps"
p "data/analysis/phs_ana_p3.d" u 1:2 t "li5-p_{3/2}" w l lt 1 lw 6.0,\
  "data/experimental/critchfield_dodder/phs_sp.d" \
  u ($1*0.8):4 t "critchfield-dodder1949-p_{3/2}" w p lt 4 pt 7 ps 2,\
  "data/experimental/miller_phillips/phs_sp.d" \
  u ($1*0.8):4 t "miller-phillips1958-p_{3/2}" w p lt 5 pt 7 ps 2,\
  "data/experimental/barnard/phs_sp_A.d"\
  u ($1*0.8):4 t "barnard-A1963-p_{3/2}" w p lt 2 pt 7 ps 2,\
  "data/experimental/barnard/phs_sp_B.d"\
  u ($1*0.8):4 t "barnard-B1963-p_{3/2}" w p lt 3 pt 7 ps 2,\
  "data/experimental/brown/li5_phs_brown_sp.d"\
  u ($1*0.8):4 t "brown1966-p_{3/2}" w p lt 1 pt 2 ps 2,\
  "data/experimental/jahns_bernstein/phs_p3.d" \
  u ($1*0.8):2  t "jahns-bernstein1967-p_{3/2}" w p lt 9 pt 7 ps 2,\
  "data/experimental/schwandt/phs_sp.d" \
  u ($1*0.8):4 t "schwandt1971-p_{3/2}" w p lt 6 pt 7 ps 2, \
  "data/analysis/kknn/phs_kknn_mhn_p3.d" \
  u 1:2 t "kknn-mhn1978-p_{3/2}" w p lt 8 pt 7 ps 2, \
  "data/experimental/kknn/phs_p3.d"\
  u 1:2 t "kknn1978-p_{3/2}" w p lt 0 pt 7 ps 2,\
  90 t "" w l 0

