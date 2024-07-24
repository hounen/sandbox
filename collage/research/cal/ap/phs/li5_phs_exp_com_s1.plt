#!/usr/bin/gnuplot -persist
set xrange[0:10]
set yrange[0:180]
set xtics 0,1,10
set ytics 0,30,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set terminal postscript eps enhanced color 15 solid
set output "li5_phs_exp_com_s1.eps"
p "data/analysis/phs_ana_s1.d" u 1:($2+180) t "li5-s_{1/2}" w l lw 6.0 lt 1,\
  "data/experimental/critchfield_dodder/phs_sp.d" \
  u ($1*0.8):($2+180) t "critchfield-dodder1949-s_{1/2}" w p lt 4 pt 7 ps 2, \
  "data/experimental/miller_phillips/phs_sp.d" \
  u ($1*0.8):($2+180) t "miller-phillips1958-s_{1/2}" w p lt 5 pt 7 ps 2, \
  "data/experimental/barnard/phs_sp_A.d" \
  u ($1*0.8):($2+180) t "barnard1963-A-s_{1/2}" w p lt 2 pt 7 ps 2, \
  "data/experimental/barnard/phs_sp_B.d" \
  u ($1*0.8):($2+180) t "barnard1963-B-s_{1/2}" w p lt 3 pt 7 ps 2, \
  "data/experimental/brown/li5_phs_brown_sp.d" \
  u ($1*0.8):($2+180) t "brown1966-s_{1/2}" w p lt 1 pt 2 ps 2,\
  "data/experimental/schwandt/phs_sp.d" \
  u ($1*0.8):2 t "schwandt1971-s_{1/2}" w p lt 6 pt 7 ps 2, \
  "data/experimental/kknn/phs_s1.d" \
  u 1:($2-30) t "kknn1978-s_{1/2}" w p lt 0 pt 7 ps 2, \
  90 t "" w l 0


