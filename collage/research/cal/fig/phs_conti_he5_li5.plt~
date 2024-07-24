#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [120:180]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol d}(E)[deg.]"
set terminal postscript eps enhanced color 30 solid
set output "phs_conti_he5_li5.eps"
p "../helium5/phs/allphs/allphs_p1_100MeV.d"\
  u 1:($4+180) t "^5He p_{1/2} conti" w l lw 6.0 ,\
  "../helium5/phs/allphs/allphs_p3_100MeV.d"\
  u 1:($4+180) t "^5He p_{3/2} conti" w l lw 6.0 ,\
  "../lithium5/phs/data/analysis/phs_ana_p1.d"\
  u 1:($4+180) t "^5Li p_{1/2} conti" w l lw 6.0 ,\
  "../lithium5/phs/data/analysis/phs_ana_p3.d"\
  u 1:($4+180) t "^5Li p_{3/2} conti" w l lw 6.0
