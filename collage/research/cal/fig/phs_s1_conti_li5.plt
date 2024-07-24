#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [0:180]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol d}(E)[deg.]"
set key 8,80 
set terminal postscript eps enhanced color 30 solid
set output "phs_s1_conti_li5.eps"
p "../lithium5/phs/data/analysis/phs_ana_s1.d"\
  u 1:($2+180) t "^5Li s_{1/2}" w l lw 6.0 ,\
  "../lithium5/phs/data/analysis/phs_ana_p1.d"\
  u 1:($4+180) t "^5Li p_{1/2} conti" w l lw 6.0 ,\
  "../lithium5/phs/data/analysis/phs_ana_p3.d"\
  u 1:($4+180) t "^5Li p_{3/2} conti" w l lw 6.0
