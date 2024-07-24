#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [0:180]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol d}(E)[deg.]"
set key 8,80 
set terminal postscript eps enhanced color 30 solid
set output "phs_s1_conti_he5.eps"
p "../helium5/phs/data/analysis/phs_ana_s1.d"\
  u 1:($2+180) t "^5He s_{1/2}" w l lw 6.0 ,\
  "../helium5/phs/allphs/allphs_p1_100MeV.d"\
  u 1:($4+180) t "^5He p_{1/2} conti" w l lw 6.0 ,\
  "../helium5/phs/allphs/allphs_p3_100MeV.d"\
  u 1:($4+180) t "^5He p_{3/2} conti" w l lw 6.0
