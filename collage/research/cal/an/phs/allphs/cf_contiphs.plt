#!/usr/bin/gnuplot -persist
set xrange [0:100]
set yrange [-180:0]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol d}(E)[deg.]"
set terminal postscript eps enhanced color 30 solid
set output "cf_contiphs.eps"
p  "allphs_p1_100MeV.d" u 1:4 t "p_{1/2} continuum" w l lw 6.0 ,\
   "allphs_p3_100MeV.d" u 1:4 t "p_{3/2} continuum" w l lw 6.0
#  "allphs_d3_100MeV.d" u 1:($4+180) t "d_{3/2}" w l lw 6.0 ,\
#  "allphs_d5_100MeV.d" u 1:($4+180) t "d_{5/2}" w l lw 6.0 ,\
#  "allphs_f5_100MeV.d" u 1:($4+180) t "f_{5/2}" w l lw 6.0 ,\
#  "allphs_f7_100MeV.d" u 1:($4+180) t "f_{7/2}" w l lw 6.0 
#  "phs_s1_100MeV.d" u 1:($2+180) t "s_{1/2}scattering" w l lw 6.0 ,\
