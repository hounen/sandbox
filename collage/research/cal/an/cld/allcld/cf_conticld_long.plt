#!/usr/bin/gnuplot -persist
set xrange [0:100]
set yrange [-0.06:0.01]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript eps enhanced color 30 solid
set output "cf_conticld_long.eps"
p "cld_s1_100MeV.d" t "s_{1/2}scattering" w l lw 6.0 ,\
  "allcld_p1_100MeV.d" u 1:4 t "p_{1/2}" w l lw 6.0 ,\
  "allcld_p3_100MeV.d" u 1:4 t "p_{3/2}" w l lw 6.0 ,\
  "allcld_d3_100MeV.d" u 1:4 t "d_{3/2}" w l lw 6.0 ,\
  "allcld_d5_100MeV.d" u 1:4 t "d_{5/2}" w l lw 6.0 ,\
  "allcld_f5_100MeV.d" u 1:4 t "f_{5/2}" w l lw 6.0 ,\
  "allcld_f7_100MeV.d" u 1:4 t "f_{7/2}" w l lw 6.0 
