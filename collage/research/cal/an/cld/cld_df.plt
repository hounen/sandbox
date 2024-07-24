#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [-0.01:0.01]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript eps enhanced color 30 solid
set output "cld_df.eps"
p "cld_d3.d" u 1:2 t "d_{3/2}" w l lw 6.0 ,\
  "cld_d5.d" u 1:2 t "d_{5/2}" w l lw 6.0 ,\
  "cld_f5.d" u 1:2 t "f_{5/2}" w l lw 6.0 ,\
  "cld_f7.d" u 1:2 t "f_{7/2}" w l lw 6.0
