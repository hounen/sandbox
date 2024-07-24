#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [-0.5:1.5]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set x2tics ("P_{3/2}" 1.60,"P_{1/2}" 3.00)
set arrow 1 from 1.60,-0.5 rto 1.60,1.5 nohead lt 3 lw 3
set arrow 2 from 3.00,-0.5 rto 3.00,1.5 nohead lt 2 lw 3
set terminal postscript eps enhanced color 30 solid
set output "cld_sp.eps"
p "data/cld_s1.d" u 1:2 t "s_{1/2}" w l lw 6.0 ,\
  "data/cld_p1.d" u 1:2 t "p_{1/2}" w l lw 6.0 ,\
  "data/cld_p3.d" u 1:2 t "p_{3/2}" w l lw 6.0

