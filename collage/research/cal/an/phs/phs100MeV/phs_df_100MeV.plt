#!/bin/gnuplot -persist
set yrange [-5:100]
set xtics 0,20,100
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set x2tics ("  f_{5/2} f_{7/2} d_{5/2} d_{3/2}(E_{r})" 29.31415,"" 28.49982,"" 17.84957,"" 25.42382)
set x2tics font "Helvetica,20"
set arrow 1 from 17.84957,-5 rto 17.84957,100 nohead lt 3 lw 4 
set arrow 2 from 25.42382,-5 rto 25.42382,100 nohead lt 4 lw 4
set arrow 3 from 28.49982,-5 rto 28.49982,100 nohead lt 2 lw 4
set arrow 4 from 29.31415,-5 rto 29.31415,100 nohead lt 1 lw 4
set terminal postscript eps enhanced color 30 solid
set output "phs_df_100MeV.eps"
p "phs_d3_100MeV.d" t "d_{3/2}" w l lw 6,\
  "phs_d5_100MeV.d" t "d_{5/2}" w l lw 6,\
  "phs_f5_100MeV.d" t "f_{5/2}" w l lw 6,\
  "phs_f7_100MeV.d" t "f_{7/2}" w l lw 6,\
  90 t "" w l -1

