#!/usr/bin/gnuplot -persist
set xrange[0:10] 
set yrange[-50:180]
set xtics 0,2,10
set ytics -180,30,180
set key 8,40
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set label "^4He-n p_{1/2} E_{r}=2.08MeV {/Symbol G}=5.67MeV" at 5,165 center font "Times,25"
set terminal postscript eps enhanced color 25 solid
set output "phs_deco_5he_p1.eps"
p "allphs_p1_100MeV.d" u 1:2 t "total" w l lt -1 lw 3.0,\
  "allphs_p1_100MeV.d" u 1:3 t "resonance" w l lt 1 lw 6.0,\
  "allphs_p1_100MeV.d" u 1:4 t "continuum" w l lt 3 lw 6.0,\
  90 t "" w l lt 0 lw 3,\
  0  t "" w l lt -1 lw 1
