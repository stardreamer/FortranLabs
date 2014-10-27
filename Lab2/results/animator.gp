#!/usr/bin/gnuplot
reset
set xrange [0:1] #границы графика по X
set yrange [0:1.5] #границы графика по Y
set terminal png font "Droid, 14" size 750,750
set key off
set size square
stats "./anim.txt" nooutput
iterations=STATS_blocks #задаем число кадров
iterator=0
offset=0.0
load "animate.gp"