#!/usr/bin/gnuplot
reset
set term png 
set pm3d
set hidden3d
set surface
set output "surface.png"
splot "surface.txt"
