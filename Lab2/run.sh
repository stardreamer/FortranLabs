#!/bin/bash

rm -f results/animation/*.png
rm -f results/*.avi
rm -f results/*.txt

cd bin/
./fortranlab

cd ../results
./animator.gp
mencoder 'mf://animation/*.png' -mf type=png:fps=20 -ovc lavc -lavcopts vcodec=wmv2 -oac copy -o  'result.avi'
mplayer 'result.avi'
cd ../
rm -f render/animation/*.png
