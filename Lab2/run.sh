#!/bin/bash

cd bin/
./fortranlab
cp 'anim.txt' ../render/
cd ../render
./animator.gp
mencoder 'mf://animation/*.png' -mf type=png:fps=20 -ovc lavc -lavcopts vcodec=wmv2 -oac copy -o  'result.avi'
mplayer 'result.avi'
cd ../
