iterator=iterator+1
offset=iterator/(iterations+1.0) #1.0 нужно, чтобы все выражение вычислялось как float
outfile = sprintf('animation/%03i.png', iterator)
if (offset <= 1.0)\
   set output outfile ;\
   plot "./anim.txt" index iterator using 2:3 smooth csplines ;\
   reread 
