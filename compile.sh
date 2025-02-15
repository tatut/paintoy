#!/bin/sh

# compile "somefile.pt" to "somefile.ptc" (compiled)

swipl -q -l paintoy.pl -t "compile_file('$1','$1c')"
