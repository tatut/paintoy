#!/bin/sh

swipl -l paintoy.pl -t "compile_file('$1.pt','$1.ptc')"
