#!/bin/bash
let fname=aoeu
mkdir -p pics
make &&
./anderson "pics/$fname" $1 $2 &&
make pics &&
sxiv pics/$fname-?.png pics/$fname-??.png pics/$fname-???.png
