#!/usr/bin/bash
./build.rkt

for file in scss/*; do
    sass $file "assets/$(basename -s ".scss" $file).css"
done
